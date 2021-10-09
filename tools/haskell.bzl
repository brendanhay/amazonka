load("@rules_haskell//haskell:cabal.bzl", "stack_snapshot")

def _snapshot_repo_name(snapshot):
    return "stackage-{}".format(snapshot)

def _stack_snapshot_alias_impl(repository_ctx):
    content = ['package(default_visibility = ["//visibility:public"])']

    for pkg in repository_ctx.attr.packages:
        conditions = []

        for ghc, snapshot in repository_ctx.attr.snapshots.items():
            conditions.append('"@{ghc}": "@{snapshot}//:{pkg}"'.format(
                pkg = pkg,
                ghc = ghc,
                snapshot = _snapshot_repo_name(snapshot),
            ))

        content.append("""
alias(
    name = "{pkg}",
    actual = select({{
        {conditions}
    }})
)""".format(
            pkg = pkg,
            conditions = ",\n        ".join(conditions),
        ))

    repository_ctx.file(
        "BUILD.bazel",
        executable = False,
        content = "\n".join(content),
    )

stack_snapshot_alias = repository_rule(
    implementation = _stack_snapshot_alias_impl,
    attrs = {
        "snapshots": attr.label_keyed_string_dict(),
        "packages": attr.string_list(),
    },
)

def stack_snapshots(name, snapshots, packages, **kwargs):
    for ghc, snapshot in snapshots.items():
        repo_name = _snapshot_repo_name(snapshot)

        stack_snapshot(
            name = repo_name,
            snapshot = snapshot,
            packages = packages,
            stack_snapshot_json = "//:{}-snapshot.json".format(repo_name),
            **kwargs
        )

    stack_snapshot_alias(
        name = name,
        snapshots = snapshots,
        packages = packages,
    )
