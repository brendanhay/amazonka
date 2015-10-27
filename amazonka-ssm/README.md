# Amazon Simple Systems Management Service SDK

* [Version](#version)
* [Description](#description)
* [Contribute](#contribute)
* [Licence](#licence)


## Version

`1.3.4`


## Description

Simple Systems Manager (SSM) is a set of capabilities that can help you
manage your Amazon EC2 instances running on Windows. SSM enables you to
run scripts or other common administrative tasks on your instances using
either SSM Run Command or SSM Config.

Run Command extends the server administration capabilities of SSM by
offering an on-demand experience for executing commands. You can use
pre-defined Amazon SSM documents (formerly called configuration
documents) to perform the actions listed later in this section, or you
can create your own documents. With these document, you can then
remotely configure your instances by sending commands using the AWS
command line interface (CLI), AWS Tools for Windows PowerShell, or the
__Commands__ page in the Amazon EC2 console. Additionally, because Run
Command enables you to execute PowerShell commands or scripts, you can
administer your instances remotely using PowerShell as though you were
logged on locally to the instance. Run Command reports the status of the
command execution for each instance targeted by a command. You can also
audit the command execution to understand who executed commands, when,
and what changes were made. By switching between different SSM
documents, you can quickly configure your instances with different types
of commands.

SSM Config is a lightweight instance configuration solution. With SSM
Config, you can specify a setup configuration for your instances. SSM
Config is similar to EC2 User Data, which is another way of running
one-time scripts or applying settings during instance launch. SSM Config
is an extension of this capability. Using SSM documents, you can specify
which actions the system should perform on your instances, including
which applications to install, which AWS Directory Service directory to
join, which Microsoft PowerShell modules to install, etc. If an instance
is missing one or more of these configurations, the system makes those
changes. By default, the system checks every five minutes to see if
there is a new configuration to apply as defined in a new SSM document.
If so, the system updates the instances accordingly. In this way, you
can remotely maintain a consistent configuration baseline on your
instances. SSM Config is available using the AWS CLI or the AWS Tools
for Windows PowerShell.

SSM is currently not supported on Linux instances.

You can use Run Command and SSM Config to do the following:

-   Join an AWS Directory Service directory (SSM Config and Run Command)

-   Install, repair, or uninstall software using an MSI package (SSM
    Config and Run Command)

-   Install PowerShell modules (SSM Config and Run Command)

-   Configure CloudWatch Logs to monitor applications and systems (SSM
    Config and Run Command)

-   Run PowerShell commands or scripts (Run Command only)

-   Update the EC2Config service (Run Command only)

-   Configure Windows Update settings (Run Command only)

SSM documents run with administrative privilege on Windows instances
because the EC2Config service runs in the Local System account. If a
user has permission to execute any of the pre-defined SSM documents (any
document that begins with AWS-*) then that user also has administrator
access to the instance. Delegate access to SSM Config and Run Command
judiciously. This becomes extremely important if you create your own SSM
documents. Amazon Web Services does not provide guidance about how to
create secure SSM documents. You create SSM documents and delegate
access to Run Command actions at your own risk. As a security best
practice, we recommend that you assign access to \"AWS-*\" documents,
especially the AWS-RunPowerShellScript document, to trusted
administrators only. You can create low-level SSM documents for low
security tasks and delegate access to non-administrators.

Documentation is available via [Hackage](http://hackage.haskell.org/package/amazonka-ssm)
and the [AWS API Reference](http://docs.aws.amazon.com/ssm/latest/APIReference/Welcome.html).

The types from this library are intended to be used with [amazonka](http://hackage.haskell.org/package/amazonka),
which provides mechanisms for specifying AuthN/AuthZ information and sending requests.

Use of lenses is required for constructing and manipulating types.
This is due to the amount of nesting of AWS types and transparency regarding
de/serialisation into more palatable Haskell values.
The provided lenses should be compatible with any of the major lens libraries
[lens](http://hackage.haskell.org/package/lens) or [lens-family-core](http://hackage.haskell.org/package/lens-family-core).

## Contribute

For any problems, comments, or feedback please create an issue [here on GitHub](https://github.com/brendanhay/amazonka/issues).

> _Note:_ this library is an auto-generated Haskell package. Please see `amazonka-gen` for more information.


## Licence

`amazonka-ssm` is released under the [Mozilla Public License Version 2.0](http://www.mozilla.org/MPL/).

Parts of the code are derived from AWS service descriptions, licensed under Apache 2.0.
Source files subject to this contain an additional licensing clause in their header.
