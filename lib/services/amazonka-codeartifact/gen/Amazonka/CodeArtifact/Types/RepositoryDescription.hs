{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.CodeArtifact.Types.RepositoryDescription
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeArtifact.Types.RepositoryDescription where

import Amazonka.CodeArtifact.Types.RepositoryExternalConnectionInfo
import Amazonka.CodeArtifact.Types.UpstreamRepositoryInfo
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The details of a repository stored in CodeArtifact. A CodeArtifact
-- repository contains a set of package versions, each of which maps to a
-- set of assets. Repositories are polyglot—a single repository can contain
-- packages of any supported type. Each repository exposes endpoints for
-- fetching and publishing packages using tools like the @npm@ CLI, the
-- Maven CLI (@mvn@), and @pip@. You can create up to 100 repositories per
-- Amazon Web Services account.
--
-- /See:/ 'newRepositoryDescription' smart constructor.
data RepositoryDescription = RepositoryDescription'
  { -- | The 12-digit account number of the Amazon Web Services account that
    -- manages the repository.
    administratorAccount :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the repository.
    arn :: Prelude.Maybe Prelude.Text,
    -- | A text description of the repository.
    description :: Prelude.Maybe Prelude.Text,
    -- | The name of the domain that contains the repository.
    domainName :: Prelude.Maybe Prelude.Text,
    -- | The 12-digit account number of the Amazon Web Services account that owns
    -- the domain that contains the repository. It does not include dashes or
    -- spaces.
    domainOwner :: Prelude.Maybe Prelude.Text,
    -- | An array of external connections associated with the repository.
    externalConnections :: Prelude.Maybe [RepositoryExternalConnectionInfo],
    -- | The name of the repository.
    name :: Prelude.Maybe Prelude.Text,
    -- | A list of upstream repositories to associate with the repository. The
    -- order of the upstream repositories in the list determines their priority
    -- order when CodeArtifact looks for a requested package version. For more
    -- information, see
    -- <https://docs.aws.amazon.com/codeartifact/latest/ug/repos-upstream.html Working with upstream repositories>.
    upstreams :: Prelude.Maybe [UpstreamRepositoryInfo]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RepositoryDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'administratorAccount', 'repositoryDescription_administratorAccount' - The 12-digit account number of the Amazon Web Services account that
-- manages the repository.
--
-- 'arn', 'repositoryDescription_arn' - The Amazon Resource Name (ARN) of the repository.
--
-- 'description', 'repositoryDescription_description' - A text description of the repository.
--
-- 'domainName', 'repositoryDescription_domainName' - The name of the domain that contains the repository.
--
-- 'domainOwner', 'repositoryDescription_domainOwner' - The 12-digit account number of the Amazon Web Services account that owns
-- the domain that contains the repository. It does not include dashes or
-- spaces.
--
-- 'externalConnections', 'repositoryDescription_externalConnections' - An array of external connections associated with the repository.
--
-- 'name', 'repositoryDescription_name' - The name of the repository.
--
-- 'upstreams', 'repositoryDescription_upstreams' - A list of upstream repositories to associate with the repository. The
-- order of the upstream repositories in the list determines their priority
-- order when CodeArtifact looks for a requested package version. For more
-- information, see
-- <https://docs.aws.amazon.com/codeartifact/latest/ug/repos-upstream.html Working with upstream repositories>.
newRepositoryDescription ::
  RepositoryDescription
newRepositoryDescription =
  RepositoryDescription'
    { administratorAccount =
        Prelude.Nothing,
      arn = Prelude.Nothing,
      description = Prelude.Nothing,
      domainName = Prelude.Nothing,
      domainOwner = Prelude.Nothing,
      externalConnections = Prelude.Nothing,
      name = Prelude.Nothing,
      upstreams = Prelude.Nothing
    }

-- | The 12-digit account number of the Amazon Web Services account that
-- manages the repository.
repositoryDescription_administratorAccount :: Lens.Lens' RepositoryDescription (Prelude.Maybe Prelude.Text)
repositoryDescription_administratorAccount = Lens.lens (\RepositoryDescription' {administratorAccount} -> administratorAccount) (\s@RepositoryDescription' {} a -> s {administratorAccount = a} :: RepositoryDescription)

-- | The Amazon Resource Name (ARN) of the repository.
repositoryDescription_arn :: Lens.Lens' RepositoryDescription (Prelude.Maybe Prelude.Text)
repositoryDescription_arn = Lens.lens (\RepositoryDescription' {arn} -> arn) (\s@RepositoryDescription' {} a -> s {arn = a} :: RepositoryDescription)

-- | A text description of the repository.
repositoryDescription_description :: Lens.Lens' RepositoryDescription (Prelude.Maybe Prelude.Text)
repositoryDescription_description = Lens.lens (\RepositoryDescription' {description} -> description) (\s@RepositoryDescription' {} a -> s {description = a} :: RepositoryDescription)

-- | The name of the domain that contains the repository.
repositoryDescription_domainName :: Lens.Lens' RepositoryDescription (Prelude.Maybe Prelude.Text)
repositoryDescription_domainName = Lens.lens (\RepositoryDescription' {domainName} -> domainName) (\s@RepositoryDescription' {} a -> s {domainName = a} :: RepositoryDescription)

-- | The 12-digit account number of the Amazon Web Services account that owns
-- the domain that contains the repository. It does not include dashes or
-- spaces.
repositoryDescription_domainOwner :: Lens.Lens' RepositoryDescription (Prelude.Maybe Prelude.Text)
repositoryDescription_domainOwner = Lens.lens (\RepositoryDescription' {domainOwner} -> domainOwner) (\s@RepositoryDescription' {} a -> s {domainOwner = a} :: RepositoryDescription)

-- | An array of external connections associated with the repository.
repositoryDescription_externalConnections :: Lens.Lens' RepositoryDescription (Prelude.Maybe [RepositoryExternalConnectionInfo])
repositoryDescription_externalConnections = Lens.lens (\RepositoryDescription' {externalConnections} -> externalConnections) (\s@RepositoryDescription' {} a -> s {externalConnections = a} :: RepositoryDescription) Prelude.. Lens.mapping Lens.coerced

-- | The name of the repository.
repositoryDescription_name :: Lens.Lens' RepositoryDescription (Prelude.Maybe Prelude.Text)
repositoryDescription_name = Lens.lens (\RepositoryDescription' {name} -> name) (\s@RepositoryDescription' {} a -> s {name = a} :: RepositoryDescription)

-- | A list of upstream repositories to associate with the repository. The
-- order of the upstream repositories in the list determines their priority
-- order when CodeArtifact looks for a requested package version. For more
-- information, see
-- <https://docs.aws.amazon.com/codeartifact/latest/ug/repos-upstream.html Working with upstream repositories>.
repositoryDescription_upstreams :: Lens.Lens' RepositoryDescription (Prelude.Maybe [UpstreamRepositoryInfo])
repositoryDescription_upstreams = Lens.lens (\RepositoryDescription' {upstreams} -> upstreams) (\s@RepositoryDescription' {} a -> s {upstreams = a} :: RepositoryDescription) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON RepositoryDescription where
  parseJSON =
    Data.withObject
      "RepositoryDescription"
      ( \x ->
          RepositoryDescription'
            Prelude.<$> (x Data..:? "administratorAccount")
            Prelude.<*> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "description")
            Prelude.<*> (x Data..:? "domainName")
            Prelude.<*> (x Data..:? "domainOwner")
            Prelude.<*> ( x
                            Data..:? "externalConnections"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "upstreams" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable RepositoryDescription where
  hashWithSalt _salt RepositoryDescription' {..} =
    _salt
      `Prelude.hashWithSalt` administratorAccount
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` domainName
      `Prelude.hashWithSalt` domainOwner
      `Prelude.hashWithSalt` externalConnections
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` upstreams

instance Prelude.NFData RepositoryDescription where
  rnf RepositoryDescription' {..} =
    Prelude.rnf administratorAccount
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf domainName
      `Prelude.seq` Prelude.rnf domainOwner
      `Prelude.seq` Prelude.rnf externalConnections
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf upstreams
