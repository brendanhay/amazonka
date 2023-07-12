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
-- Module      : Amazonka.ECRPublic.Types.Repository
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECRPublic.Types.Repository where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object representing a repository.
--
-- /See:/ 'newRepository' smart constructor.
data Repository = Repository'
  { -- | The date and time, in JavaScript date format, when the repository was
    -- created.
    createdAt :: Prelude.Maybe Data.POSIX,
    -- | The AWS account ID associated with the public registry that contains the
    -- repository.
    registryId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) that identifies the repository. The ARN
    -- contains the @arn:aws:ecr@ namespace, followed by the region of the
    -- repository, AWS account ID of the repository owner, repository
    -- namespace, and repository name. For example,
    -- @arn:aws:ecr:region:012345678910:repository\/test@.
    repositoryArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the repository.
    repositoryName :: Prelude.Maybe Prelude.Text,
    -- | The URI for the repository. You can use this URI for container image
    -- @push@ and @pull@ operations.
    repositoryUri :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Repository' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdAt', 'repository_createdAt' - The date and time, in JavaScript date format, when the repository was
-- created.
--
-- 'registryId', 'repository_registryId' - The AWS account ID associated with the public registry that contains the
-- repository.
--
-- 'repositoryArn', 'repository_repositoryArn' - The Amazon Resource Name (ARN) that identifies the repository. The ARN
-- contains the @arn:aws:ecr@ namespace, followed by the region of the
-- repository, AWS account ID of the repository owner, repository
-- namespace, and repository name. For example,
-- @arn:aws:ecr:region:012345678910:repository\/test@.
--
-- 'repositoryName', 'repository_repositoryName' - The name of the repository.
--
-- 'repositoryUri', 'repository_repositoryUri' - The URI for the repository. You can use this URI for container image
-- @push@ and @pull@ operations.
newRepository ::
  Repository
newRepository =
  Repository'
    { createdAt = Prelude.Nothing,
      registryId = Prelude.Nothing,
      repositoryArn = Prelude.Nothing,
      repositoryName = Prelude.Nothing,
      repositoryUri = Prelude.Nothing
    }

-- | The date and time, in JavaScript date format, when the repository was
-- created.
repository_createdAt :: Lens.Lens' Repository (Prelude.Maybe Prelude.UTCTime)
repository_createdAt = Lens.lens (\Repository' {createdAt} -> createdAt) (\s@Repository' {} a -> s {createdAt = a} :: Repository) Prelude.. Lens.mapping Data._Time

-- | The AWS account ID associated with the public registry that contains the
-- repository.
repository_registryId :: Lens.Lens' Repository (Prelude.Maybe Prelude.Text)
repository_registryId = Lens.lens (\Repository' {registryId} -> registryId) (\s@Repository' {} a -> s {registryId = a} :: Repository)

-- | The Amazon Resource Name (ARN) that identifies the repository. The ARN
-- contains the @arn:aws:ecr@ namespace, followed by the region of the
-- repository, AWS account ID of the repository owner, repository
-- namespace, and repository name. For example,
-- @arn:aws:ecr:region:012345678910:repository\/test@.
repository_repositoryArn :: Lens.Lens' Repository (Prelude.Maybe Prelude.Text)
repository_repositoryArn = Lens.lens (\Repository' {repositoryArn} -> repositoryArn) (\s@Repository' {} a -> s {repositoryArn = a} :: Repository)

-- | The name of the repository.
repository_repositoryName :: Lens.Lens' Repository (Prelude.Maybe Prelude.Text)
repository_repositoryName = Lens.lens (\Repository' {repositoryName} -> repositoryName) (\s@Repository' {} a -> s {repositoryName = a} :: Repository)

-- | The URI for the repository. You can use this URI for container image
-- @push@ and @pull@ operations.
repository_repositoryUri :: Lens.Lens' Repository (Prelude.Maybe Prelude.Text)
repository_repositoryUri = Lens.lens (\Repository' {repositoryUri} -> repositoryUri) (\s@Repository' {} a -> s {repositoryUri = a} :: Repository)

instance Data.FromJSON Repository where
  parseJSON =
    Data.withObject
      "Repository"
      ( \x ->
          Repository'
            Prelude.<$> (x Data..:? "createdAt")
            Prelude.<*> (x Data..:? "registryId")
            Prelude.<*> (x Data..:? "repositoryArn")
            Prelude.<*> (x Data..:? "repositoryName")
            Prelude.<*> (x Data..:? "repositoryUri")
      )

instance Prelude.Hashable Repository where
  hashWithSalt _salt Repository' {..} =
    _salt
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` registryId
      `Prelude.hashWithSalt` repositoryArn
      `Prelude.hashWithSalt` repositoryName
      `Prelude.hashWithSalt` repositoryUri

instance Prelude.NFData Repository where
  rnf Repository' {..} =
    Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf registryId
      `Prelude.seq` Prelude.rnf repositoryArn
      `Prelude.seq` Prelude.rnf repositoryName
      `Prelude.seq` Prelude.rnf repositoryUri
