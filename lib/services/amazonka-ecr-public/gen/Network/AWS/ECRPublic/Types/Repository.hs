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
-- Module      : Network.AWS.ECRPublic.Types.Repository
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECRPublic.Types.Repository where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | An object representing a repository.
--
-- /See:/ 'newRepository' smart constructor.
data Repository = Repository'
  { -- | The Amazon Resource Name (ARN) that identifies the repository. The ARN
    -- contains the @arn:aws:ecr@ namespace, followed by the region of the
    -- repository, AWS account ID of the repository owner, repository
    -- namespace, and repository name. For example,
    -- @arn:aws:ecr:region:012345678910:repository\/test@.
    repositoryArn :: Prelude.Maybe Prelude.Text,
    -- | The date and time, in JavaScript date format, when the repository was
    -- created.
    createdAt :: Prelude.Maybe Core.POSIX,
    -- | The AWS account ID associated with the public registry that contains the
    -- repository.
    registryId :: Prelude.Maybe Prelude.Text,
    -- | The URI for the repository. You can use this URI for container image
    -- @push@ and @pull@ operations.
    repositoryUri :: Prelude.Maybe Prelude.Text,
    -- | The name of the repository.
    repositoryName :: Prelude.Maybe Prelude.Text
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
-- 'repositoryArn', 'repository_repositoryArn' - The Amazon Resource Name (ARN) that identifies the repository. The ARN
-- contains the @arn:aws:ecr@ namespace, followed by the region of the
-- repository, AWS account ID of the repository owner, repository
-- namespace, and repository name. For example,
-- @arn:aws:ecr:region:012345678910:repository\/test@.
--
-- 'createdAt', 'repository_createdAt' - The date and time, in JavaScript date format, when the repository was
-- created.
--
-- 'registryId', 'repository_registryId' - The AWS account ID associated with the public registry that contains the
-- repository.
--
-- 'repositoryUri', 'repository_repositoryUri' - The URI for the repository. You can use this URI for container image
-- @push@ and @pull@ operations.
--
-- 'repositoryName', 'repository_repositoryName' - The name of the repository.
newRepository ::
  Repository
newRepository =
  Repository'
    { repositoryArn = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      registryId = Prelude.Nothing,
      repositoryUri = Prelude.Nothing,
      repositoryName = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) that identifies the repository. The ARN
-- contains the @arn:aws:ecr@ namespace, followed by the region of the
-- repository, AWS account ID of the repository owner, repository
-- namespace, and repository name. For example,
-- @arn:aws:ecr:region:012345678910:repository\/test@.
repository_repositoryArn :: Lens.Lens' Repository (Prelude.Maybe Prelude.Text)
repository_repositoryArn = Lens.lens (\Repository' {repositoryArn} -> repositoryArn) (\s@Repository' {} a -> s {repositoryArn = a} :: Repository)

-- | The date and time, in JavaScript date format, when the repository was
-- created.
repository_createdAt :: Lens.Lens' Repository (Prelude.Maybe Prelude.UTCTime)
repository_createdAt = Lens.lens (\Repository' {createdAt} -> createdAt) (\s@Repository' {} a -> s {createdAt = a} :: Repository) Prelude.. Lens.mapping Core._Time

-- | The AWS account ID associated with the public registry that contains the
-- repository.
repository_registryId :: Lens.Lens' Repository (Prelude.Maybe Prelude.Text)
repository_registryId = Lens.lens (\Repository' {registryId} -> registryId) (\s@Repository' {} a -> s {registryId = a} :: Repository)

-- | The URI for the repository. You can use this URI for container image
-- @push@ and @pull@ operations.
repository_repositoryUri :: Lens.Lens' Repository (Prelude.Maybe Prelude.Text)
repository_repositoryUri = Lens.lens (\Repository' {repositoryUri} -> repositoryUri) (\s@Repository' {} a -> s {repositoryUri = a} :: Repository)

-- | The name of the repository.
repository_repositoryName :: Lens.Lens' Repository (Prelude.Maybe Prelude.Text)
repository_repositoryName = Lens.lens (\Repository' {repositoryName} -> repositoryName) (\s@Repository' {} a -> s {repositoryName = a} :: Repository)

instance Core.FromJSON Repository where
  parseJSON =
    Core.withObject
      "Repository"
      ( \x ->
          Repository'
            Prelude.<$> (x Core..:? "repositoryArn")
            Prelude.<*> (x Core..:? "createdAt")
            Prelude.<*> (x Core..:? "registryId")
            Prelude.<*> (x Core..:? "repositoryUri")
            Prelude.<*> (x Core..:? "repositoryName")
      )

instance Prelude.Hashable Repository

instance Prelude.NFData Repository
