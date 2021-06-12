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
-- Module      : Network.AWS.CodeCommit.Types.RepositoryMetadata
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.RepositoryMetadata where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Information about a repository.
--
-- /See:/ 'newRepositoryMetadata' smart constructor.
data RepositoryMetadata = RepositoryMetadata'
  { -- | The date and time the repository was last modified, in timestamp format.
    lastModifiedDate :: Core.Maybe Core.POSIX,
    -- | The repository\'s default branch name.
    defaultBranch :: Core.Maybe Core.Text,
    -- | The ID of the AWS account associated with the repository.
    accountId :: Core.Maybe Core.Text,
    -- | The URL to use for cloning the repository over SSH.
    cloneUrlSsh :: Core.Maybe Core.Text,
    -- | The URL to use for cloning the repository over HTTPS.
    cloneUrlHttp :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) of the repository.
    arn :: Core.Maybe Core.Text,
    -- | The date and time the repository was created, in timestamp format.
    creationDate :: Core.Maybe Core.POSIX,
    -- | The repository\'s name.
    repositoryName :: Core.Maybe Core.Text,
    -- | The ID of the repository.
    repositoryId :: Core.Maybe Core.Text,
    -- | A comment or description about the repository.
    repositoryDescription :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RepositoryMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastModifiedDate', 'repositoryMetadata_lastModifiedDate' - The date and time the repository was last modified, in timestamp format.
--
-- 'defaultBranch', 'repositoryMetadata_defaultBranch' - The repository\'s default branch name.
--
-- 'accountId', 'repositoryMetadata_accountId' - The ID of the AWS account associated with the repository.
--
-- 'cloneUrlSsh', 'repositoryMetadata_cloneUrlSsh' - The URL to use for cloning the repository over SSH.
--
-- 'cloneUrlHttp', 'repositoryMetadata_cloneUrlHttp' - The URL to use for cloning the repository over HTTPS.
--
-- 'arn', 'repositoryMetadata_arn' - The Amazon Resource Name (ARN) of the repository.
--
-- 'creationDate', 'repositoryMetadata_creationDate' - The date and time the repository was created, in timestamp format.
--
-- 'repositoryName', 'repositoryMetadata_repositoryName' - The repository\'s name.
--
-- 'repositoryId', 'repositoryMetadata_repositoryId' - The ID of the repository.
--
-- 'repositoryDescription', 'repositoryMetadata_repositoryDescription' - A comment or description about the repository.
newRepositoryMetadata ::
  RepositoryMetadata
newRepositoryMetadata =
  RepositoryMetadata'
    { lastModifiedDate =
        Core.Nothing,
      defaultBranch = Core.Nothing,
      accountId = Core.Nothing,
      cloneUrlSsh = Core.Nothing,
      cloneUrlHttp = Core.Nothing,
      arn = Core.Nothing,
      creationDate = Core.Nothing,
      repositoryName = Core.Nothing,
      repositoryId = Core.Nothing,
      repositoryDescription = Core.Nothing
    }

-- | The date and time the repository was last modified, in timestamp format.
repositoryMetadata_lastModifiedDate :: Lens.Lens' RepositoryMetadata (Core.Maybe Core.UTCTime)
repositoryMetadata_lastModifiedDate = Lens.lens (\RepositoryMetadata' {lastModifiedDate} -> lastModifiedDate) (\s@RepositoryMetadata' {} a -> s {lastModifiedDate = a} :: RepositoryMetadata) Core.. Lens.mapping Core._Time

-- | The repository\'s default branch name.
repositoryMetadata_defaultBranch :: Lens.Lens' RepositoryMetadata (Core.Maybe Core.Text)
repositoryMetadata_defaultBranch = Lens.lens (\RepositoryMetadata' {defaultBranch} -> defaultBranch) (\s@RepositoryMetadata' {} a -> s {defaultBranch = a} :: RepositoryMetadata)

-- | The ID of the AWS account associated with the repository.
repositoryMetadata_accountId :: Lens.Lens' RepositoryMetadata (Core.Maybe Core.Text)
repositoryMetadata_accountId = Lens.lens (\RepositoryMetadata' {accountId} -> accountId) (\s@RepositoryMetadata' {} a -> s {accountId = a} :: RepositoryMetadata)

-- | The URL to use for cloning the repository over SSH.
repositoryMetadata_cloneUrlSsh :: Lens.Lens' RepositoryMetadata (Core.Maybe Core.Text)
repositoryMetadata_cloneUrlSsh = Lens.lens (\RepositoryMetadata' {cloneUrlSsh} -> cloneUrlSsh) (\s@RepositoryMetadata' {} a -> s {cloneUrlSsh = a} :: RepositoryMetadata)

-- | The URL to use for cloning the repository over HTTPS.
repositoryMetadata_cloneUrlHttp :: Lens.Lens' RepositoryMetadata (Core.Maybe Core.Text)
repositoryMetadata_cloneUrlHttp = Lens.lens (\RepositoryMetadata' {cloneUrlHttp} -> cloneUrlHttp) (\s@RepositoryMetadata' {} a -> s {cloneUrlHttp = a} :: RepositoryMetadata)

-- | The Amazon Resource Name (ARN) of the repository.
repositoryMetadata_arn :: Lens.Lens' RepositoryMetadata (Core.Maybe Core.Text)
repositoryMetadata_arn = Lens.lens (\RepositoryMetadata' {arn} -> arn) (\s@RepositoryMetadata' {} a -> s {arn = a} :: RepositoryMetadata)

-- | The date and time the repository was created, in timestamp format.
repositoryMetadata_creationDate :: Lens.Lens' RepositoryMetadata (Core.Maybe Core.UTCTime)
repositoryMetadata_creationDate = Lens.lens (\RepositoryMetadata' {creationDate} -> creationDate) (\s@RepositoryMetadata' {} a -> s {creationDate = a} :: RepositoryMetadata) Core.. Lens.mapping Core._Time

-- | The repository\'s name.
repositoryMetadata_repositoryName :: Lens.Lens' RepositoryMetadata (Core.Maybe Core.Text)
repositoryMetadata_repositoryName = Lens.lens (\RepositoryMetadata' {repositoryName} -> repositoryName) (\s@RepositoryMetadata' {} a -> s {repositoryName = a} :: RepositoryMetadata)

-- | The ID of the repository.
repositoryMetadata_repositoryId :: Lens.Lens' RepositoryMetadata (Core.Maybe Core.Text)
repositoryMetadata_repositoryId = Lens.lens (\RepositoryMetadata' {repositoryId} -> repositoryId) (\s@RepositoryMetadata' {} a -> s {repositoryId = a} :: RepositoryMetadata)

-- | A comment or description about the repository.
repositoryMetadata_repositoryDescription :: Lens.Lens' RepositoryMetadata (Core.Maybe Core.Text)
repositoryMetadata_repositoryDescription = Lens.lens (\RepositoryMetadata' {repositoryDescription} -> repositoryDescription) (\s@RepositoryMetadata' {} a -> s {repositoryDescription = a} :: RepositoryMetadata)

instance Core.FromJSON RepositoryMetadata where
  parseJSON =
    Core.withObject
      "RepositoryMetadata"
      ( \x ->
          RepositoryMetadata'
            Core.<$> (x Core..:? "lastModifiedDate")
            Core.<*> (x Core..:? "defaultBranch")
            Core.<*> (x Core..:? "accountId")
            Core.<*> (x Core..:? "cloneUrlSsh")
            Core.<*> (x Core..:? "cloneUrlHttp")
            Core.<*> (x Core..:? "Arn")
            Core.<*> (x Core..:? "creationDate")
            Core.<*> (x Core..:? "repositoryName")
            Core.<*> (x Core..:? "repositoryId")
            Core.<*> (x Core..:? "repositoryDescription")
      )

instance Core.Hashable RepositoryMetadata

instance Core.NFData RepositoryMetadata
