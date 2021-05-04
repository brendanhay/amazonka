{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about a repository.
--
-- /See:/ 'newRepositoryMetadata' smart constructor.
data RepositoryMetadata = RepositoryMetadata'
  { -- | The date and time the repository was last modified, in timestamp format.
    lastModifiedDate :: Prelude.Maybe Prelude.POSIX,
    -- | The repository\'s default branch name.
    defaultBranch :: Prelude.Maybe Prelude.Text,
    -- | The ID of the AWS account associated with the repository.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | The URL to use for cloning the repository over SSH.
    cloneUrlSsh :: Prelude.Maybe Prelude.Text,
    -- | The URL to use for cloning the repository over HTTPS.
    cloneUrlHttp :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the repository.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The date and time the repository was created, in timestamp format.
    creationDate :: Prelude.Maybe Prelude.POSIX,
    -- | The repository\'s name.
    repositoryName :: Prelude.Maybe Prelude.Text,
    -- | The ID of the repository.
    repositoryId :: Prelude.Maybe Prelude.Text,
    -- | A comment or description about the repository.
    repositoryDescription :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      defaultBranch = Prelude.Nothing,
      accountId = Prelude.Nothing,
      cloneUrlSsh = Prelude.Nothing,
      cloneUrlHttp = Prelude.Nothing,
      arn = Prelude.Nothing,
      creationDate = Prelude.Nothing,
      repositoryName = Prelude.Nothing,
      repositoryId = Prelude.Nothing,
      repositoryDescription = Prelude.Nothing
    }

-- | The date and time the repository was last modified, in timestamp format.
repositoryMetadata_lastModifiedDate :: Lens.Lens' RepositoryMetadata (Prelude.Maybe Prelude.UTCTime)
repositoryMetadata_lastModifiedDate = Lens.lens (\RepositoryMetadata' {lastModifiedDate} -> lastModifiedDate) (\s@RepositoryMetadata' {} a -> s {lastModifiedDate = a} :: RepositoryMetadata) Prelude.. Lens.mapping Prelude._Time

-- | The repository\'s default branch name.
repositoryMetadata_defaultBranch :: Lens.Lens' RepositoryMetadata (Prelude.Maybe Prelude.Text)
repositoryMetadata_defaultBranch = Lens.lens (\RepositoryMetadata' {defaultBranch} -> defaultBranch) (\s@RepositoryMetadata' {} a -> s {defaultBranch = a} :: RepositoryMetadata)

-- | The ID of the AWS account associated with the repository.
repositoryMetadata_accountId :: Lens.Lens' RepositoryMetadata (Prelude.Maybe Prelude.Text)
repositoryMetadata_accountId = Lens.lens (\RepositoryMetadata' {accountId} -> accountId) (\s@RepositoryMetadata' {} a -> s {accountId = a} :: RepositoryMetadata)

-- | The URL to use for cloning the repository over SSH.
repositoryMetadata_cloneUrlSsh :: Lens.Lens' RepositoryMetadata (Prelude.Maybe Prelude.Text)
repositoryMetadata_cloneUrlSsh = Lens.lens (\RepositoryMetadata' {cloneUrlSsh} -> cloneUrlSsh) (\s@RepositoryMetadata' {} a -> s {cloneUrlSsh = a} :: RepositoryMetadata)

-- | The URL to use for cloning the repository over HTTPS.
repositoryMetadata_cloneUrlHttp :: Lens.Lens' RepositoryMetadata (Prelude.Maybe Prelude.Text)
repositoryMetadata_cloneUrlHttp = Lens.lens (\RepositoryMetadata' {cloneUrlHttp} -> cloneUrlHttp) (\s@RepositoryMetadata' {} a -> s {cloneUrlHttp = a} :: RepositoryMetadata)

-- | The Amazon Resource Name (ARN) of the repository.
repositoryMetadata_arn :: Lens.Lens' RepositoryMetadata (Prelude.Maybe Prelude.Text)
repositoryMetadata_arn = Lens.lens (\RepositoryMetadata' {arn} -> arn) (\s@RepositoryMetadata' {} a -> s {arn = a} :: RepositoryMetadata)

-- | The date and time the repository was created, in timestamp format.
repositoryMetadata_creationDate :: Lens.Lens' RepositoryMetadata (Prelude.Maybe Prelude.UTCTime)
repositoryMetadata_creationDate = Lens.lens (\RepositoryMetadata' {creationDate} -> creationDate) (\s@RepositoryMetadata' {} a -> s {creationDate = a} :: RepositoryMetadata) Prelude.. Lens.mapping Prelude._Time

-- | The repository\'s name.
repositoryMetadata_repositoryName :: Lens.Lens' RepositoryMetadata (Prelude.Maybe Prelude.Text)
repositoryMetadata_repositoryName = Lens.lens (\RepositoryMetadata' {repositoryName} -> repositoryName) (\s@RepositoryMetadata' {} a -> s {repositoryName = a} :: RepositoryMetadata)

-- | The ID of the repository.
repositoryMetadata_repositoryId :: Lens.Lens' RepositoryMetadata (Prelude.Maybe Prelude.Text)
repositoryMetadata_repositoryId = Lens.lens (\RepositoryMetadata' {repositoryId} -> repositoryId) (\s@RepositoryMetadata' {} a -> s {repositoryId = a} :: RepositoryMetadata)

-- | A comment or description about the repository.
repositoryMetadata_repositoryDescription :: Lens.Lens' RepositoryMetadata (Prelude.Maybe Prelude.Text)
repositoryMetadata_repositoryDescription = Lens.lens (\RepositoryMetadata' {repositoryDescription} -> repositoryDescription) (\s@RepositoryMetadata' {} a -> s {repositoryDescription = a} :: RepositoryMetadata)

instance Prelude.FromJSON RepositoryMetadata where
  parseJSON =
    Prelude.withObject
      "RepositoryMetadata"
      ( \x ->
          RepositoryMetadata'
            Prelude.<$> (x Prelude..:? "lastModifiedDate")
            Prelude.<*> (x Prelude..:? "defaultBranch")
            Prelude.<*> (x Prelude..:? "accountId")
            Prelude.<*> (x Prelude..:? "cloneUrlSsh")
            Prelude.<*> (x Prelude..:? "cloneUrlHttp")
            Prelude.<*> (x Prelude..:? "Arn")
            Prelude.<*> (x Prelude..:? "creationDate")
            Prelude.<*> (x Prelude..:? "repositoryName")
            Prelude.<*> (x Prelude..:? "repositoryId")
            Prelude.<*> (x Prelude..:? "repositoryDescription")
      )

instance Prelude.Hashable RepositoryMetadata

instance Prelude.NFData RepositoryMetadata
