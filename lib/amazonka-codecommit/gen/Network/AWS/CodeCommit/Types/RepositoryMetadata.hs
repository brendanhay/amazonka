{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.RepositoryMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.RepositoryMetadata
  ( RepositoryMetadata (..),

    -- * Smart constructor
    mkRepositoryMetadata,

    -- * Lenses
    rmArn,
    rmAccountId,
    rmCloneUrlHttp,
    rmCloneUrlSsh,
    rmCreationDate,
    rmDefaultBranch,
    rmLastModifiedDate,
    rmRepositoryDescription,
    rmRepositoryId,
    rmRepositoryName,
  )
where

import qualified Network.AWS.CodeCommit.Types.AccountId as Types
import qualified Network.AWS.CodeCommit.Types.Arn as Types
import qualified Network.AWS.CodeCommit.Types.BranchName as Types
import qualified Network.AWS.CodeCommit.Types.CloneUrlHttp as Types
import qualified Network.AWS.CodeCommit.Types.CloneUrlSsh as Types
import qualified Network.AWS.CodeCommit.Types.RepositoryDescription as Types
import qualified Network.AWS.CodeCommit.Types.RepositoryId as Types
import qualified Network.AWS.CodeCommit.Types.RepositoryName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about a repository.
--
-- /See:/ 'mkRepositoryMetadata' smart constructor.
data RepositoryMetadata = RepositoryMetadata'
  { -- | The Amazon Resource Name (ARN) of the repository.
    arn :: Core.Maybe Types.Arn,
    -- | The ID of the AWS account associated with the repository.
    accountId :: Core.Maybe Types.AccountId,
    -- | The URL to use for cloning the repository over HTTPS.
    cloneUrlHttp :: Core.Maybe Types.CloneUrlHttp,
    -- | The URL to use for cloning the repository over SSH.
    cloneUrlSsh :: Core.Maybe Types.CloneUrlSsh,
    -- | The date and time the repository was created, in timestamp format.
    creationDate :: Core.Maybe Core.NominalDiffTime,
    -- | The repository's default branch name.
    defaultBranch :: Core.Maybe Types.BranchName,
    -- | The date and time the repository was last modified, in timestamp format.
    lastModifiedDate :: Core.Maybe Core.NominalDiffTime,
    -- | A comment or description about the repository.
    repositoryDescription :: Core.Maybe Types.RepositoryDescription,
    -- | The ID of the repository.
    repositoryId :: Core.Maybe Types.RepositoryId,
    -- | The repository's name.
    repositoryName :: Core.Maybe Types.RepositoryName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'RepositoryMetadata' value with any optional fields omitted.
mkRepositoryMetadata ::
  RepositoryMetadata
mkRepositoryMetadata =
  RepositoryMetadata'
    { arn = Core.Nothing,
      accountId = Core.Nothing,
      cloneUrlHttp = Core.Nothing,
      cloneUrlSsh = Core.Nothing,
      creationDate = Core.Nothing,
      defaultBranch = Core.Nothing,
      lastModifiedDate = Core.Nothing,
      repositoryDescription = Core.Nothing,
      repositoryId = Core.Nothing,
      repositoryName = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the repository.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rmArn :: Lens.Lens' RepositoryMetadata (Core.Maybe Types.Arn)
rmArn = Lens.field @"arn"
{-# DEPRECATED rmArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The ID of the AWS account associated with the repository.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rmAccountId :: Lens.Lens' RepositoryMetadata (Core.Maybe Types.AccountId)
rmAccountId = Lens.field @"accountId"
{-# DEPRECATED rmAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

-- | The URL to use for cloning the repository over HTTPS.
--
-- /Note:/ Consider using 'cloneUrlHttp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rmCloneUrlHttp :: Lens.Lens' RepositoryMetadata (Core.Maybe Types.CloneUrlHttp)
rmCloneUrlHttp = Lens.field @"cloneUrlHttp"
{-# DEPRECATED rmCloneUrlHttp "Use generic-lens or generic-optics with 'cloneUrlHttp' instead." #-}

-- | The URL to use for cloning the repository over SSH.
--
-- /Note:/ Consider using 'cloneUrlSsh' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rmCloneUrlSsh :: Lens.Lens' RepositoryMetadata (Core.Maybe Types.CloneUrlSsh)
rmCloneUrlSsh = Lens.field @"cloneUrlSsh"
{-# DEPRECATED rmCloneUrlSsh "Use generic-lens or generic-optics with 'cloneUrlSsh' instead." #-}

-- | The date and time the repository was created, in timestamp format.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rmCreationDate :: Lens.Lens' RepositoryMetadata (Core.Maybe Core.NominalDiffTime)
rmCreationDate = Lens.field @"creationDate"
{-# DEPRECATED rmCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

-- | The repository's default branch name.
--
-- /Note:/ Consider using 'defaultBranch' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rmDefaultBranch :: Lens.Lens' RepositoryMetadata (Core.Maybe Types.BranchName)
rmDefaultBranch = Lens.field @"defaultBranch"
{-# DEPRECATED rmDefaultBranch "Use generic-lens or generic-optics with 'defaultBranch' instead." #-}

-- | The date and time the repository was last modified, in timestamp format.
--
-- /Note:/ Consider using 'lastModifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rmLastModifiedDate :: Lens.Lens' RepositoryMetadata (Core.Maybe Core.NominalDiffTime)
rmLastModifiedDate = Lens.field @"lastModifiedDate"
{-# DEPRECATED rmLastModifiedDate "Use generic-lens or generic-optics with 'lastModifiedDate' instead." #-}

-- | A comment or description about the repository.
--
-- /Note:/ Consider using 'repositoryDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rmRepositoryDescription :: Lens.Lens' RepositoryMetadata (Core.Maybe Types.RepositoryDescription)
rmRepositoryDescription = Lens.field @"repositoryDescription"
{-# DEPRECATED rmRepositoryDescription "Use generic-lens or generic-optics with 'repositoryDescription' instead." #-}

-- | The ID of the repository.
--
-- /Note:/ Consider using 'repositoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rmRepositoryId :: Lens.Lens' RepositoryMetadata (Core.Maybe Types.RepositoryId)
rmRepositoryId = Lens.field @"repositoryId"
{-# DEPRECATED rmRepositoryId "Use generic-lens or generic-optics with 'repositoryId' instead." #-}

-- | The repository's name.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rmRepositoryName :: Lens.Lens' RepositoryMetadata (Core.Maybe Types.RepositoryName)
rmRepositoryName = Lens.field @"repositoryName"
{-# DEPRECATED rmRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

instance Core.FromJSON RepositoryMetadata where
  parseJSON =
    Core.withObject "RepositoryMetadata" Core.$
      \x ->
        RepositoryMetadata'
          Core.<$> (x Core..:? "Arn")
          Core.<*> (x Core..:? "accountId")
          Core.<*> (x Core..:? "cloneUrlHttp")
          Core.<*> (x Core..:? "cloneUrlSsh")
          Core.<*> (x Core..:? "creationDate")
          Core.<*> (x Core..:? "defaultBranch")
          Core.<*> (x Core..:? "lastModifiedDate")
          Core.<*> (x Core..:? "repositoryDescription")
          Core.<*> (x Core..:? "repositoryId")
          Core.<*> (x Core..:? "repositoryName")
