{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.GitConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.GitConfig
  ( GitConfig (..)
  -- * Smart constructor
  , mkGitConfig
  -- * Lenses
  , gcRepositoryUrl
  , gcBranch
  , gcSecretArn
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.Branch as Types
import qualified Network.AWS.SageMaker.Types.RepositoryUrl as Types
import qualified Network.AWS.SageMaker.Types.SecretArn as Types

-- | Specifies configuration details for a Git repository in your AWS account.
--
-- /See:/ 'mkGitConfig' smart constructor.
data GitConfig = GitConfig'
  { repositoryUrl :: Types.RepositoryUrl
    -- ^ The URL where the Git repository is located.
  , branch :: Core.Maybe Types.Branch
    -- ^ The default branch for the Git repository.
  , secretArn :: Core.Maybe Types.SecretArn
    -- ^ The Amazon Resource Name (ARN) of the AWS Secrets Manager secret that contains the credentials used to access the git repository. The secret must have a staging label of @AWSCURRENT@ and must be in the following format:
--
-- @{"username": /UserName/ , "password": /Password/ }@ 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GitConfig' value with any optional fields omitted.
mkGitConfig
    :: Types.RepositoryUrl -- ^ 'repositoryUrl'
    -> GitConfig
mkGitConfig repositoryUrl
  = GitConfig'{repositoryUrl, branch = Core.Nothing,
               secretArn = Core.Nothing}

-- | The URL where the Git repository is located.
--
-- /Note:/ Consider using 'repositoryUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcRepositoryUrl :: Lens.Lens' GitConfig Types.RepositoryUrl
gcRepositoryUrl = Lens.field @"repositoryUrl"
{-# INLINEABLE gcRepositoryUrl #-}
{-# DEPRECATED repositoryUrl "Use generic-lens or generic-optics with 'repositoryUrl' instead"  #-}

-- | The default branch for the Git repository.
--
-- /Note:/ Consider using 'branch' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcBranch :: Lens.Lens' GitConfig (Core.Maybe Types.Branch)
gcBranch = Lens.field @"branch"
{-# INLINEABLE gcBranch #-}
{-# DEPRECATED branch "Use generic-lens or generic-optics with 'branch' instead"  #-}

-- | The Amazon Resource Name (ARN) of the AWS Secrets Manager secret that contains the credentials used to access the git repository. The secret must have a staging label of @AWSCURRENT@ and must be in the following format:
--
-- @{"username": /UserName/ , "password": /Password/ }@ 
--
-- /Note:/ Consider using 'secretArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcSecretArn :: Lens.Lens' GitConfig (Core.Maybe Types.SecretArn)
gcSecretArn = Lens.field @"secretArn"
{-# INLINEABLE gcSecretArn #-}
{-# DEPRECATED secretArn "Use generic-lens or generic-optics with 'secretArn' instead"  #-}

instance Core.FromJSON GitConfig where
        toJSON GitConfig{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("RepositoryUrl" Core..= repositoryUrl),
                  ("Branch" Core..=) Core.<$> branch,
                  ("SecretArn" Core..=) Core.<$> secretArn])

instance Core.FromJSON GitConfig where
        parseJSON
          = Core.withObject "GitConfig" Core.$
              \ x ->
                GitConfig' Core.<$>
                  (x Core..: "RepositoryUrl") Core.<*> x Core..:? "Branch" Core.<*>
                    x Core..:? "SecretArn"
