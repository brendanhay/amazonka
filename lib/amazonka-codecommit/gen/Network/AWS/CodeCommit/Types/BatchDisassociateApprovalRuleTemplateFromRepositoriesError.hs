{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.BatchDisassociateApprovalRuleTemplateFromRepositoriesError
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.BatchDisassociateApprovalRuleTemplateFromRepositoriesError
  ( BatchDisassociateApprovalRuleTemplateFromRepositoriesError (..),

    -- * Smart constructor
    mkBatchDisassociateApprovalRuleTemplateFromRepositoriesError,

    -- * Lenses
    bdartfreErrorCode,
    bdartfreErrorMessage,
    bdartfreRepositoryName,
  )
where

import qualified Network.AWS.CodeCommit.Types.ErrorCode as Types
import qualified Network.AWS.CodeCommit.Types.ErrorMessage as Types
import qualified Network.AWS.CodeCommit.Types.RepositoryName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Returns information about errors in a BatchDisassociateApprovalRuleTemplateFromRepositories operation.
--
-- /See:/ 'mkBatchDisassociateApprovalRuleTemplateFromRepositoriesError' smart constructor.
data BatchDisassociateApprovalRuleTemplateFromRepositoriesError = BatchDisassociateApprovalRuleTemplateFromRepositoriesError'
  { -- | An error code that specifies whether the repository name was not valid or not found.
    errorCode :: Core.Maybe Types.ErrorCode,
    -- | An error message that provides details about why the repository name was either not found or not valid.
    errorMessage :: Core.Maybe Types.ErrorMessage,
    -- | The name of the repository where the association with the template was not able to be removed.
    repositoryName :: Core.Maybe Types.RepositoryName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BatchDisassociateApprovalRuleTemplateFromRepositoriesError' value with any optional fields omitted.
mkBatchDisassociateApprovalRuleTemplateFromRepositoriesError ::
  BatchDisassociateApprovalRuleTemplateFromRepositoriesError
mkBatchDisassociateApprovalRuleTemplateFromRepositoriesError =
  BatchDisassociateApprovalRuleTemplateFromRepositoriesError'
    { errorCode =
        Core.Nothing,
      errorMessage = Core.Nothing,
      repositoryName = Core.Nothing
    }

-- | An error code that specifies whether the repository name was not valid or not found.
--
-- /Note:/ Consider using 'errorCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdartfreErrorCode :: Lens.Lens' BatchDisassociateApprovalRuleTemplateFromRepositoriesError (Core.Maybe Types.ErrorCode)
bdartfreErrorCode = Lens.field @"errorCode"
{-# DEPRECATED bdartfreErrorCode "Use generic-lens or generic-optics with 'errorCode' instead." #-}

-- | An error message that provides details about why the repository name was either not found or not valid.
--
-- /Note:/ Consider using 'errorMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdartfreErrorMessage :: Lens.Lens' BatchDisassociateApprovalRuleTemplateFromRepositoriesError (Core.Maybe Types.ErrorMessage)
bdartfreErrorMessage = Lens.field @"errorMessage"
{-# DEPRECATED bdartfreErrorMessage "Use generic-lens or generic-optics with 'errorMessage' instead." #-}

-- | The name of the repository where the association with the template was not able to be removed.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdartfreRepositoryName :: Lens.Lens' BatchDisassociateApprovalRuleTemplateFromRepositoriesError (Core.Maybe Types.RepositoryName)
bdartfreRepositoryName = Lens.field @"repositoryName"
{-# DEPRECATED bdartfreRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

instance
  Core.FromJSON
    BatchDisassociateApprovalRuleTemplateFromRepositoriesError
  where
  parseJSON =
    Core.withObject
      "BatchDisassociateApprovalRuleTemplateFromRepositoriesError"
      Core.$ \x ->
        BatchDisassociateApprovalRuleTemplateFromRepositoriesError'
          Core.<$> (x Core..:? "errorCode") Core.<*> (x Core..:? "errorMessage")
            Core.<*> (x Core..:? "repositoryName")
