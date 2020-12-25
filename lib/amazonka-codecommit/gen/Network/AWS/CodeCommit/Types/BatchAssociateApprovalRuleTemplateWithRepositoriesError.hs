{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.BatchAssociateApprovalRuleTemplateWithRepositoriesError
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.BatchAssociateApprovalRuleTemplateWithRepositoriesError
  ( BatchAssociateApprovalRuleTemplateWithRepositoriesError (..),

    -- * Smart constructor
    mkBatchAssociateApprovalRuleTemplateWithRepositoriesError,

    -- * Lenses
    baartwreErrorCode,
    baartwreErrorMessage,
    baartwreRepositoryName,
  )
where

import qualified Network.AWS.CodeCommit.Types.ErrorCode as Types
import qualified Network.AWS.CodeCommit.Types.ErrorMessage as Types
import qualified Network.AWS.CodeCommit.Types.RepositoryName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Returns information about errors in a BatchAssociateApprovalRuleTemplateWithRepositories operation.
--
-- /See:/ 'mkBatchAssociateApprovalRuleTemplateWithRepositoriesError' smart constructor.
data BatchAssociateApprovalRuleTemplateWithRepositoriesError = BatchAssociateApprovalRuleTemplateWithRepositoriesError'
  { -- | An error code that specifies whether the repository name was not valid or not found.
    errorCode :: Core.Maybe Types.ErrorCode,
    -- | An error message that provides details about why the repository name was not found or not valid.
    errorMessage :: Core.Maybe Types.ErrorMessage,
    -- | The name of the repository where the association was not made.
    repositoryName :: Core.Maybe Types.RepositoryName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BatchAssociateApprovalRuleTemplateWithRepositoriesError' value with any optional fields omitted.
mkBatchAssociateApprovalRuleTemplateWithRepositoriesError ::
  BatchAssociateApprovalRuleTemplateWithRepositoriesError
mkBatchAssociateApprovalRuleTemplateWithRepositoriesError =
  BatchAssociateApprovalRuleTemplateWithRepositoriesError'
    { errorCode =
        Core.Nothing,
      errorMessage = Core.Nothing,
      repositoryName = Core.Nothing
    }

-- | An error code that specifies whether the repository name was not valid or not found.
--
-- /Note:/ Consider using 'errorCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
baartwreErrorCode :: Lens.Lens' BatchAssociateApprovalRuleTemplateWithRepositoriesError (Core.Maybe Types.ErrorCode)
baartwreErrorCode = Lens.field @"errorCode"
{-# DEPRECATED baartwreErrorCode "Use generic-lens or generic-optics with 'errorCode' instead." #-}

-- | An error message that provides details about why the repository name was not found or not valid.
--
-- /Note:/ Consider using 'errorMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
baartwreErrorMessage :: Lens.Lens' BatchAssociateApprovalRuleTemplateWithRepositoriesError (Core.Maybe Types.ErrorMessage)
baartwreErrorMessage = Lens.field @"errorMessage"
{-# DEPRECATED baartwreErrorMessage "Use generic-lens or generic-optics with 'errorMessage' instead." #-}

-- | The name of the repository where the association was not made.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
baartwreRepositoryName :: Lens.Lens' BatchAssociateApprovalRuleTemplateWithRepositoriesError (Core.Maybe Types.RepositoryName)
baartwreRepositoryName = Lens.field @"repositoryName"
{-# DEPRECATED baartwreRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

instance
  Core.FromJSON
    BatchAssociateApprovalRuleTemplateWithRepositoriesError
  where
  parseJSON =
    Core.withObject
      "BatchAssociateApprovalRuleTemplateWithRepositoriesError"
      Core.$ \x ->
        BatchAssociateApprovalRuleTemplateWithRepositoriesError'
          Core.<$> (x Core..:? "errorCode")
          Core.<*> (x Core..:? "errorMessage")
          Core.<*> (x Core..:? "repositoryName")
