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
    bdartfreRepositoryName,
    bdartfreErrorMessage,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Returns information about errors in a BatchDisassociateApprovalRuleTemplateFromRepositories operation.
--
-- /See:/ 'mkBatchDisassociateApprovalRuleTemplateFromRepositoriesError' smart constructor.
data BatchDisassociateApprovalRuleTemplateFromRepositoriesError = BatchDisassociateApprovalRuleTemplateFromRepositoriesError'
  { errorCode ::
      Lude.Maybe
        Lude.Text,
    repositoryName ::
      Lude.Maybe
        Lude.Text,
    errorMessage ::
      Lude.Maybe
        Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass
    ( Lude.Hashable,
      Lude.NFData
    )

-- | Creates a value of 'BatchDisassociateApprovalRuleTemplateFromRepositoriesError' with the minimum fields required to make a request.
--
-- * 'errorCode' - An error code that specifies whether the repository name was not valid or not found.
-- * 'errorMessage' - An error message that provides details about why the repository name was either not found or not valid.
-- * 'repositoryName' - The name of the repository where the association with the template was not able to be removed.
mkBatchDisassociateApprovalRuleTemplateFromRepositoriesError ::
  BatchDisassociateApprovalRuleTemplateFromRepositoriesError
mkBatchDisassociateApprovalRuleTemplateFromRepositoriesError =
  BatchDisassociateApprovalRuleTemplateFromRepositoriesError'
    { errorCode =
        Lude.Nothing,
      repositoryName = Lude.Nothing,
      errorMessage = Lude.Nothing
    }

-- | An error code that specifies whether the repository name was not valid or not found.
--
-- /Note:/ Consider using 'errorCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdartfreErrorCode :: Lens.Lens' BatchDisassociateApprovalRuleTemplateFromRepositoriesError (Lude.Maybe Lude.Text)
bdartfreErrorCode = Lens.lens (errorCode :: BatchDisassociateApprovalRuleTemplateFromRepositoriesError -> Lude.Maybe Lude.Text) (\s a -> s {errorCode = a} :: BatchDisassociateApprovalRuleTemplateFromRepositoriesError)
{-# DEPRECATED bdartfreErrorCode "Use generic-lens or generic-optics with 'errorCode' instead." #-}

-- | The name of the repository where the association with the template was not able to be removed.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdartfreRepositoryName :: Lens.Lens' BatchDisassociateApprovalRuleTemplateFromRepositoriesError (Lude.Maybe Lude.Text)
bdartfreRepositoryName = Lens.lens (repositoryName :: BatchDisassociateApprovalRuleTemplateFromRepositoriesError -> Lude.Maybe Lude.Text) (\s a -> s {repositoryName = a} :: BatchDisassociateApprovalRuleTemplateFromRepositoriesError)
{-# DEPRECATED bdartfreRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

-- | An error message that provides details about why the repository name was either not found or not valid.
--
-- /Note:/ Consider using 'errorMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdartfreErrorMessage :: Lens.Lens' BatchDisassociateApprovalRuleTemplateFromRepositoriesError (Lude.Maybe Lude.Text)
bdartfreErrorMessage = Lens.lens (errorMessage :: BatchDisassociateApprovalRuleTemplateFromRepositoriesError -> Lude.Maybe Lude.Text) (\s a -> s {errorMessage = a} :: BatchDisassociateApprovalRuleTemplateFromRepositoriesError)
{-# DEPRECATED bdartfreErrorMessage "Use generic-lens or generic-optics with 'errorMessage' instead." #-}

instance
  Lude.FromJSON
    BatchDisassociateApprovalRuleTemplateFromRepositoriesError
  where
  parseJSON =
    Lude.withObject
      "BatchDisassociateApprovalRuleTemplateFromRepositoriesError"
      ( \x ->
          BatchDisassociateApprovalRuleTemplateFromRepositoriesError'
            Lude.<$> (x Lude..:? "errorCode") Lude.<*> (x Lude..:? "repositoryName")
              Lude.<*> (x Lude..:? "errorMessage")
      )
