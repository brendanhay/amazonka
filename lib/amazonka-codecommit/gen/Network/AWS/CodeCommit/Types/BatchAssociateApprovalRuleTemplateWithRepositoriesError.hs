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
    baartwreRepositoryName,
    baartwreErrorMessage,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Returns information about errors in a BatchAssociateApprovalRuleTemplateWithRepositories operation.
--
-- /See:/ 'mkBatchAssociateApprovalRuleTemplateWithRepositoriesError' smart constructor.
data BatchAssociateApprovalRuleTemplateWithRepositoriesError = BatchAssociateApprovalRuleTemplateWithRepositoriesError'
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

-- | Creates a value of 'BatchAssociateApprovalRuleTemplateWithRepositoriesError' with the minimum fields required to make a request.
--
-- * 'errorCode' - An error code that specifies whether the repository name was not valid or not found.
-- * 'errorMessage' - An error message that provides details about why the repository name was not found or not valid.
-- * 'repositoryName' - The name of the repository where the association was not made.
mkBatchAssociateApprovalRuleTemplateWithRepositoriesError ::
  BatchAssociateApprovalRuleTemplateWithRepositoriesError
mkBatchAssociateApprovalRuleTemplateWithRepositoriesError =
  BatchAssociateApprovalRuleTemplateWithRepositoriesError'
    { errorCode =
        Lude.Nothing,
      repositoryName = Lude.Nothing,
      errorMessage = Lude.Nothing
    }

-- | An error code that specifies whether the repository name was not valid or not found.
--
-- /Note:/ Consider using 'errorCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
baartwreErrorCode :: Lens.Lens' BatchAssociateApprovalRuleTemplateWithRepositoriesError (Lude.Maybe Lude.Text)
baartwreErrorCode = Lens.lens (errorCode :: BatchAssociateApprovalRuleTemplateWithRepositoriesError -> Lude.Maybe Lude.Text) (\s a -> s {errorCode = a} :: BatchAssociateApprovalRuleTemplateWithRepositoriesError)
{-# DEPRECATED baartwreErrorCode "Use generic-lens or generic-optics with 'errorCode' instead." #-}

-- | The name of the repository where the association was not made.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
baartwreRepositoryName :: Lens.Lens' BatchAssociateApprovalRuleTemplateWithRepositoriesError (Lude.Maybe Lude.Text)
baartwreRepositoryName = Lens.lens (repositoryName :: BatchAssociateApprovalRuleTemplateWithRepositoriesError -> Lude.Maybe Lude.Text) (\s a -> s {repositoryName = a} :: BatchAssociateApprovalRuleTemplateWithRepositoriesError)
{-# DEPRECATED baartwreRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

-- | An error message that provides details about why the repository name was not found or not valid.
--
-- /Note:/ Consider using 'errorMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
baartwreErrorMessage :: Lens.Lens' BatchAssociateApprovalRuleTemplateWithRepositoriesError (Lude.Maybe Lude.Text)
baartwreErrorMessage = Lens.lens (errorMessage :: BatchAssociateApprovalRuleTemplateWithRepositoriesError -> Lude.Maybe Lude.Text) (\s a -> s {errorMessage = a} :: BatchAssociateApprovalRuleTemplateWithRepositoriesError)
{-# DEPRECATED baartwreErrorMessage "Use generic-lens or generic-optics with 'errorMessage' instead." #-}

instance
  Lude.FromJSON
    BatchAssociateApprovalRuleTemplateWithRepositoriesError
  where
  parseJSON =
    Lude.withObject
      "BatchAssociateApprovalRuleTemplateWithRepositoriesError"
      ( \x ->
          BatchAssociateApprovalRuleTemplateWithRepositoriesError'
            Lude.<$> (x Lude..:? "errorCode")
            Lude.<*> (x Lude..:? "repositoryName")
            Lude.<*> (x Lude..:? "errorMessage")
      )
