{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.BatchGetCommitsError
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.BatchGetCommitsError
  ( BatchGetCommitsError (..),

    -- * Smart constructor
    mkBatchGetCommitsError,

    -- * Lenses
    bgceCommitId,
    bgceErrorCode,
    bgceErrorMessage,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Returns information about errors in a BatchGetCommits operation.
--
-- /See:/ 'mkBatchGetCommitsError' smart constructor.
data BatchGetCommitsError = BatchGetCommitsError'
  { commitId ::
      Lude.Maybe Lude.Text,
    errorCode :: Lude.Maybe Lude.Text,
    errorMessage :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchGetCommitsError' with the minimum fields required to make a request.
--
-- * 'commitId' - A commit ID that either could not be found or was not in a valid format.
-- * 'errorCode' - An error code that specifies whether the commit ID was not valid or not found.
-- * 'errorMessage' - An error message that provides detail about why the commit ID either was not found or was not valid.
mkBatchGetCommitsError ::
  BatchGetCommitsError
mkBatchGetCommitsError =
  BatchGetCommitsError'
    { commitId = Lude.Nothing,
      errorCode = Lude.Nothing,
      errorMessage = Lude.Nothing
    }

-- | A commit ID that either could not be found or was not in a valid format.
--
-- /Note:/ Consider using 'commitId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgceCommitId :: Lens.Lens' BatchGetCommitsError (Lude.Maybe Lude.Text)
bgceCommitId = Lens.lens (commitId :: BatchGetCommitsError -> Lude.Maybe Lude.Text) (\s a -> s {commitId = a} :: BatchGetCommitsError)
{-# DEPRECATED bgceCommitId "Use generic-lens or generic-optics with 'commitId' instead." #-}

-- | An error code that specifies whether the commit ID was not valid or not found.
--
-- /Note:/ Consider using 'errorCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgceErrorCode :: Lens.Lens' BatchGetCommitsError (Lude.Maybe Lude.Text)
bgceErrorCode = Lens.lens (errorCode :: BatchGetCommitsError -> Lude.Maybe Lude.Text) (\s a -> s {errorCode = a} :: BatchGetCommitsError)
{-# DEPRECATED bgceErrorCode "Use generic-lens or generic-optics with 'errorCode' instead." #-}

-- | An error message that provides detail about why the commit ID either was not found or was not valid.
--
-- /Note:/ Consider using 'errorMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgceErrorMessage :: Lens.Lens' BatchGetCommitsError (Lude.Maybe Lude.Text)
bgceErrorMessage = Lens.lens (errorMessage :: BatchGetCommitsError -> Lude.Maybe Lude.Text) (\s a -> s {errorMessage = a} :: BatchGetCommitsError)
{-# DEPRECATED bgceErrorMessage "Use generic-lens or generic-optics with 'errorMessage' instead." #-}

instance Lude.FromJSON BatchGetCommitsError where
  parseJSON =
    Lude.withObject
      "BatchGetCommitsError"
      ( \x ->
          BatchGetCommitsError'
            Lude.<$> (x Lude..:? "commitId")
            Lude.<*> (x Lude..:? "errorCode")
            Lude.<*> (x Lude..:? "errorMessage")
      )
