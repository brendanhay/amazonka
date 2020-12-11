-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Athena.Types.UnprocessedQueryExecutionId
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Athena.Types.UnprocessedQueryExecutionId
  ( UnprocessedQueryExecutionId (..),

    -- * Smart constructor
    mkUnprocessedQueryExecutionId,

    -- * Lenses
    uqeiErrorCode,
    uqeiQueryExecutionId,
    uqeiErrorMessage,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a query execution that failed to process.
--
-- /See:/ 'mkUnprocessedQueryExecutionId' smart constructor.
data UnprocessedQueryExecutionId = UnprocessedQueryExecutionId'
  { errorCode ::
      Lude.Maybe Lude.Text,
    queryExecutionId ::
      Lude.Maybe Lude.Text,
    errorMessage ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UnprocessedQueryExecutionId' with the minimum fields required to make a request.
--
-- * 'errorCode' - The error code returned when the query execution failed to process, if applicable.
-- * 'errorMessage' - The error message returned when the query execution failed to process, if applicable.
-- * 'queryExecutionId' - The unique identifier of the query execution.
mkUnprocessedQueryExecutionId ::
  UnprocessedQueryExecutionId
mkUnprocessedQueryExecutionId =
  UnprocessedQueryExecutionId'
    { errorCode = Lude.Nothing,
      queryExecutionId = Lude.Nothing,
      errorMessage = Lude.Nothing
    }

-- | The error code returned when the query execution failed to process, if applicable.
--
-- /Note:/ Consider using 'errorCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uqeiErrorCode :: Lens.Lens' UnprocessedQueryExecutionId (Lude.Maybe Lude.Text)
uqeiErrorCode = Lens.lens (errorCode :: UnprocessedQueryExecutionId -> Lude.Maybe Lude.Text) (\s a -> s {errorCode = a} :: UnprocessedQueryExecutionId)
{-# DEPRECATED uqeiErrorCode "Use generic-lens or generic-optics with 'errorCode' instead." #-}

-- | The unique identifier of the query execution.
--
-- /Note:/ Consider using 'queryExecutionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uqeiQueryExecutionId :: Lens.Lens' UnprocessedQueryExecutionId (Lude.Maybe Lude.Text)
uqeiQueryExecutionId = Lens.lens (queryExecutionId :: UnprocessedQueryExecutionId -> Lude.Maybe Lude.Text) (\s a -> s {queryExecutionId = a} :: UnprocessedQueryExecutionId)
{-# DEPRECATED uqeiQueryExecutionId "Use generic-lens or generic-optics with 'queryExecutionId' instead." #-}

-- | The error message returned when the query execution failed to process, if applicable.
--
-- /Note:/ Consider using 'errorMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uqeiErrorMessage :: Lens.Lens' UnprocessedQueryExecutionId (Lude.Maybe Lude.Text)
uqeiErrorMessage = Lens.lens (errorMessage :: UnprocessedQueryExecutionId -> Lude.Maybe Lude.Text) (\s a -> s {errorMessage = a} :: UnprocessedQueryExecutionId)
{-# DEPRECATED uqeiErrorMessage "Use generic-lens or generic-optics with 'errorMessage' instead." #-}

instance Lude.FromJSON UnprocessedQueryExecutionId where
  parseJSON =
    Lude.withObject
      "UnprocessedQueryExecutionId"
      ( \x ->
          UnprocessedQueryExecutionId'
            Lude.<$> (x Lude..:? "ErrorCode")
            Lude.<*> (x Lude..:? "QueryExecutionId")
            Lude.<*> (x Lude..:? "ErrorMessage")
      )
