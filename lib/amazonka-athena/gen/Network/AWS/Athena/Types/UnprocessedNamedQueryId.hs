{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Athena.Types.UnprocessedNamedQueryId
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Athena.Types.UnprocessedNamedQueryId
  ( UnprocessedNamedQueryId (..),

    -- * Smart constructor
    mkUnprocessedNamedQueryId,

    -- * Lenses
    unqiNamedQueryId,
    unqiErrorCode,
    unqiErrorMessage,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about a named query ID that could not be processed.
--
-- /See:/ 'mkUnprocessedNamedQueryId' smart constructor.
data UnprocessedNamedQueryId = UnprocessedNamedQueryId'
  { -- | The unique identifier of the named query.
    namedQueryId :: Lude.Maybe Lude.Text,
    -- | The error code returned when the processing request for the named query failed, if applicable.
    errorCode :: Lude.Maybe Lude.Text,
    -- | The error message returned when the processing request for the named query failed, if applicable.
    errorMessage :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UnprocessedNamedQueryId' with the minimum fields required to make a request.
--
-- * 'namedQueryId' - The unique identifier of the named query.
-- * 'errorCode' - The error code returned when the processing request for the named query failed, if applicable.
-- * 'errorMessage' - The error message returned when the processing request for the named query failed, if applicable.
mkUnprocessedNamedQueryId ::
  UnprocessedNamedQueryId
mkUnprocessedNamedQueryId =
  UnprocessedNamedQueryId'
    { namedQueryId = Lude.Nothing,
      errorCode = Lude.Nothing,
      errorMessage = Lude.Nothing
    }

-- | The unique identifier of the named query.
--
-- /Note:/ Consider using 'namedQueryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
unqiNamedQueryId :: Lens.Lens' UnprocessedNamedQueryId (Lude.Maybe Lude.Text)
unqiNamedQueryId = Lens.lens (namedQueryId :: UnprocessedNamedQueryId -> Lude.Maybe Lude.Text) (\s a -> s {namedQueryId = a} :: UnprocessedNamedQueryId)
{-# DEPRECATED unqiNamedQueryId "Use generic-lens or generic-optics with 'namedQueryId' instead." #-}

-- | The error code returned when the processing request for the named query failed, if applicable.
--
-- /Note:/ Consider using 'errorCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
unqiErrorCode :: Lens.Lens' UnprocessedNamedQueryId (Lude.Maybe Lude.Text)
unqiErrorCode = Lens.lens (errorCode :: UnprocessedNamedQueryId -> Lude.Maybe Lude.Text) (\s a -> s {errorCode = a} :: UnprocessedNamedQueryId)
{-# DEPRECATED unqiErrorCode "Use generic-lens or generic-optics with 'errorCode' instead." #-}

-- | The error message returned when the processing request for the named query failed, if applicable.
--
-- /Note:/ Consider using 'errorMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
unqiErrorMessage :: Lens.Lens' UnprocessedNamedQueryId (Lude.Maybe Lude.Text)
unqiErrorMessage = Lens.lens (errorMessage :: UnprocessedNamedQueryId -> Lude.Maybe Lude.Text) (\s a -> s {errorMessage = a} :: UnprocessedNamedQueryId)
{-# DEPRECATED unqiErrorMessage "Use generic-lens or generic-optics with 'errorMessage' instead." #-}

instance Lude.FromJSON UnprocessedNamedQueryId where
  parseJSON =
    Lude.withObject
      "UnprocessedNamedQueryId"
      ( \x ->
          UnprocessedNamedQueryId'
            Lude.<$> (x Lude..:? "NamedQueryId")
            Lude.<*> (x Lude..:? "ErrorCode")
            Lude.<*> (x Lude..:? "ErrorMessage")
      )
