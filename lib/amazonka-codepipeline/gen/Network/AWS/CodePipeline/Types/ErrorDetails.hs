-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.ErrorDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.ErrorDetails
  ( ErrorDetails (..),

    -- * Smart constructor
    mkErrorDetails,

    -- * Lenses
    edCode,
    edMessage,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents information about an error in AWS CodePipeline.
--
-- /See:/ 'mkErrorDetails' smart constructor.
data ErrorDetails = ErrorDetails'
  { code :: Lude.Maybe Lude.Text,
    message :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ErrorDetails' with the minimum fields required to make a request.
--
-- * 'code' - The system ID or number code of the error.
-- * 'message' - The text of the error message.
mkErrorDetails ::
  ErrorDetails
mkErrorDetails =
  ErrorDetails' {code = Lude.Nothing, message = Lude.Nothing}

-- | The system ID or number code of the error.
--
-- /Note:/ Consider using 'code' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edCode :: Lens.Lens' ErrorDetails (Lude.Maybe Lude.Text)
edCode = Lens.lens (code :: ErrorDetails -> Lude.Maybe Lude.Text) (\s a -> s {code = a} :: ErrorDetails)
{-# DEPRECATED edCode "Use generic-lens or generic-optics with 'code' instead." #-}

-- | The text of the error message.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edMessage :: Lens.Lens' ErrorDetails (Lude.Maybe Lude.Text)
edMessage = Lens.lens (message :: ErrorDetails -> Lude.Maybe Lude.Text) (\s a -> s {message = a} :: ErrorDetails)
{-# DEPRECATED edMessage "Use generic-lens or generic-optics with 'message' instead." #-}

instance Lude.FromJSON ErrorDetails where
  parseJSON =
    Lude.withObject
      "ErrorDetails"
      ( \x ->
          ErrorDetails'
            Lude.<$> (x Lude..:? "code") Lude.<*> (x Lude..:? "message")
      )
