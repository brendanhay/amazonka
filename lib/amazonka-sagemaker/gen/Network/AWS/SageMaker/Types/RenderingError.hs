{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.RenderingError
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.RenderingError
  ( RenderingError (..),

    -- * Smart constructor
    mkRenderingError,

    -- * Lenses
    reCode,
    reMessage,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A description of an error that occurred while rendering the template.
--
-- /See:/ 'mkRenderingError' smart constructor.
data RenderingError = RenderingError'
  { code :: Lude.Text,
    message :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RenderingError' with the minimum fields required to make a request.
--
-- * 'code' - A unique identifier for a specific class of errors.
-- * 'message' - A human-readable message describing the error.
mkRenderingError ::
  -- | 'code'
  Lude.Text ->
  -- | 'message'
  Lude.Text ->
  RenderingError
mkRenderingError pCode_ pMessage_ =
  RenderingError' {code = pCode_, message = pMessage_}

-- | A unique identifier for a specific class of errors.
--
-- /Note:/ Consider using 'code' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
reCode :: Lens.Lens' RenderingError Lude.Text
reCode = Lens.lens (code :: RenderingError -> Lude.Text) (\s a -> s {code = a} :: RenderingError)
{-# DEPRECATED reCode "Use generic-lens or generic-optics with 'code' instead." #-}

-- | A human-readable message describing the error.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
reMessage :: Lens.Lens' RenderingError Lude.Text
reMessage = Lens.lens (message :: RenderingError -> Lude.Text) (\s a -> s {message = a} :: RenderingError)
{-# DEPRECATED reMessage "Use generic-lens or generic-optics with 'message' instead." #-}

instance Lude.FromJSON RenderingError where
  parseJSON =
    Lude.withObject
      "RenderingError"
      ( \x ->
          RenderingError'
            Lude.<$> (x Lude..: "Code") Lude.<*> (x Lude..: "Message")
      )
