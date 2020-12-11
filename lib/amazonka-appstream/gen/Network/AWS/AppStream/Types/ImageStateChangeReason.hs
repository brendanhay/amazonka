-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Types.ImageStateChangeReason
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppStream.Types.ImageStateChangeReason
  ( ImageStateChangeReason (..),

    -- * Smart constructor
    mkImageStateChangeReason,

    -- * Lenses
    iscrCode,
    iscrMessage,
  )
where

import Network.AWS.AppStream.Types.ImageStateChangeReasonCode
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the reason why the last image state change occurred.
--
-- /See:/ 'mkImageStateChangeReason' smart constructor.
data ImageStateChangeReason = ImageStateChangeReason'
  { code ::
      Lude.Maybe ImageStateChangeReasonCode,
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

-- | Creates a value of 'ImageStateChangeReason' with the minimum fields required to make a request.
--
-- * 'code' - The state change reason code.
-- * 'message' - The state change reason message.
mkImageStateChangeReason ::
  ImageStateChangeReason
mkImageStateChangeReason =
  ImageStateChangeReason'
    { code = Lude.Nothing,
      message = Lude.Nothing
    }

-- | The state change reason code.
--
-- /Note:/ Consider using 'code' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iscrCode :: Lens.Lens' ImageStateChangeReason (Lude.Maybe ImageStateChangeReasonCode)
iscrCode = Lens.lens (code :: ImageStateChangeReason -> Lude.Maybe ImageStateChangeReasonCode) (\s a -> s {code = a} :: ImageStateChangeReason)
{-# DEPRECATED iscrCode "Use generic-lens or generic-optics with 'code' instead." #-}

-- | The state change reason message.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iscrMessage :: Lens.Lens' ImageStateChangeReason (Lude.Maybe Lude.Text)
iscrMessage = Lens.lens (message :: ImageStateChangeReason -> Lude.Maybe Lude.Text) (\s a -> s {message = a} :: ImageStateChangeReason)
{-# DEPRECATED iscrMessage "Use generic-lens or generic-optics with 'message' instead." #-}

instance Lude.FromJSON ImageStateChangeReason where
  parseJSON =
    Lude.withObject
      "ImageStateChangeReason"
      ( \x ->
          ImageStateChangeReason'
            Lude.<$> (x Lude..:? "Code") Lude.<*> (x Lude..:? "Message")
      )
