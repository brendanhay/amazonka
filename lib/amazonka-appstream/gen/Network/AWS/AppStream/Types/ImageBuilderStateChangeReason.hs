{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Types.ImageBuilderStateChangeReason
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppStream.Types.ImageBuilderStateChangeReason
  ( ImageBuilderStateChangeReason (..),

    -- * Smart constructor
    mkImageBuilderStateChangeReason,

    -- * Lenses
    ibscrCode,
    ibscrMessage,
  )
where

import Network.AWS.AppStream.Types.ImageBuilderStateChangeReasonCode
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the reason why the last image builder state change occurred.
--
-- /See:/ 'mkImageBuilderStateChangeReason' smart constructor.
data ImageBuilderStateChangeReason = ImageBuilderStateChangeReason'
  { -- | The state change reason code.
    code :: Lude.Maybe ImageBuilderStateChangeReasonCode,
    -- | The state change reason message.
    message :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ImageBuilderStateChangeReason' with the minimum fields required to make a request.
--
-- * 'code' - The state change reason code.
-- * 'message' - The state change reason message.
mkImageBuilderStateChangeReason ::
  ImageBuilderStateChangeReason
mkImageBuilderStateChangeReason =
  ImageBuilderStateChangeReason'
    { code = Lude.Nothing,
      message = Lude.Nothing
    }

-- | The state change reason code.
--
-- /Note:/ Consider using 'code' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ibscrCode :: Lens.Lens' ImageBuilderStateChangeReason (Lude.Maybe ImageBuilderStateChangeReasonCode)
ibscrCode = Lens.lens (code :: ImageBuilderStateChangeReason -> Lude.Maybe ImageBuilderStateChangeReasonCode) (\s a -> s {code = a} :: ImageBuilderStateChangeReason)
{-# DEPRECATED ibscrCode "Use generic-lens or generic-optics with 'code' instead." #-}

-- | The state change reason message.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ibscrMessage :: Lens.Lens' ImageBuilderStateChangeReason (Lude.Maybe Lude.Text)
ibscrMessage = Lens.lens (message :: ImageBuilderStateChangeReason -> Lude.Maybe Lude.Text) (\s a -> s {message = a} :: ImageBuilderStateChangeReason)
{-# DEPRECATED ibscrMessage "Use generic-lens or generic-optics with 'message' instead." #-}

instance Lude.FromJSON ImageBuilderStateChangeReason where
  parseJSON =
    Lude.withObject
      "ImageBuilderStateChangeReason"
      ( \x ->
          ImageBuilderStateChangeReason'
            Lude.<$> (x Lude..:? "Code") Lude.<*> (x Lude..:? "Message")
      )
