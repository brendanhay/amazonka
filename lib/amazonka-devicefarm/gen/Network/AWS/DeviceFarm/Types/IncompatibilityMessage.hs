-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.IncompatibilityMessage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.IncompatibilityMessage
  ( IncompatibilityMessage (..),

    -- * Smart constructor
    mkIncompatibilityMessage,

    -- * Lenses
    imType,
    imMessage,
  )
where

import Network.AWS.DeviceFarm.Types.DeviceAttribute
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents information about incompatibility.
--
-- /See:/ 'mkIncompatibilityMessage' smart constructor.
data IncompatibilityMessage = IncompatibilityMessage'
  { type' ::
      Lude.Maybe DeviceAttribute,
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

-- | Creates a value of 'IncompatibilityMessage' with the minimum fields required to make a request.
--
-- * 'message' - A message about the incompatibility.
-- * 'type'' - The type of incompatibility.
--
-- Allowed values include:
--
--     * ARN
--
--
--     * FORM_FACTOR (for example, phone or tablet)
--
--
--     * MANUFACTURER
--
--
--     * PLATFORM (for example, Android or iOS)
--
--
--     * REMOTE_ACCESS_ENABLED
--
--
--     * APPIUM_VERSION
mkIncompatibilityMessage ::
  IncompatibilityMessage
mkIncompatibilityMessage =
  IncompatibilityMessage'
    { type' = Lude.Nothing,
      message = Lude.Nothing
    }

-- | The type of incompatibility.
--
-- Allowed values include:
--
--     * ARN
--
--
--     * FORM_FACTOR (for example, phone or tablet)
--
--
--     * MANUFACTURER
--
--
--     * PLATFORM (for example, Android or iOS)
--
--
--     * REMOTE_ACCESS_ENABLED
--
--
--     * APPIUM_VERSION
--
--
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
imType :: Lens.Lens' IncompatibilityMessage (Lude.Maybe DeviceAttribute)
imType = Lens.lens (type' :: IncompatibilityMessage -> Lude.Maybe DeviceAttribute) (\s a -> s {type' = a} :: IncompatibilityMessage)
{-# DEPRECATED imType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | A message about the incompatibility.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
imMessage :: Lens.Lens' IncompatibilityMessage (Lude.Maybe Lude.Text)
imMessage = Lens.lens (message :: IncompatibilityMessage -> Lude.Maybe Lude.Text) (\s a -> s {message = a} :: IncompatibilityMessage)
{-# DEPRECATED imMessage "Use generic-lens or generic-optics with 'message' instead." #-}

instance Lude.FromJSON IncompatibilityMessage where
  parseJSON =
    Lude.withObject
      "IncompatibilityMessage"
      ( \x ->
          IncompatibilityMessage'
            Lude.<$> (x Lude..:? "type") Lude.<*> (x Lude..:? "message")
      )
