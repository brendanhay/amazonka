{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.IncompatibilityMessage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DeviceFarm.Types.IncompatibilityMessage
  ( IncompatibilityMessage (..)
  -- * Smart constructor
  , mkIncompatibilityMessage
  -- * Lenses
  , imMessage
  , imType
  ) where

import qualified Network.AWS.DeviceFarm.Types.DeviceAttribute as Types
import qualified Network.AWS.DeviceFarm.Types.Message as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents information about incompatibility.
--
-- /See:/ 'mkIncompatibilityMessage' smart constructor.
data IncompatibilityMessage = IncompatibilityMessage'
  { message :: Core.Maybe Types.Message
    -- ^ A message about the incompatibility.
  , type' :: Core.Maybe Types.DeviceAttribute
    -- ^ The type of incompatibility.
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
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'IncompatibilityMessage' value with any optional fields omitted.
mkIncompatibilityMessage
    :: IncompatibilityMessage
mkIncompatibilityMessage
  = IncompatibilityMessage'{message = Core.Nothing,
                            type' = Core.Nothing}

-- | A message about the incompatibility.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
imMessage :: Lens.Lens' IncompatibilityMessage (Core.Maybe Types.Message)
imMessage = Lens.field @"message"
{-# INLINEABLE imMessage #-}
{-# DEPRECATED message "Use generic-lens or generic-optics with 'message' instead"  #-}

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
imType :: Lens.Lens' IncompatibilityMessage (Core.Maybe Types.DeviceAttribute)
imType = Lens.field @"type'"
{-# INLINEABLE imType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

instance Core.FromJSON IncompatibilityMessage where
        parseJSON
          = Core.withObject "IncompatibilityMessage" Core.$
              \ x ->
                IncompatibilityMessage' Core.<$>
                  (x Core..:? "message") Core.<*> x Core..:? "type"
