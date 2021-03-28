{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.InputPrepareScheduleActionSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.InputPrepareScheduleActionSettings
  ( InputPrepareScheduleActionSettings (..)
  -- * Smart constructor
  , mkInputPrepareScheduleActionSettings
  -- * Lenses
  , ipsasInputAttachmentNameReference
  , ipsasInputClippingSettings
  , ipsasUrlPath
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types.InputClippingSettings as Types
import qualified Network.AWS.Prelude as Core

-- | Action to prepare an input for a future immediate input switch.
--
-- /See:/ 'mkInputPrepareScheduleActionSettings' smart constructor.
data InputPrepareScheduleActionSettings = InputPrepareScheduleActionSettings'
  { inputAttachmentNameReference :: Core.Maybe Core.Text
    -- ^ The name of the input attachment that should be prepared by this action. If no name is provided, the action will stop the most recent prepare (if any) when activated.
  , inputClippingSettings :: Core.Maybe Types.InputClippingSettings
    -- ^ Settings to let you create a clip of the file input, in order to set up the input to ingest only a portion of the file.
  , urlPath :: Core.Maybe [Core.Text]
    -- ^ The value for the variable portion of the URL for the dynamic input, for this instance of the input. Each time you use the same dynamic input in an input switch action, you can provide a different value, in order to connect the input to a different content source.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InputPrepareScheduleActionSettings' value with any optional fields omitted.
mkInputPrepareScheduleActionSettings
    :: InputPrepareScheduleActionSettings
mkInputPrepareScheduleActionSettings
  = InputPrepareScheduleActionSettings'{inputAttachmentNameReference
                                          = Core.Nothing,
                                        inputClippingSettings = Core.Nothing,
                                        urlPath = Core.Nothing}

-- | The name of the input attachment that should be prepared by this action. If no name is provided, the action will stop the most recent prepare (if any) when activated.
--
-- /Note:/ Consider using 'inputAttachmentNameReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipsasInputAttachmentNameReference :: Lens.Lens' InputPrepareScheduleActionSettings (Core.Maybe Core.Text)
ipsasInputAttachmentNameReference = Lens.field @"inputAttachmentNameReference"
{-# INLINEABLE ipsasInputAttachmentNameReference #-}
{-# DEPRECATED inputAttachmentNameReference "Use generic-lens or generic-optics with 'inputAttachmentNameReference' instead"  #-}

-- | Settings to let you create a clip of the file input, in order to set up the input to ingest only a portion of the file.
--
-- /Note:/ Consider using 'inputClippingSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipsasInputClippingSettings :: Lens.Lens' InputPrepareScheduleActionSettings (Core.Maybe Types.InputClippingSettings)
ipsasInputClippingSettings = Lens.field @"inputClippingSettings"
{-# INLINEABLE ipsasInputClippingSettings #-}
{-# DEPRECATED inputClippingSettings "Use generic-lens or generic-optics with 'inputClippingSettings' instead"  #-}

-- | The value for the variable portion of the URL for the dynamic input, for this instance of the input. Each time you use the same dynamic input in an input switch action, you can provide a different value, in order to connect the input to a different content source.
--
-- /Note:/ Consider using 'urlPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipsasUrlPath :: Lens.Lens' InputPrepareScheduleActionSettings (Core.Maybe [Core.Text])
ipsasUrlPath = Lens.field @"urlPath"
{-# INLINEABLE ipsasUrlPath #-}
{-# DEPRECATED urlPath "Use generic-lens or generic-optics with 'urlPath' instead"  #-}

instance Core.FromJSON InputPrepareScheduleActionSettings where
        toJSON InputPrepareScheduleActionSettings{..}
          = Core.object
              (Core.catMaybes
                 [("inputAttachmentNameReference" Core..=) Core.<$>
                    inputAttachmentNameReference,
                  ("inputClippingSettings" Core..=) Core.<$> inputClippingSettings,
                  ("urlPath" Core..=) Core.<$> urlPath])

instance Core.FromJSON InputPrepareScheduleActionSettings where
        parseJSON
          = Core.withObject "InputPrepareScheduleActionSettings" Core.$
              \ x ->
                InputPrepareScheduleActionSettings' Core.<$>
                  (x Core..:? "inputAttachmentNameReference") Core.<*>
                    x Core..:? "inputClippingSettings"
                    Core.<*> x Core..:? "urlPath"
