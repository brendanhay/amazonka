{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.WriteTreatmentResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Pinpoint.Types.WriteTreatmentResource
  ( WriteTreatmentResource (..)
  -- * Smart constructor
  , mkWriteTreatmentResource
  -- * Lenses
  , wtrSizePercent
  , wtrCustomDeliveryConfiguration
  , wtrMessageConfiguration
  , wtrSchedule
  , wtrTemplateConfiguration
  , wtrTreatmentDescription
  , wtrTreatmentName
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types.CustomDeliveryConfiguration as Types
import qualified Network.AWS.Pinpoint.Types.MessageConfiguration as Types
import qualified Network.AWS.Pinpoint.Types.Schedule as Types
import qualified Network.AWS.Pinpoint.Types.TemplateConfiguration as Types
import qualified Network.AWS.Prelude as Core

-- | Specifies the settings for a campaign treatment. A /treatment/ is a variation of a campaign that's used for A/B testing of a campaign.
--
-- /See:/ 'mkWriteTreatmentResource' smart constructor.
data WriteTreatmentResource = WriteTreatmentResource'
  { sizePercent :: Core.Int
    -- ^ The allocated percentage of users (segment members) to send the treatment to.
  , customDeliveryConfiguration :: Core.Maybe Types.CustomDeliveryConfiguration
    -- ^ The delivery configuration settings for sending the treatment through a custom channel. This object is required if the MessageConfiguration object for the treatment specifies a CustomMessage object.
  , messageConfiguration :: Core.Maybe Types.MessageConfiguration
    -- ^ The message configuration settings for the treatment.
  , schedule :: Core.Maybe Types.Schedule
    -- ^ The schedule settings for the treatment.
  , templateConfiguration :: Core.Maybe Types.TemplateConfiguration
    -- ^ The message template to use for the treatment.
  , treatmentDescription :: Core.Maybe Core.Text
    -- ^ A custom description of the treatment.
  , treatmentName :: Core.Maybe Core.Text
    -- ^ A custom name for the treatment.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'WriteTreatmentResource' value with any optional fields omitted.
mkWriteTreatmentResource
    :: Core.Int -- ^ 'sizePercent'
    -> WriteTreatmentResource
mkWriteTreatmentResource sizePercent
  = WriteTreatmentResource'{sizePercent,
                            customDeliveryConfiguration = Core.Nothing,
                            messageConfiguration = Core.Nothing, schedule = Core.Nothing,
                            templateConfiguration = Core.Nothing,
                            treatmentDescription = Core.Nothing, treatmentName = Core.Nothing}

-- | The allocated percentage of users (segment members) to send the treatment to.
--
-- /Note:/ Consider using 'sizePercent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wtrSizePercent :: Lens.Lens' WriteTreatmentResource Core.Int
wtrSizePercent = Lens.field @"sizePercent"
{-# INLINEABLE wtrSizePercent #-}
{-# DEPRECATED sizePercent "Use generic-lens or generic-optics with 'sizePercent' instead"  #-}

-- | The delivery configuration settings for sending the treatment through a custom channel. This object is required if the MessageConfiguration object for the treatment specifies a CustomMessage object.
--
-- /Note:/ Consider using 'customDeliveryConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wtrCustomDeliveryConfiguration :: Lens.Lens' WriteTreatmentResource (Core.Maybe Types.CustomDeliveryConfiguration)
wtrCustomDeliveryConfiguration = Lens.field @"customDeliveryConfiguration"
{-# INLINEABLE wtrCustomDeliveryConfiguration #-}
{-# DEPRECATED customDeliveryConfiguration "Use generic-lens or generic-optics with 'customDeliveryConfiguration' instead"  #-}

-- | The message configuration settings for the treatment.
--
-- /Note:/ Consider using 'messageConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wtrMessageConfiguration :: Lens.Lens' WriteTreatmentResource (Core.Maybe Types.MessageConfiguration)
wtrMessageConfiguration = Lens.field @"messageConfiguration"
{-# INLINEABLE wtrMessageConfiguration #-}
{-# DEPRECATED messageConfiguration "Use generic-lens or generic-optics with 'messageConfiguration' instead"  #-}

-- | The schedule settings for the treatment.
--
-- /Note:/ Consider using 'schedule' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wtrSchedule :: Lens.Lens' WriteTreatmentResource (Core.Maybe Types.Schedule)
wtrSchedule = Lens.field @"schedule"
{-# INLINEABLE wtrSchedule #-}
{-# DEPRECATED schedule "Use generic-lens or generic-optics with 'schedule' instead"  #-}

-- | The message template to use for the treatment.
--
-- /Note:/ Consider using 'templateConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wtrTemplateConfiguration :: Lens.Lens' WriteTreatmentResource (Core.Maybe Types.TemplateConfiguration)
wtrTemplateConfiguration = Lens.field @"templateConfiguration"
{-# INLINEABLE wtrTemplateConfiguration #-}
{-# DEPRECATED templateConfiguration "Use generic-lens or generic-optics with 'templateConfiguration' instead"  #-}

-- | A custom description of the treatment.
--
-- /Note:/ Consider using 'treatmentDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wtrTreatmentDescription :: Lens.Lens' WriteTreatmentResource (Core.Maybe Core.Text)
wtrTreatmentDescription = Lens.field @"treatmentDescription"
{-# INLINEABLE wtrTreatmentDescription #-}
{-# DEPRECATED treatmentDescription "Use generic-lens or generic-optics with 'treatmentDescription' instead"  #-}

-- | A custom name for the treatment.
--
-- /Note:/ Consider using 'treatmentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wtrTreatmentName :: Lens.Lens' WriteTreatmentResource (Core.Maybe Core.Text)
wtrTreatmentName = Lens.field @"treatmentName"
{-# INLINEABLE wtrTreatmentName #-}
{-# DEPRECATED treatmentName "Use generic-lens or generic-optics with 'treatmentName' instead"  #-}

instance Core.FromJSON WriteTreatmentResource where
        toJSON WriteTreatmentResource{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("SizePercent" Core..= sizePercent),
                  ("CustomDeliveryConfiguration" Core..=) Core.<$>
                    customDeliveryConfiguration,
                  ("MessageConfiguration" Core..=) Core.<$> messageConfiguration,
                  ("Schedule" Core..=) Core.<$> schedule,
                  ("TemplateConfiguration" Core..=) Core.<$> templateConfiguration,
                  ("TreatmentDescription" Core..=) Core.<$> treatmentDescription,
                  ("TreatmentName" Core..=) Core.<$> treatmentName])
