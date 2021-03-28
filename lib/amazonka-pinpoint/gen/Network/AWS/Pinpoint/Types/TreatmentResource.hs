{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.TreatmentResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Pinpoint.Types.TreatmentResource
  ( TreatmentResource (..)
  -- * Smart constructor
  , mkTreatmentResource
  -- * Lenses
  , trId
  , trSizePercent
  , trCustomDeliveryConfiguration
  , trMessageConfiguration
  , trSchedule
  , trState
  , trTemplateConfiguration
  , trTreatmentDescription
  , trTreatmentName
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types.CampaignState as Types
import qualified Network.AWS.Pinpoint.Types.CustomDeliveryConfiguration as Types
import qualified Network.AWS.Pinpoint.Types.MessageConfiguration as Types
import qualified Network.AWS.Pinpoint.Types.Schedule as Types
import qualified Network.AWS.Pinpoint.Types.TemplateConfiguration as Types
import qualified Network.AWS.Prelude as Core

-- | Specifies the settings for a campaign treatment. A /treatment/ is a variation of a campaign that's used for A/B testing of a campaign.
--
-- /See:/ 'mkTreatmentResource' smart constructor.
data TreatmentResource = TreatmentResource'
  { id :: Core.Text
    -- ^ The unique identifier for the treatment.
  , sizePercent :: Core.Int
    -- ^ The allocated percentage of users (segment members) that the treatment is sent to.
  , customDeliveryConfiguration :: Core.Maybe Types.CustomDeliveryConfiguration
    -- ^ The delivery configuration settings for sending the treatment through a custom channel. This object is required if the MessageConfiguration object for the treatment specifies a CustomMessage object.
  , messageConfiguration :: Core.Maybe Types.MessageConfiguration
    -- ^ The message configuration settings for the treatment.
  , schedule :: Core.Maybe Types.Schedule
    -- ^ The schedule settings for the treatment.
  , state :: Core.Maybe Types.CampaignState
    -- ^ The current status of the treatment.
  , templateConfiguration :: Core.Maybe Types.TemplateConfiguration
    -- ^ The message template to use for the treatment.
  , treatmentDescription :: Core.Maybe Core.Text
    -- ^ The custom description of the treatment.
  , treatmentName :: Core.Maybe Core.Text
    -- ^ The custom name of the treatment.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TreatmentResource' value with any optional fields omitted.
mkTreatmentResource
    :: Core.Text -- ^ 'id'
    -> Core.Int -- ^ 'sizePercent'
    -> TreatmentResource
mkTreatmentResource id sizePercent
  = TreatmentResource'{id, sizePercent,
                       customDeliveryConfiguration = Core.Nothing,
                       messageConfiguration = Core.Nothing, schedule = Core.Nothing,
                       state = Core.Nothing, templateConfiguration = Core.Nothing,
                       treatmentDescription = Core.Nothing, treatmentName = Core.Nothing}

-- | The unique identifier for the treatment.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trId :: Lens.Lens' TreatmentResource Core.Text
trId = Lens.field @"id"
{-# INLINEABLE trId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The allocated percentage of users (segment members) that the treatment is sent to.
--
-- /Note:/ Consider using 'sizePercent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trSizePercent :: Lens.Lens' TreatmentResource Core.Int
trSizePercent = Lens.field @"sizePercent"
{-# INLINEABLE trSizePercent #-}
{-# DEPRECATED sizePercent "Use generic-lens or generic-optics with 'sizePercent' instead"  #-}

-- | The delivery configuration settings for sending the treatment through a custom channel. This object is required if the MessageConfiguration object for the treatment specifies a CustomMessage object.
--
-- /Note:/ Consider using 'customDeliveryConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trCustomDeliveryConfiguration :: Lens.Lens' TreatmentResource (Core.Maybe Types.CustomDeliveryConfiguration)
trCustomDeliveryConfiguration = Lens.field @"customDeliveryConfiguration"
{-# INLINEABLE trCustomDeliveryConfiguration #-}
{-# DEPRECATED customDeliveryConfiguration "Use generic-lens or generic-optics with 'customDeliveryConfiguration' instead"  #-}

-- | The message configuration settings for the treatment.
--
-- /Note:/ Consider using 'messageConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trMessageConfiguration :: Lens.Lens' TreatmentResource (Core.Maybe Types.MessageConfiguration)
trMessageConfiguration = Lens.field @"messageConfiguration"
{-# INLINEABLE trMessageConfiguration #-}
{-# DEPRECATED messageConfiguration "Use generic-lens or generic-optics with 'messageConfiguration' instead"  #-}

-- | The schedule settings for the treatment.
--
-- /Note:/ Consider using 'schedule' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trSchedule :: Lens.Lens' TreatmentResource (Core.Maybe Types.Schedule)
trSchedule = Lens.field @"schedule"
{-# INLINEABLE trSchedule #-}
{-# DEPRECATED schedule "Use generic-lens or generic-optics with 'schedule' instead"  #-}

-- | The current status of the treatment.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trState :: Lens.Lens' TreatmentResource (Core.Maybe Types.CampaignState)
trState = Lens.field @"state"
{-# INLINEABLE trState #-}
{-# DEPRECATED state "Use generic-lens or generic-optics with 'state' instead"  #-}

-- | The message template to use for the treatment.
--
-- /Note:/ Consider using 'templateConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trTemplateConfiguration :: Lens.Lens' TreatmentResource (Core.Maybe Types.TemplateConfiguration)
trTemplateConfiguration = Lens.field @"templateConfiguration"
{-# INLINEABLE trTemplateConfiguration #-}
{-# DEPRECATED templateConfiguration "Use generic-lens or generic-optics with 'templateConfiguration' instead"  #-}

-- | The custom description of the treatment.
--
-- /Note:/ Consider using 'treatmentDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trTreatmentDescription :: Lens.Lens' TreatmentResource (Core.Maybe Core.Text)
trTreatmentDescription = Lens.field @"treatmentDescription"
{-# INLINEABLE trTreatmentDescription #-}
{-# DEPRECATED treatmentDescription "Use generic-lens or generic-optics with 'treatmentDescription' instead"  #-}

-- | The custom name of the treatment.
--
-- /Note:/ Consider using 'treatmentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trTreatmentName :: Lens.Lens' TreatmentResource (Core.Maybe Core.Text)
trTreatmentName = Lens.field @"treatmentName"
{-# INLINEABLE trTreatmentName #-}
{-# DEPRECATED treatmentName "Use generic-lens or generic-optics with 'treatmentName' instead"  #-}

instance Core.FromJSON TreatmentResource where
        parseJSON
          = Core.withObject "TreatmentResource" Core.$
              \ x ->
                TreatmentResource' Core.<$>
                  (x Core..: "Id") Core.<*> x Core..: "SizePercent" Core.<*>
                    x Core..:? "CustomDeliveryConfiguration"
                    Core.<*> x Core..:? "MessageConfiguration"
                    Core.<*> x Core..:? "Schedule"
                    Core.<*> x Core..:? "State"
                    Core.<*> x Core..:? "TemplateConfiguration"
                    Core.<*> x Core..:? "TreatmentDescription"
                    Core.<*> x Core..:? "TreatmentName"
