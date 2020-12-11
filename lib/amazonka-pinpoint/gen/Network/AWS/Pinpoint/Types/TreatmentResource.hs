-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.TreatmentResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.TreatmentResource
  ( TreatmentResource (..),

    -- * Smart constructor
    mkTreatmentResource,

    -- * Lenses
    trCustomDeliveryConfiguration,
    trState,
    trSchedule,
    trTemplateConfiguration,
    trTreatmentName,
    trTreatmentDescription,
    trMessageConfiguration,
    trId,
    trSizePercent,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.CampaignState
import Network.AWS.Pinpoint.Types.CustomDeliveryConfiguration
import Network.AWS.Pinpoint.Types.MessageConfiguration
import Network.AWS.Pinpoint.Types.Schedule
import Network.AWS.Pinpoint.Types.TemplateConfiguration
import qualified Network.AWS.Prelude as Lude

-- | Specifies the settings for a campaign treatment. A /treatment/ is a variation of a campaign that's used for A/B testing of a campaign.
--
-- /See:/ 'mkTreatmentResource' smart constructor.
data TreatmentResource = TreatmentResource'
  { customDeliveryConfiguration ::
      Lude.Maybe CustomDeliveryConfiguration,
    state :: Lude.Maybe CampaignState,
    schedule :: Lude.Maybe Schedule,
    templateConfiguration ::
      Lude.Maybe TemplateConfiguration,
    treatmentName :: Lude.Maybe Lude.Text,
    treatmentDescription :: Lude.Maybe Lude.Text,
    messageConfiguration :: Lude.Maybe MessageConfiguration,
    id :: Lude.Text,
    sizePercent :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TreatmentResource' with the minimum fields required to make a request.
--
-- * 'customDeliveryConfiguration' - The delivery configuration settings for sending the treatment through a custom channel. This object is required if the MessageConfiguration object for the treatment specifies a CustomMessage object.
-- * 'id' - The unique identifier for the treatment.
-- * 'messageConfiguration' - The message configuration settings for the treatment.
-- * 'schedule' - The schedule settings for the treatment.
-- * 'sizePercent' - The allocated percentage of users (segment members) that the treatment is sent to.
-- * 'state' - The current status of the treatment.
-- * 'templateConfiguration' - The message template to use for the treatment.
-- * 'treatmentDescription' - The custom description of the treatment.
-- * 'treatmentName' - The custom name of the treatment.
mkTreatmentResource ::
  -- | 'id'
  Lude.Text ->
  -- | 'sizePercent'
  Lude.Int ->
  TreatmentResource
mkTreatmentResource pId_ pSizePercent_ =
  TreatmentResource'
    { customDeliveryConfiguration = Lude.Nothing,
      state = Lude.Nothing,
      schedule = Lude.Nothing,
      templateConfiguration = Lude.Nothing,
      treatmentName = Lude.Nothing,
      treatmentDescription = Lude.Nothing,
      messageConfiguration = Lude.Nothing,
      id = pId_,
      sizePercent = pSizePercent_
    }

-- | The delivery configuration settings for sending the treatment through a custom channel. This object is required if the MessageConfiguration object for the treatment specifies a CustomMessage object.
--
-- /Note:/ Consider using 'customDeliveryConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trCustomDeliveryConfiguration :: Lens.Lens' TreatmentResource (Lude.Maybe CustomDeliveryConfiguration)
trCustomDeliveryConfiguration = Lens.lens (customDeliveryConfiguration :: TreatmentResource -> Lude.Maybe CustomDeliveryConfiguration) (\s a -> s {customDeliveryConfiguration = a} :: TreatmentResource)
{-# DEPRECATED trCustomDeliveryConfiguration "Use generic-lens or generic-optics with 'customDeliveryConfiguration' instead." #-}

-- | The current status of the treatment.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trState :: Lens.Lens' TreatmentResource (Lude.Maybe CampaignState)
trState = Lens.lens (state :: TreatmentResource -> Lude.Maybe CampaignState) (\s a -> s {state = a} :: TreatmentResource)
{-# DEPRECATED trState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The schedule settings for the treatment.
--
-- /Note:/ Consider using 'schedule' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trSchedule :: Lens.Lens' TreatmentResource (Lude.Maybe Schedule)
trSchedule = Lens.lens (schedule :: TreatmentResource -> Lude.Maybe Schedule) (\s a -> s {schedule = a} :: TreatmentResource)
{-# DEPRECATED trSchedule "Use generic-lens or generic-optics with 'schedule' instead." #-}

-- | The message template to use for the treatment.
--
-- /Note:/ Consider using 'templateConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trTemplateConfiguration :: Lens.Lens' TreatmentResource (Lude.Maybe TemplateConfiguration)
trTemplateConfiguration = Lens.lens (templateConfiguration :: TreatmentResource -> Lude.Maybe TemplateConfiguration) (\s a -> s {templateConfiguration = a} :: TreatmentResource)
{-# DEPRECATED trTemplateConfiguration "Use generic-lens or generic-optics with 'templateConfiguration' instead." #-}

-- | The custom name of the treatment.
--
-- /Note:/ Consider using 'treatmentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trTreatmentName :: Lens.Lens' TreatmentResource (Lude.Maybe Lude.Text)
trTreatmentName = Lens.lens (treatmentName :: TreatmentResource -> Lude.Maybe Lude.Text) (\s a -> s {treatmentName = a} :: TreatmentResource)
{-# DEPRECATED trTreatmentName "Use generic-lens or generic-optics with 'treatmentName' instead." #-}

-- | The custom description of the treatment.
--
-- /Note:/ Consider using 'treatmentDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trTreatmentDescription :: Lens.Lens' TreatmentResource (Lude.Maybe Lude.Text)
trTreatmentDescription = Lens.lens (treatmentDescription :: TreatmentResource -> Lude.Maybe Lude.Text) (\s a -> s {treatmentDescription = a} :: TreatmentResource)
{-# DEPRECATED trTreatmentDescription "Use generic-lens or generic-optics with 'treatmentDescription' instead." #-}

-- | The message configuration settings for the treatment.
--
-- /Note:/ Consider using 'messageConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trMessageConfiguration :: Lens.Lens' TreatmentResource (Lude.Maybe MessageConfiguration)
trMessageConfiguration = Lens.lens (messageConfiguration :: TreatmentResource -> Lude.Maybe MessageConfiguration) (\s a -> s {messageConfiguration = a} :: TreatmentResource)
{-# DEPRECATED trMessageConfiguration "Use generic-lens or generic-optics with 'messageConfiguration' instead." #-}

-- | The unique identifier for the treatment.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trId :: Lens.Lens' TreatmentResource Lude.Text
trId = Lens.lens (id :: TreatmentResource -> Lude.Text) (\s a -> s {id = a} :: TreatmentResource)
{-# DEPRECATED trId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The allocated percentage of users (segment members) that the treatment is sent to.
--
-- /Note:/ Consider using 'sizePercent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trSizePercent :: Lens.Lens' TreatmentResource Lude.Int
trSizePercent = Lens.lens (sizePercent :: TreatmentResource -> Lude.Int) (\s a -> s {sizePercent = a} :: TreatmentResource)
{-# DEPRECATED trSizePercent "Use generic-lens or generic-optics with 'sizePercent' instead." #-}

instance Lude.FromJSON TreatmentResource where
  parseJSON =
    Lude.withObject
      "TreatmentResource"
      ( \x ->
          TreatmentResource'
            Lude.<$> (x Lude..:? "CustomDeliveryConfiguration")
            Lude.<*> (x Lude..:? "State")
            Lude.<*> (x Lude..:? "Schedule")
            Lude.<*> (x Lude..:? "TemplateConfiguration")
            Lude.<*> (x Lude..:? "TreatmentName")
            Lude.<*> (x Lude..:? "TreatmentDescription")
            Lude.<*> (x Lude..:? "MessageConfiguration")
            Lude.<*> (x Lude..: "Id")
            Lude.<*> (x Lude..: "SizePercent")
      )
