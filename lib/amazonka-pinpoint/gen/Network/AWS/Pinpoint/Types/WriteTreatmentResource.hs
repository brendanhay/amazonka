{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.WriteTreatmentResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.WriteTreatmentResource
  ( WriteTreatmentResource (..),

    -- * Smart constructor
    mkWriteTreatmentResource,

    -- * Lenses
    wtrCustomDeliveryConfiguration,
    wtrSchedule,
    wtrTemplateConfiguration,
    wtrTreatmentName,
    wtrSizePercent,
    wtrTreatmentDescription,
    wtrMessageConfiguration,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.CustomDeliveryConfiguration
import Network.AWS.Pinpoint.Types.MessageConfiguration
import Network.AWS.Pinpoint.Types.Schedule
import Network.AWS.Pinpoint.Types.TemplateConfiguration
import qualified Network.AWS.Prelude as Lude

-- | Specifies the settings for a campaign treatment. A /treatment/ is a variation of a campaign that's used for A/B testing of a campaign.
--
-- /See:/ 'mkWriteTreatmentResource' smart constructor.
data WriteTreatmentResource = WriteTreatmentResource'
  { -- | The delivery configuration settings for sending the treatment through a custom channel. This object is required if the MessageConfiguration object for the treatment specifies a CustomMessage object.
    customDeliveryConfiguration :: Lude.Maybe CustomDeliveryConfiguration,
    -- | The schedule settings for the treatment.
    schedule :: Lude.Maybe Schedule,
    -- | The message template to use for the treatment.
    templateConfiguration :: Lude.Maybe TemplateConfiguration,
    -- | A custom name for the treatment.
    treatmentName :: Lude.Maybe Lude.Text,
    -- | The allocated percentage of users (segment members) to send the treatment to.
    sizePercent :: Lude.Int,
    -- | A custom description of the treatment.
    treatmentDescription :: Lude.Maybe Lude.Text,
    -- | The message configuration settings for the treatment.
    messageConfiguration :: Lude.Maybe MessageConfiguration
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'WriteTreatmentResource' with the minimum fields required to make a request.
--
-- * 'customDeliveryConfiguration' - The delivery configuration settings for sending the treatment through a custom channel. This object is required if the MessageConfiguration object for the treatment specifies a CustomMessage object.
-- * 'schedule' - The schedule settings for the treatment.
-- * 'templateConfiguration' - The message template to use for the treatment.
-- * 'treatmentName' - A custom name for the treatment.
-- * 'sizePercent' - The allocated percentage of users (segment members) to send the treatment to.
-- * 'treatmentDescription' - A custom description of the treatment.
-- * 'messageConfiguration' - The message configuration settings for the treatment.
mkWriteTreatmentResource ::
  -- | 'sizePercent'
  Lude.Int ->
  WriteTreatmentResource
mkWriteTreatmentResource pSizePercent_ =
  WriteTreatmentResource'
    { customDeliveryConfiguration =
        Lude.Nothing,
      schedule = Lude.Nothing,
      templateConfiguration = Lude.Nothing,
      treatmentName = Lude.Nothing,
      sizePercent = pSizePercent_,
      treatmentDescription = Lude.Nothing,
      messageConfiguration = Lude.Nothing
    }

-- | The delivery configuration settings for sending the treatment through a custom channel. This object is required if the MessageConfiguration object for the treatment specifies a CustomMessage object.
--
-- /Note:/ Consider using 'customDeliveryConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wtrCustomDeliveryConfiguration :: Lens.Lens' WriteTreatmentResource (Lude.Maybe CustomDeliveryConfiguration)
wtrCustomDeliveryConfiguration = Lens.lens (customDeliveryConfiguration :: WriteTreatmentResource -> Lude.Maybe CustomDeliveryConfiguration) (\s a -> s {customDeliveryConfiguration = a} :: WriteTreatmentResource)
{-# DEPRECATED wtrCustomDeliveryConfiguration "Use generic-lens or generic-optics with 'customDeliveryConfiguration' instead." #-}

-- | The schedule settings for the treatment.
--
-- /Note:/ Consider using 'schedule' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wtrSchedule :: Lens.Lens' WriteTreatmentResource (Lude.Maybe Schedule)
wtrSchedule = Lens.lens (schedule :: WriteTreatmentResource -> Lude.Maybe Schedule) (\s a -> s {schedule = a} :: WriteTreatmentResource)
{-# DEPRECATED wtrSchedule "Use generic-lens or generic-optics with 'schedule' instead." #-}

-- | The message template to use for the treatment.
--
-- /Note:/ Consider using 'templateConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wtrTemplateConfiguration :: Lens.Lens' WriteTreatmentResource (Lude.Maybe TemplateConfiguration)
wtrTemplateConfiguration = Lens.lens (templateConfiguration :: WriteTreatmentResource -> Lude.Maybe TemplateConfiguration) (\s a -> s {templateConfiguration = a} :: WriteTreatmentResource)
{-# DEPRECATED wtrTemplateConfiguration "Use generic-lens or generic-optics with 'templateConfiguration' instead." #-}

-- | A custom name for the treatment.
--
-- /Note:/ Consider using 'treatmentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wtrTreatmentName :: Lens.Lens' WriteTreatmentResource (Lude.Maybe Lude.Text)
wtrTreatmentName = Lens.lens (treatmentName :: WriteTreatmentResource -> Lude.Maybe Lude.Text) (\s a -> s {treatmentName = a} :: WriteTreatmentResource)
{-# DEPRECATED wtrTreatmentName "Use generic-lens or generic-optics with 'treatmentName' instead." #-}

-- | The allocated percentage of users (segment members) to send the treatment to.
--
-- /Note:/ Consider using 'sizePercent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wtrSizePercent :: Lens.Lens' WriteTreatmentResource Lude.Int
wtrSizePercent = Lens.lens (sizePercent :: WriteTreatmentResource -> Lude.Int) (\s a -> s {sizePercent = a} :: WriteTreatmentResource)
{-# DEPRECATED wtrSizePercent "Use generic-lens or generic-optics with 'sizePercent' instead." #-}

-- | A custom description of the treatment.
--
-- /Note:/ Consider using 'treatmentDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wtrTreatmentDescription :: Lens.Lens' WriteTreatmentResource (Lude.Maybe Lude.Text)
wtrTreatmentDescription = Lens.lens (treatmentDescription :: WriteTreatmentResource -> Lude.Maybe Lude.Text) (\s a -> s {treatmentDescription = a} :: WriteTreatmentResource)
{-# DEPRECATED wtrTreatmentDescription "Use generic-lens or generic-optics with 'treatmentDescription' instead." #-}

-- | The message configuration settings for the treatment.
--
-- /Note:/ Consider using 'messageConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wtrMessageConfiguration :: Lens.Lens' WriteTreatmentResource (Lude.Maybe MessageConfiguration)
wtrMessageConfiguration = Lens.lens (messageConfiguration :: WriteTreatmentResource -> Lude.Maybe MessageConfiguration) (\s a -> s {messageConfiguration = a} :: WriteTreatmentResource)
{-# DEPRECATED wtrMessageConfiguration "Use generic-lens or generic-optics with 'messageConfiguration' instead." #-}

instance Lude.ToJSON WriteTreatmentResource where
  toJSON WriteTreatmentResource' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("CustomDeliveryConfiguration" Lude..=)
              Lude.<$> customDeliveryConfiguration,
            ("Schedule" Lude..=) Lude.<$> schedule,
            ("TemplateConfiguration" Lude..=) Lude.<$> templateConfiguration,
            ("TreatmentName" Lude..=) Lude.<$> treatmentName,
            Lude.Just ("SizePercent" Lude..= sizePercent),
            ("TreatmentDescription" Lude..=) Lude.<$> treatmentDescription,
            ("MessageConfiguration" Lude..=) Lude.<$> messageConfiguration
          ]
      )
