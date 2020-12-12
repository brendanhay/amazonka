{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.WriteCampaignRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.WriteCampaignRequest
  ( WriteCampaignRequest (..),

    -- * Smart constructor
    mkWriteCampaignRequest,

    -- * Lenses
    wcrCustomDeliveryConfiguration,
    wcrSchedule,
    wcrTemplateConfiguration,
    wcrHook,
    wcrTreatmentName,
    wcrLimits,
    wcrIsPaused,
    wcrName,
    wcrHoldoutPercent,
    wcrTreatmentDescription,
    wcrMessageConfiguration,
    wcrDescription,
    wcrSegmentId,
    wcrAdditionalTreatments,
    wcrTags,
    wcrSegmentVersion,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.CampaignHook
import Network.AWS.Pinpoint.Types.CampaignLimits
import Network.AWS.Pinpoint.Types.CustomDeliveryConfiguration
import Network.AWS.Pinpoint.Types.MessageConfiguration
import Network.AWS.Pinpoint.Types.Schedule
import Network.AWS.Pinpoint.Types.TemplateConfiguration
import Network.AWS.Pinpoint.Types.WriteTreatmentResource
import qualified Network.AWS.Prelude as Lude

-- | Specifies the configuration and other settings for a campaign.
--
-- /See:/ 'mkWriteCampaignRequest' smart constructor.
data WriteCampaignRequest = WriteCampaignRequest'
  { customDeliveryConfiguration ::
      Lude.Maybe CustomDeliveryConfiguration,
    schedule :: Lude.Maybe Schedule,
    templateConfiguration ::
      Lude.Maybe TemplateConfiguration,
    hook :: Lude.Maybe CampaignHook,
    treatmentName :: Lude.Maybe Lude.Text,
    limits :: Lude.Maybe CampaignLimits,
    isPaused :: Lude.Maybe Lude.Bool,
    name :: Lude.Maybe Lude.Text,
    holdoutPercent :: Lude.Maybe Lude.Int,
    treatmentDescription :: Lude.Maybe Lude.Text,
    messageConfiguration ::
      Lude.Maybe MessageConfiguration,
    description :: Lude.Maybe Lude.Text,
    segmentId :: Lude.Maybe Lude.Text,
    additionalTreatments ::
      Lude.Maybe [WriteTreatmentResource],
    tags ::
      Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    segmentVersion :: Lude.Maybe Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'WriteCampaignRequest' with the minimum fields required to make a request.
--
-- * 'additionalTreatments' - An array of requests that defines additional treatments for the campaign, in addition to the default treatment for the campaign.
-- * 'customDeliveryConfiguration' - The delivery configuration settings for sending the campaign through a custom channel. This object is required if the MessageConfiguration object for the campaign specifies a CustomMessage object.
-- * 'description' - A custom description of the campaign.
-- * 'holdoutPercent' - The allocated percentage of users (segment members) who shouldn't receive messages from the campaign.
-- * 'hook' - The settings for the AWS Lambda function to invoke as a code hook for the campaign. You can use this hook to customize the segment that's used by the campaign.
-- * 'isPaused' - Specifies whether to pause the campaign. A paused campaign doesn't run unless you resume it by changing this value to false.
-- * 'limits' - The messaging limits for the campaign.
-- * 'messageConfiguration' - The message configuration settings for the campaign.
-- * 'name' - A custom name for the campaign.
-- * 'schedule' - The schedule settings for the campaign.
-- * 'segmentId' - The unique identifier for the segment to associate with the campaign.
-- * 'segmentVersion' - The version of the segment to associate with the campaign.
-- * 'tags' - A string-to-string map of key-value pairs that defines the tags to associate with the campaign. Each tag consists of a required tag key and an associated tag value.
-- * 'templateConfiguration' - The message template to use for the campaign.
-- * 'treatmentDescription' - A custom description of the default treatment for the campaign.
-- * 'treatmentName' - A custom name of the default treatment for the campaign, if the campaign has multiple treatments. A /treatment/ is a variation of a campaign that's used for A/B testing.
mkWriteCampaignRequest ::
  WriteCampaignRequest
mkWriteCampaignRequest =
  WriteCampaignRequest'
    { customDeliveryConfiguration = Lude.Nothing,
      schedule = Lude.Nothing,
      templateConfiguration = Lude.Nothing,
      hook = Lude.Nothing,
      treatmentName = Lude.Nothing,
      limits = Lude.Nothing,
      isPaused = Lude.Nothing,
      name = Lude.Nothing,
      holdoutPercent = Lude.Nothing,
      treatmentDescription = Lude.Nothing,
      messageConfiguration = Lude.Nothing,
      description = Lude.Nothing,
      segmentId = Lude.Nothing,
      additionalTreatments = Lude.Nothing,
      tags = Lude.Nothing,
      segmentVersion = Lude.Nothing
    }

-- | The delivery configuration settings for sending the campaign through a custom channel. This object is required if the MessageConfiguration object for the campaign specifies a CustomMessage object.
--
-- /Note:/ Consider using 'customDeliveryConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wcrCustomDeliveryConfiguration :: Lens.Lens' WriteCampaignRequest (Lude.Maybe CustomDeliveryConfiguration)
wcrCustomDeliveryConfiguration = Lens.lens (customDeliveryConfiguration :: WriteCampaignRequest -> Lude.Maybe CustomDeliveryConfiguration) (\s a -> s {customDeliveryConfiguration = a} :: WriteCampaignRequest)
{-# DEPRECATED wcrCustomDeliveryConfiguration "Use generic-lens or generic-optics with 'customDeliveryConfiguration' instead." #-}

-- | The schedule settings for the campaign.
--
-- /Note:/ Consider using 'schedule' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wcrSchedule :: Lens.Lens' WriteCampaignRequest (Lude.Maybe Schedule)
wcrSchedule = Lens.lens (schedule :: WriteCampaignRequest -> Lude.Maybe Schedule) (\s a -> s {schedule = a} :: WriteCampaignRequest)
{-# DEPRECATED wcrSchedule "Use generic-lens or generic-optics with 'schedule' instead." #-}

-- | The message template to use for the campaign.
--
-- /Note:/ Consider using 'templateConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wcrTemplateConfiguration :: Lens.Lens' WriteCampaignRequest (Lude.Maybe TemplateConfiguration)
wcrTemplateConfiguration = Lens.lens (templateConfiguration :: WriteCampaignRequest -> Lude.Maybe TemplateConfiguration) (\s a -> s {templateConfiguration = a} :: WriteCampaignRequest)
{-# DEPRECATED wcrTemplateConfiguration "Use generic-lens or generic-optics with 'templateConfiguration' instead." #-}

-- | The settings for the AWS Lambda function to invoke as a code hook for the campaign. You can use this hook to customize the segment that's used by the campaign.
--
-- /Note:/ Consider using 'hook' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wcrHook :: Lens.Lens' WriteCampaignRequest (Lude.Maybe CampaignHook)
wcrHook = Lens.lens (hook :: WriteCampaignRequest -> Lude.Maybe CampaignHook) (\s a -> s {hook = a} :: WriteCampaignRequest)
{-# DEPRECATED wcrHook "Use generic-lens or generic-optics with 'hook' instead." #-}

-- | A custom name of the default treatment for the campaign, if the campaign has multiple treatments. A /treatment/ is a variation of a campaign that's used for A/B testing.
--
-- /Note:/ Consider using 'treatmentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wcrTreatmentName :: Lens.Lens' WriteCampaignRequest (Lude.Maybe Lude.Text)
wcrTreatmentName = Lens.lens (treatmentName :: WriteCampaignRequest -> Lude.Maybe Lude.Text) (\s a -> s {treatmentName = a} :: WriteCampaignRequest)
{-# DEPRECATED wcrTreatmentName "Use generic-lens or generic-optics with 'treatmentName' instead." #-}

-- | The messaging limits for the campaign.
--
-- /Note:/ Consider using 'limits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wcrLimits :: Lens.Lens' WriteCampaignRequest (Lude.Maybe CampaignLimits)
wcrLimits = Lens.lens (limits :: WriteCampaignRequest -> Lude.Maybe CampaignLimits) (\s a -> s {limits = a} :: WriteCampaignRequest)
{-# DEPRECATED wcrLimits "Use generic-lens or generic-optics with 'limits' instead." #-}

-- | Specifies whether to pause the campaign. A paused campaign doesn't run unless you resume it by changing this value to false.
--
-- /Note:/ Consider using 'isPaused' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wcrIsPaused :: Lens.Lens' WriteCampaignRequest (Lude.Maybe Lude.Bool)
wcrIsPaused = Lens.lens (isPaused :: WriteCampaignRequest -> Lude.Maybe Lude.Bool) (\s a -> s {isPaused = a} :: WriteCampaignRequest)
{-# DEPRECATED wcrIsPaused "Use generic-lens or generic-optics with 'isPaused' instead." #-}

-- | A custom name for the campaign.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wcrName :: Lens.Lens' WriteCampaignRequest (Lude.Maybe Lude.Text)
wcrName = Lens.lens (name :: WriteCampaignRequest -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: WriteCampaignRequest)
{-# DEPRECATED wcrName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The allocated percentage of users (segment members) who shouldn't receive messages from the campaign.
--
-- /Note:/ Consider using 'holdoutPercent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wcrHoldoutPercent :: Lens.Lens' WriteCampaignRequest (Lude.Maybe Lude.Int)
wcrHoldoutPercent = Lens.lens (holdoutPercent :: WriteCampaignRequest -> Lude.Maybe Lude.Int) (\s a -> s {holdoutPercent = a} :: WriteCampaignRequest)
{-# DEPRECATED wcrHoldoutPercent "Use generic-lens or generic-optics with 'holdoutPercent' instead." #-}

-- | A custom description of the default treatment for the campaign.
--
-- /Note:/ Consider using 'treatmentDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wcrTreatmentDescription :: Lens.Lens' WriteCampaignRequest (Lude.Maybe Lude.Text)
wcrTreatmentDescription = Lens.lens (treatmentDescription :: WriteCampaignRequest -> Lude.Maybe Lude.Text) (\s a -> s {treatmentDescription = a} :: WriteCampaignRequest)
{-# DEPRECATED wcrTreatmentDescription "Use generic-lens or generic-optics with 'treatmentDescription' instead." #-}

-- | The message configuration settings for the campaign.
--
-- /Note:/ Consider using 'messageConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wcrMessageConfiguration :: Lens.Lens' WriteCampaignRequest (Lude.Maybe MessageConfiguration)
wcrMessageConfiguration = Lens.lens (messageConfiguration :: WriteCampaignRequest -> Lude.Maybe MessageConfiguration) (\s a -> s {messageConfiguration = a} :: WriteCampaignRequest)
{-# DEPRECATED wcrMessageConfiguration "Use generic-lens or generic-optics with 'messageConfiguration' instead." #-}

-- | A custom description of the campaign.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wcrDescription :: Lens.Lens' WriteCampaignRequest (Lude.Maybe Lude.Text)
wcrDescription = Lens.lens (description :: WriteCampaignRequest -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: WriteCampaignRequest)
{-# DEPRECATED wcrDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The unique identifier for the segment to associate with the campaign.
--
-- /Note:/ Consider using 'segmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wcrSegmentId :: Lens.Lens' WriteCampaignRequest (Lude.Maybe Lude.Text)
wcrSegmentId = Lens.lens (segmentId :: WriteCampaignRequest -> Lude.Maybe Lude.Text) (\s a -> s {segmentId = a} :: WriteCampaignRequest)
{-# DEPRECATED wcrSegmentId "Use generic-lens or generic-optics with 'segmentId' instead." #-}

-- | An array of requests that defines additional treatments for the campaign, in addition to the default treatment for the campaign.
--
-- /Note:/ Consider using 'additionalTreatments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wcrAdditionalTreatments :: Lens.Lens' WriteCampaignRequest (Lude.Maybe [WriteTreatmentResource])
wcrAdditionalTreatments = Lens.lens (additionalTreatments :: WriteCampaignRequest -> Lude.Maybe [WriteTreatmentResource]) (\s a -> s {additionalTreatments = a} :: WriteCampaignRequest)
{-# DEPRECATED wcrAdditionalTreatments "Use generic-lens or generic-optics with 'additionalTreatments' instead." #-}

-- | A string-to-string map of key-value pairs that defines the tags to associate with the campaign. Each tag consists of a required tag key and an associated tag value.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wcrTags :: Lens.Lens' WriteCampaignRequest (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
wcrTags = Lens.lens (tags :: WriteCampaignRequest -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: WriteCampaignRequest)
{-# DEPRECATED wcrTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The version of the segment to associate with the campaign.
--
-- /Note:/ Consider using 'segmentVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wcrSegmentVersion :: Lens.Lens' WriteCampaignRequest (Lude.Maybe Lude.Int)
wcrSegmentVersion = Lens.lens (segmentVersion :: WriteCampaignRequest -> Lude.Maybe Lude.Int) (\s a -> s {segmentVersion = a} :: WriteCampaignRequest)
{-# DEPRECATED wcrSegmentVersion "Use generic-lens or generic-optics with 'segmentVersion' instead." #-}

instance Lude.ToJSON WriteCampaignRequest where
  toJSON WriteCampaignRequest' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("CustomDeliveryConfiguration" Lude..=)
              Lude.<$> customDeliveryConfiguration,
            ("Schedule" Lude..=) Lude.<$> schedule,
            ("TemplateConfiguration" Lude..=) Lude.<$> templateConfiguration,
            ("Hook" Lude..=) Lude.<$> hook,
            ("TreatmentName" Lude..=) Lude.<$> treatmentName,
            ("Limits" Lude..=) Lude.<$> limits,
            ("IsPaused" Lude..=) Lude.<$> isPaused,
            ("Name" Lude..=) Lude.<$> name,
            ("HoldoutPercent" Lude..=) Lude.<$> holdoutPercent,
            ("TreatmentDescription" Lude..=) Lude.<$> treatmentDescription,
            ("MessageConfiguration" Lude..=) Lude.<$> messageConfiguration,
            ("Description" Lude..=) Lude.<$> description,
            ("SegmentId" Lude..=) Lude.<$> segmentId,
            ("AdditionalTreatments" Lude..=) Lude.<$> additionalTreatments,
            ("tags" Lude..=) Lude.<$> tags,
            ("SegmentVersion" Lude..=) Lude.<$> segmentVersion
          ]
      )
