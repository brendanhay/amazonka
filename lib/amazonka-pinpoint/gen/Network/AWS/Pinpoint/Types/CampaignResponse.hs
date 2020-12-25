{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.CampaignResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.CampaignResponse
  ( CampaignResponse (..),

    -- * Smart constructor
    mkCampaignResponse,

    -- * Lenses
    crLastModifiedDate,
    crCreationDate,
    crSegmentId,
    crSegmentVersion,
    crId,
    crArn,
    crApplicationId,
    crAdditionalTreatments,
    crCustomDeliveryConfiguration,
    crDefaultState,
    crDescription,
    crHoldoutPercent,
    crHook,
    crIsPaused,
    crLimits,
    crMessageConfiguration,
    crName,
    crSchedule,
    crState,
    crTemplateConfiguration,
    crTreatmentDescription,
    crTreatmentName,
    crVersion,
    crTags,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types.CampaignHook as Types
import qualified Network.AWS.Pinpoint.Types.CampaignLimits as Types
import qualified Network.AWS.Pinpoint.Types.CampaignState as Types
import qualified Network.AWS.Pinpoint.Types.CustomDeliveryConfiguration as Types
import qualified Network.AWS.Pinpoint.Types.MessageConfiguration as Types
import qualified Network.AWS.Pinpoint.Types.Schedule as Types
import qualified Network.AWS.Pinpoint.Types.TemplateConfiguration as Types
import qualified Network.AWS.Pinpoint.Types.TreatmentResource as Types
import qualified Network.AWS.Prelude as Core

-- | Provides information about the status, configuration, and other settings for a campaign.
--
-- /See:/ 'mkCampaignResponse' smart constructor.
data CampaignResponse = CampaignResponse'
  { -- | The date, in ISO 8601 format, when the campaign was last modified.
    lastModifiedDate :: Core.Text,
    -- | The date, in ISO 8601 format, when the campaign was created.
    creationDate :: Core.Text,
    -- | The unique identifier for the segment that's associated with the campaign.
    segmentId :: Core.Text,
    -- | The version number of the segment that's associated with the campaign.
    segmentVersion :: Core.Int,
    -- | The unique identifier for the campaign.
    id :: Core.Text,
    -- | The Amazon Resource Name (ARN) of the campaign.
    arn :: Core.Text,
    -- | The unique identifier for the application that the campaign applies to.
    applicationId :: Core.Text,
    -- | An array of responses, one for each treatment that you defined for the campaign, in addition to the default treatment.
    additionalTreatments :: Core.Maybe [Types.TreatmentResource],
    -- | The delivery configuration settings for sending the campaign through a custom channel.
    customDeliveryConfiguration :: Core.Maybe Types.CustomDeliveryConfiguration,
    -- | The current status of the campaign's default treatment. This value exists only for campaigns that have more than one treatment.
    defaultState :: Core.Maybe Types.CampaignState,
    -- | The custom description of the campaign.
    description :: Core.Maybe Core.Text,
    -- | The allocated percentage of users (segment members) who shouldn't receive messages from the campaign.
    holdoutPercent :: Core.Maybe Core.Int,
    -- | The settings for the AWS Lambda function to use as a code hook for the campaign. You can use this hook to customize the segment that's used by the campaign.
    hook :: Core.Maybe Types.CampaignHook,
    -- | Specifies whether the campaign is paused. A paused campaign doesn't run unless you resume it by changing this value to false.
    isPaused :: Core.Maybe Core.Bool,
    -- | The messaging limits for the campaign.
    limits :: Core.Maybe Types.CampaignLimits,
    -- | The message configuration settings for the campaign.
    messageConfiguration :: Core.Maybe Types.MessageConfiguration,
    -- | The name of the campaign.
    name :: Core.Maybe Core.Text,
    -- | The schedule settings for the campaign.
    schedule :: Core.Maybe Types.Schedule,
    -- | The current status of the campaign.
    state :: Core.Maybe Types.CampaignState,
    -- | The message template that’s used for the campaign.
    templateConfiguration :: Core.Maybe Types.TemplateConfiguration,
    -- | The custom description of the default treatment for the campaign.
    treatmentDescription :: Core.Maybe Core.Text,
    -- | The custom name of the default treatment for the campaign, if the campaign has multiple treatments. A /treatment/ is a variation of a campaign that's used for A/B testing.
    treatmentName :: Core.Maybe Core.Text,
    -- | The version number of the campaign.
    version :: Core.Maybe Core.Int,
    -- | A string-to-string map of key-value pairs that identifies the tags that are associated with the campaign. Each tag consists of a required tag key and an associated tag value.
    tags :: Core.Maybe (Core.HashMap Core.Text Core.Text)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CampaignResponse' value with any optional fields omitted.
mkCampaignResponse ::
  -- | 'lastModifiedDate'
  Core.Text ->
  -- | 'creationDate'
  Core.Text ->
  -- | 'segmentId'
  Core.Text ->
  -- | 'segmentVersion'
  Core.Int ->
  -- | 'id'
  Core.Text ->
  -- | 'arn'
  Core.Text ->
  -- | 'applicationId'
  Core.Text ->
  CampaignResponse
mkCampaignResponse
  lastModifiedDate
  creationDate
  segmentId
  segmentVersion
  id
  arn
  applicationId =
    CampaignResponse'
      { lastModifiedDate,
        creationDate,
        segmentId,
        segmentVersion,
        id,
        arn,
        applicationId,
        additionalTreatments = Core.Nothing,
        customDeliveryConfiguration = Core.Nothing,
        defaultState = Core.Nothing,
        description = Core.Nothing,
        holdoutPercent = Core.Nothing,
        hook = Core.Nothing,
        isPaused = Core.Nothing,
        limits = Core.Nothing,
        messageConfiguration = Core.Nothing,
        name = Core.Nothing,
        schedule = Core.Nothing,
        state = Core.Nothing,
        templateConfiguration = Core.Nothing,
        treatmentDescription = Core.Nothing,
        treatmentName = Core.Nothing,
        version = Core.Nothing,
        tags = Core.Nothing
      }

-- | The date, in ISO 8601 format, when the campaign was last modified.
--
-- /Note:/ Consider using 'lastModifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crLastModifiedDate :: Lens.Lens' CampaignResponse Core.Text
crLastModifiedDate = Lens.field @"lastModifiedDate"
{-# DEPRECATED crLastModifiedDate "Use generic-lens or generic-optics with 'lastModifiedDate' instead." #-}

-- | The date, in ISO 8601 format, when the campaign was created.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crCreationDate :: Lens.Lens' CampaignResponse Core.Text
crCreationDate = Lens.field @"creationDate"
{-# DEPRECATED crCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

-- | The unique identifier for the segment that's associated with the campaign.
--
-- /Note:/ Consider using 'segmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crSegmentId :: Lens.Lens' CampaignResponse Core.Text
crSegmentId = Lens.field @"segmentId"
{-# DEPRECATED crSegmentId "Use generic-lens or generic-optics with 'segmentId' instead." #-}

-- | The version number of the segment that's associated with the campaign.
--
-- /Note:/ Consider using 'segmentVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crSegmentVersion :: Lens.Lens' CampaignResponse Core.Int
crSegmentVersion = Lens.field @"segmentVersion"
{-# DEPRECATED crSegmentVersion "Use generic-lens or generic-optics with 'segmentVersion' instead." #-}

-- | The unique identifier for the campaign.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crId :: Lens.Lens' CampaignResponse Core.Text
crId = Lens.field @"id"
{-# DEPRECATED crId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The Amazon Resource Name (ARN) of the campaign.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crArn :: Lens.Lens' CampaignResponse Core.Text
crArn = Lens.field @"arn"
{-# DEPRECATED crArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The unique identifier for the application that the campaign applies to.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crApplicationId :: Lens.Lens' CampaignResponse Core.Text
crApplicationId = Lens.field @"applicationId"
{-# DEPRECATED crApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

-- | An array of responses, one for each treatment that you defined for the campaign, in addition to the default treatment.
--
-- /Note:/ Consider using 'additionalTreatments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crAdditionalTreatments :: Lens.Lens' CampaignResponse (Core.Maybe [Types.TreatmentResource])
crAdditionalTreatments = Lens.field @"additionalTreatments"
{-# DEPRECATED crAdditionalTreatments "Use generic-lens or generic-optics with 'additionalTreatments' instead." #-}

-- | The delivery configuration settings for sending the campaign through a custom channel.
--
-- /Note:/ Consider using 'customDeliveryConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crCustomDeliveryConfiguration :: Lens.Lens' CampaignResponse (Core.Maybe Types.CustomDeliveryConfiguration)
crCustomDeliveryConfiguration = Lens.field @"customDeliveryConfiguration"
{-# DEPRECATED crCustomDeliveryConfiguration "Use generic-lens or generic-optics with 'customDeliveryConfiguration' instead." #-}

-- | The current status of the campaign's default treatment. This value exists only for campaigns that have more than one treatment.
--
-- /Note:/ Consider using 'defaultState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crDefaultState :: Lens.Lens' CampaignResponse (Core.Maybe Types.CampaignState)
crDefaultState = Lens.field @"defaultState"
{-# DEPRECATED crDefaultState "Use generic-lens or generic-optics with 'defaultState' instead." #-}

-- | The custom description of the campaign.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crDescription :: Lens.Lens' CampaignResponse (Core.Maybe Core.Text)
crDescription = Lens.field @"description"
{-# DEPRECATED crDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The allocated percentage of users (segment members) who shouldn't receive messages from the campaign.
--
-- /Note:/ Consider using 'holdoutPercent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crHoldoutPercent :: Lens.Lens' CampaignResponse (Core.Maybe Core.Int)
crHoldoutPercent = Lens.field @"holdoutPercent"
{-# DEPRECATED crHoldoutPercent "Use generic-lens or generic-optics with 'holdoutPercent' instead." #-}

-- | The settings for the AWS Lambda function to use as a code hook for the campaign. You can use this hook to customize the segment that's used by the campaign.
--
-- /Note:/ Consider using 'hook' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crHook :: Lens.Lens' CampaignResponse (Core.Maybe Types.CampaignHook)
crHook = Lens.field @"hook"
{-# DEPRECATED crHook "Use generic-lens or generic-optics with 'hook' instead." #-}

-- | Specifies whether the campaign is paused. A paused campaign doesn't run unless you resume it by changing this value to false.
--
-- /Note:/ Consider using 'isPaused' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crIsPaused :: Lens.Lens' CampaignResponse (Core.Maybe Core.Bool)
crIsPaused = Lens.field @"isPaused"
{-# DEPRECATED crIsPaused "Use generic-lens or generic-optics with 'isPaused' instead." #-}

-- | The messaging limits for the campaign.
--
-- /Note:/ Consider using 'limits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crLimits :: Lens.Lens' CampaignResponse (Core.Maybe Types.CampaignLimits)
crLimits = Lens.field @"limits"
{-# DEPRECATED crLimits "Use generic-lens or generic-optics with 'limits' instead." #-}

-- | The message configuration settings for the campaign.
--
-- /Note:/ Consider using 'messageConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crMessageConfiguration :: Lens.Lens' CampaignResponse (Core.Maybe Types.MessageConfiguration)
crMessageConfiguration = Lens.field @"messageConfiguration"
{-# DEPRECATED crMessageConfiguration "Use generic-lens or generic-optics with 'messageConfiguration' instead." #-}

-- | The name of the campaign.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crName :: Lens.Lens' CampaignResponse (Core.Maybe Core.Text)
crName = Lens.field @"name"
{-# DEPRECATED crName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The schedule settings for the campaign.
--
-- /Note:/ Consider using 'schedule' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crSchedule :: Lens.Lens' CampaignResponse (Core.Maybe Types.Schedule)
crSchedule = Lens.field @"schedule"
{-# DEPRECATED crSchedule "Use generic-lens or generic-optics with 'schedule' instead." #-}

-- | The current status of the campaign.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crState :: Lens.Lens' CampaignResponse (Core.Maybe Types.CampaignState)
crState = Lens.field @"state"
{-# DEPRECATED crState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The message template that’s used for the campaign.
--
-- /Note:/ Consider using 'templateConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crTemplateConfiguration :: Lens.Lens' CampaignResponse (Core.Maybe Types.TemplateConfiguration)
crTemplateConfiguration = Lens.field @"templateConfiguration"
{-# DEPRECATED crTemplateConfiguration "Use generic-lens or generic-optics with 'templateConfiguration' instead." #-}

-- | The custom description of the default treatment for the campaign.
--
-- /Note:/ Consider using 'treatmentDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crTreatmentDescription :: Lens.Lens' CampaignResponse (Core.Maybe Core.Text)
crTreatmentDescription = Lens.field @"treatmentDescription"
{-# DEPRECATED crTreatmentDescription "Use generic-lens or generic-optics with 'treatmentDescription' instead." #-}

-- | The custom name of the default treatment for the campaign, if the campaign has multiple treatments. A /treatment/ is a variation of a campaign that's used for A/B testing.
--
-- /Note:/ Consider using 'treatmentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crTreatmentName :: Lens.Lens' CampaignResponse (Core.Maybe Core.Text)
crTreatmentName = Lens.field @"treatmentName"
{-# DEPRECATED crTreatmentName "Use generic-lens or generic-optics with 'treatmentName' instead." #-}

-- | The version number of the campaign.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crVersion :: Lens.Lens' CampaignResponse (Core.Maybe Core.Int)
crVersion = Lens.field @"version"
{-# DEPRECATED crVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | A string-to-string map of key-value pairs that identifies the tags that are associated with the campaign. Each tag consists of a required tag key and an associated tag value.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crTags :: Lens.Lens' CampaignResponse (Core.Maybe (Core.HashMap Core.Text Core.Text))
crTags = Lens.field @"tags"
{-# DEPRECATED crTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromJSON CampaignResponse where
  parseJSON =
    Core.withObject "CampaignResponse" Core.$
      \x ->
        CampaignResponse'
          Core.<$> (x Core..: "LastModifiedDate")
          Core.<*> (x Core..: "CreationDate")
          Core.<*> (x Core..: "SegmentId")
          Core.<*> (x Core..: "SegmentVersion")
          Core.<*> (x Core..: "Id")
          Core.<*> (x Core..: "Arn")
          Core.<*> (x Core..: "ApplicationId")
          Core.<*> (x Core..:? "AdditionalTreatments")
          Core.<*> (x Core..:? "CustomDeliveryConfiguration")
          Core.<*> (x Core..:? "DefaultState")
          Core.<*> (x Core..:? "Description")
          Core.<*> (x Core..:? "HoldoutPercent")
          Core.<*> (x Core..:? "Hook")
          Core.<*> (x Core..:? "IsPaused")
          Core.<*> (x Core..:? "Limits")
          Core.<*> (x Core..:? "MessageConfiguration")
          Core.<*> (x Core..:? "Name")
          Core.<*> (x Core..:? "Schedule")
          Core.<*> (x Core..:? "State")
          Core.<*> (x Core..:? "TemplateConfiguration")
          Core.<*> (x Core..:? "TreatmentDescription")
          Core.<*> (x Core..:? "TreatmentName")
          Core.<*> (x Core..:? "Version")
          Core.<*> (x Core..:? "tags")
