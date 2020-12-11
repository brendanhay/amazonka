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
    cCustomDeliveryConfiguration,
    cState,
    cSchedule,
    cTemplateConfiguration,
    cHook,
    cTreatmentName,
    cLimits,
    cIsPaused,
    cDefaultState,
    cName,
    cVersion,
    cHoldoutPercent,
    cTreatmentDescription,
    cMessageConfiguration,
    cDescription,
    cAdditionalTreatments,
    cTags,
    cLastModifiedDate,
    cCreationDate,
    cSegmentId,
    cSegmentVersion,
    cId,
    cARN,
    cApplicationId,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.CampaignHook
import Network.AWS.Pinpoint.Types.CampaignLimits
import Network.AWS.Pinpoint.Types.CampaignState
import Network.AWS.Pinpoint.Types.CustomDeliveryConfiguration
import Network.AWS.Pinpoint.Types.MessageConfiguration
import Network.AWS.Pinpoint.Types.Schedule
import Network.AWS.Pinpoint.Types.TemplateConfiguration
import Network.AWS.Pinpoint.Types.TreatmentResource
import qualified Network.AWS.Prelude as Lude

-- | Provides information about the status, configuration, and other settings for a campaign.
--
-- /See:/ 'mkCampaignResponse' smart constructor.
data CampaignResponse = CampaignResponse'
  { customDeliveryConfiguration ::
      Lude.Maybe CustomDeliveryConfiguration,
    state :: Lude.Maybe CampaignState,
    schedule :: Lude.Maybe Schedule,
    templateConfiguration :: Lude.Maybe TemplateConfiguration,
    hook :: Lude.Maybe CampaignHook,
    treatmentName :: Lude.Maybe Lude.Text,
    limits :: Lude.Maybe CampaignLimits,
    isPaused :: Lude.Maybe Lude.Bool,
    defaultState :: Lude.Maybe CampaignState,
    name :: Lude.Maybe Lude.Text,
    version :: Lude.Maybe Lude.Int,
    holdoutPercent :: Lude.Maybe Lude.Int,
    treatmentDescription :: Lude.Maybe Lude.Text,
    messageConfiguration :: Lude.Maybe MessageConfiguration,
    description :: Lude.Maybe Lude.Text,
    additionalTreatments :: Lude.Maybe [TreatmentResource],
    tags :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    lastModifiedDate :: Lude.Text,
    creationDate :: Lude.Text,
    segmentId :: Lude.Text,
    segmentVersion :: Lude.Int,
    id :: Lude.Text,
    arn :: Lude.Text,
    applicationId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CampaignResponse' with the minimum fields required to make a request.
--
-- * 'additionalTreatments' - An array of responses, one for each treatment that you defined for the campaign, in addition to the default treatment.
-- * 'applicationId' - The unique identifier for the application that the campaign applies to.
-- * 'arn' - The Amazon Resource Name (ARN) of the campaign.
-- * 'creationDate' - The date, in ISO 8601 format, when the campaign was created.
-- * 'customDeliveryConfiguration' - The delivery configuration settings for sending the campaign through a custom channel.
-- * 'defaultState' - The current status of the campaign's default treatment. This value exists only for campaigns that have more than one treatment.
-- * 'description' - The custom description of the campaign.
-- * 'holdoutPercent' - The allocated percentage of users (segment members) who shouldn't receive messages from the campaign.
-- * 'hook' - The settings for the AWS Lambda function to use as a code hook for the campaign. You can use this hook to customize the segment that's used by the campaign.
-- * 'id' - The unique identifier for the campaign.
-- * 'isPaused' - Specifies whether the campaign is paused. A paused campaign doesn't run unless you resume it by changing this value to false.
-- * 'lastModifiedDate' - The date, in ISO 8601 format, when the campaign was last modified.
-- * 'limits' - The messaging limits for the campaign.
-- * 'messageConfiguration' - The message configuration settings for the campaign.
-- * 'name' - The name of the campaign.
-- * 'schedule' - The schedule settings for the campaign.
-- * 'segmentId' - The unique identifier for the segment that's associated with the campaign.
-- * 'segmentVersion' - The version number of the segment that's associated with the campaign.
-- * 'state' - The current status of the campaign.
-- * 'tags' - A string-to-string map of key-value pairs that identifies the tags that are associated with the campaign. Each tag consists of a required tag key and an associated tag value.
-- * 'templateConfiguration' - The message template that’s used for the campaign.
-- * 'treatmentDescription' - The custom description of the default treatment for the campaign.
-- * 'treatmentName' - The custom name of the default treatment for the campaign, if the campaign has multiple treatments. A /treatment/ is a variation of a campaign that's used for A/B testing.
-- * 'version' - The version number of the campaign.
mkCampaignResponse ::
  -- | 'lastModifiedDate'
  Lude.Text ->
  -- | 'creationDate'
  Lude.Text ->
  -- | 'segmentId'
  Lude.Text ->
  -- | 'segmentVersion'
  Lude.Int ->
  -- | 'id'
  Lude.Text ->
  -- | 'arn'
  Lude.Text ->
  -- | 'applicationId'
  Lude.Text ->
  CampaignResponse
mkCampaignResponse
  pLastModifiedDate_
  pCreationDate_
  pSegmentId_
  pSegmentVersion_
  pId_
  pARN_
  pApplicationId_ =
    CampaignResponse'
      { customDeliveryConfiguration = Lude.Nothing,
        state = Lude.Nothing,
        schedule = Lude.Nothing,
        templateConfiguration = Lude.Nothing,
        hook = Lude.Nothing,
        treatmentName = Lude.Nothing,
        limits = Lude.Nothing,
        isPaused = Lude.Nothing,
        defaultState = Lude.Nothing,
        name = Lude.Nothing,
        version = Lude.Nothing,
        holdoutPercent = Lude.Nothing,
        treatmentDescription = Lude.Nothing,
        messageConfiguration = Lude.Nothing,
        description = Lude.Nothing,
        additionalTreatments = Lude.Nothing,
        tags = Lude.Nothing,
        lastModifiedDate = pLastModifiedDate_,
        creationDate = pCreationDate_,
        segmentId = pSegmentId_,
        segmentVersion = pSegmentVersion_,
        id = pId_,
        arn = pARN_,
        applicationId = pApplicationId_
      }

-- | The delivery configuration settings for sending the campaign through a custom channel.
--
-- /Note:/ Consider using 'customDeliveryConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCustomDeliveryConfiguration :: Lens.Lens' CampaignResponse (Lude.Maybe CustomDeliveryConfiguration)
cCustomDeliveryConfiguration = Lens.lens (customDeliveryConfiguration :: CampaignResponse -> Lude.Maybe CustomDeliveryConfiguration) (\s a -> s {customDeliveryConfiguration = a} :: CampaignResponse)
{-# DEPRECATED cCustomDeliveryConfiguration "Use generic-lens or generic-optics with 'customDeliveryConfiguration' instead." #-}

-- | The current status of the campaign.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cState :: Lens.Lens' CampaignResponse (Lude.Maybe CampaignState)
cState = Lens.lens (state :: CampaignResponse -> Lude.Maybe CampaignState) (\s a -> s {state = a} :: CampaignResponse)
{-# DEPRECATED cState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The schedule settings for the campaign.
--
-- /Note:/ Consider using 'schedule' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cSchedule :: Lens.Lens' CampaignResponse (Lude.Maybe Schedule)
cSchedule = Lens.lens (schedule :: CampaignResponse -> Lude.Maybe Schedule) (\s a -> s {schedule = a} :: CampaignResponse)
{-# DEPRECATED cSchedule "Use generic-lens or generic-optics with 'schedule' instead." #-}

-- | The message template that’s used for the campaign.
--
-- /Note:/ Consider using 'templateConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cTemplateConfiguration :: Lens.Lens' CampaignResponse (Lude.Maybe TemplateConfiguration)
cTemplateConfiguration = Lens.lens (templateConfiguration :: CampaignResponse -> Lude.Maybe TemplateConfiguration) (\s a -> s {templateConfiguration = a} :: CampaignResponse)
{-# DEPRECATED cTemplateConfiguration "Use generic-lens or generic-optics with 'templateConfiguration' instead." #-}

-- | The settings for the AWS Lambda function to use as a code hook for the campaign. You can use this hook to customize the segment that's used by the campaign.
--
-- /Note:/ Consider using 'hook' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cHook :: Lens.Lens' CampaignResponse (Lude.Maybe CampaignHook)
cHook = Lens.lens (hook :: CampaignResponse -> Lude.Maybe CampaignHook) (\s a -> s {hook = a} :: CampaignResponse)
{-# DEPRECATED cHook "Use generic-lens or generic-optics with 'hook' instead." #-}

-- | The custom name of the default treatment for the campaign, if the campaign has multiple treatments. A /treatment/ is a variation of a campaign that's used for A/B testing.
--
-- /Note:/ Consider using 'treatmentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cTreatmentName :: Lens.Lens' CampaignResponse (Lude.Maybe Lude.Text)
cTreatmentName = Lens.lens (treatmentName :: CampaignResponse -> Lude.Maybe Lude.Text) (\s a -> s {treatmentName = a} :: CampaignResponse)
{-# DEPRECATED cTreatmentName "Use generic-lens or generic-optics with 'treatmentName' instead." #-}

-- | The messaging limits for the campaign.
--
-- /Note:/ Consider using 'limits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cLimits :: Lens.Lens' CampaignResponse (Lude.Maybe CampaignLimits)
cLimits = Lens.lens (limits :: CampaignResponse -> Lude.Maybe CampaignLimits) (\s a -> s {limits = a} :: CampaignResponse)
{-# DEPRECATED cLimits "Use generic-lens or generic-optics with 'limits' instead." #-}

-- | Specifies whether the campaign is paused. A paused campaign doesn't run unless you resume it by changing this value to false.
--
-- /Note:/ Consider using 'isPaused' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cIsPaused :: Lens.Lens' CampaignResponse (Lude.Maybe Lude.Bool)
cIsPaused = Lens.lens (isPaused :: CampaignResponse -> Lude.Maybe Lude.Bool) (\s a -> s {isPaused = a} :: CampaignResponse)
{-# DEPRECATED cIsPaused "Use generic-lens or generic-optics with 'isPaused' instead." #-}

-- | The current status of the campaign's default treatment. This value exists only for campaigns that have more than one treatment.
--
-- /Note:/ Consider using 'defaultState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cDefaultState :: Lens.Lens' CampaignResponse (Lude.Maybe CampaignState)
cDefaultState = Lens.lens (defaultState :: CampaignResponse -> Lude.Maybe CampaignState) (\s a -> s {defaultState = a} :: CampaignResponse)
{-# DEPRECATED cDefaultState "Use generic-lens or generic-optics with 'defaultState' instead." #-}

-- | The name of the campaign.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cName :: Lens.Lens' CampaignResponse (Lude.Maybe Lude.Text)
cName = Lens.lens (name :: CampaignResponse -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: CampaignResponse)
{-# DEPRECATED cName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The version number of the campaign.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cVersion :: Lens.Lens' CampaignResponse (Lude.Maybe Lude.Int)
cVersion = Lens.lens (version :: CampaignResponse -> Lude.Maybe Lude.Int) (\s a -> s {version = a} :: CampaignResponse)
{-# DEPRECATED cVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | The allocated percentage of users (segment members) who shouldn't receive messages from the campaign.
--
-- /Note:/ Consider using 'holdoutPercent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cHoldoutPercent :: Lens.Lens' CampaignResponse (Lude.Maybe Lude.Int)
cHoldoutPercent = Lens.lens (holdoutPercent :: CampaignResponse -> Lude.Maybe Lude.Int) (\s a -> s {holdoutPercent = a} :: CampaignResponse)
{-# DEPRECATED cHoldoutPercent "Use generic-lens or generic-optics with 'holdoutPercent' instead." #-}

-- | The custom description of the default treatment for the campaign.
--
-- /Note:/ Consider using 'treatmentDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cTreatmentDescription :: Lens.Lens' CampaignResponse (Lude.Maybe Lude.Text)
cTreatmentDescription = Lens.lens (treatmentDescription :: CampaignResponse -> Lude.Maybe Lude.Text) (\s a -> s {treatmentDescription = a} :: CampaignResponse)
{-# DEPRECATED cTreatmentDescription "Use generic-lens or generic-optics with 'treatmentDescription' instead." #-}

-- | The message configuration settings for the campaign.
--
-- /Note:/ Consider using 'messageConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cMessageConfiguration :: Lens.Lens' CampaignResponse (Lude.Maybe MessageConfiguration)
cMessageConfiguration = Lens.lens (messageConfiguration :: CampaignResponse -> Lude.Maybe MessageConfiguration) (\s a -> s {messageConfiguration = a} :: CampaignResponse)
{-# DEPRECATED cMessageConfiguration "Use generic-lens or generic-optics with 'messageConfiguration' instead." #-}

-- | The custom description of the campaign.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cDescription :: Lens.Lens' CampaignResponse (Lude.Maybe Lude.Text)
cDescription = Lens.lens (description :: CampaignResponse -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: CampaignResponse)
{-# DEPRECATED cDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | An array of responses, one for each treatment that you defined for the campaign, in addition to the default treatment.
--
-- /Note:/ Consider using 'additionalTreatments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cAdditionalTreatments :: Lens.Lens' CampaignResponse (Lude.Maybe [TreatmentResource])
cAdditionalTreatments = Lens.lens (additionalTreatments :: CampaignResponse -> Lude.Maybe [TreatmentResource]) (\s a -> s {additionalTreatments = a} :: CampaignResponse)
{-# DEPRECATED cAdditionalTreatments "Use generic-lens or generic-optics with 'additionalTreatments' instead." #-}

-- | A string-to-string map of key-value pairs that identifies the tags that are associated with the campaign. Each tag consists of a required tag key and an associated tag value.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cTags :: Lens.Lens' CampaignResponse (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
cTags = Lens.lens (tags :: CampaignResponse -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: CampaignResponse)
{-# DEPRECATED cTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The date, in ISO 8601 format, when the campaign was last modified.
--
-- /Note:/ Consider using 'lastModifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cLastModifiedDate :: Lens.Lens' CampaignResponse Lude.Text
cLastModifiedDate = Lens.lens (lastModifiedDate :: CampaignResponse -> Lude.Text) (\s a -> s {lastModifiedDate = a} :: CampaignResponse)
{-# DEPRECATED cLastModifiedDate "Use generic-lens or generic-optics with 'lastModifiedDate' instead." #-}

-- | The date, in ISO 8601 format, when the campaign was created.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCreationDate :: Lens.Lens' CampaignResponse Lude.Text
cCreationDate = Lens.lens (creationDate :: CampaignResponse -> Lude.Text) (\s a -> s {creationDate = a} :: CampaignResponse)
{-# DEPRECATED cCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

-- | The unique identifier for the segment that's associated with the campaign.
--
-- /Note:/ Consider using 'segmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cSegmentId :: Lens.Lens' CampaignResponse Lude.Text
cSegmentId = Lens.lens (segmentId :: CampaignResponse -> Lude.Text) (\s a -> s {segmentId = a} :: CampaignResponse)
{-# DEPRECATED cSegmentId "Use generic-lens or generic-optics with 'segmentId' instead." #-}

-- | The version number of the segment that's associated with the campaign.
--
-- /Note:/ Consider using 'segmentVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cSegmentVersion :: Lens.Lens' CampaignResponse Lude.Int
cSegmentVersion = Lens.lens (segmentVersion :: CampaignResponse -> Lude.Int) (\s a -> s {segmentVersion = a} :: CampaignResponse)
{-# DEPRECATED cSegmentVersion "Use generic-lens or generic-optics with 'segmentVersion' instead." #-}

-- | The unique identifier for the campaign.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cId :: Lens.Lens' CampaignResponse Lude.Text
cId = Lens.lens (id :: CampaignResponse -> Lude.Text) (\s a -> s {id = a} :: CampaignResponse)
{-# DEPRECATED cId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The Amazon Resource Name (ARN) of the campaign.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cARN :: Lens.Lens' CampaignResponse Lude.Text
cARN = Lens.lens (arn :: CampaignResponse -> Lude.Text) (\s a -> s {arn = a} :: CampaignResponse)
{-# DEPRECATED cARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The unique identifier for the application that the campaign applies to.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cApplicationId :: Lens.Lens' CampaignResponse Lude.Text
cApplicationId = Lens.lens (applicationId :: CampaignResponse -> Lude.Text) (\s a -> s {applicationId = a} :: CampaignResponse)
{-# DEPRECATED cApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

instance Lude.FromJSON CampaignResponse where
  parseJSON =
    Lude.withObject
      "CampaignResponse"
      ( \x ->
          CampaignResponse'
            Lude.<$> (x Lude..:? "CustomDeliveryConfiguration")
            Lude.<*> (x Lude..:? "State")
            Lude.<*> (x Lude..:? "Schedule")
            Lude.<*> (x Lude..:? "TemplateConfiguration")
            Lude.<*> (x Lude..:? "Hook")
            Lude.<*> (x Lude..:? "TreatmentName")
            Lude.<*> (x Lude..:? "Limits")
            Lude.<*> (x Lude..:? "IsPaused")
            Lude.<*> (x Lude..:? "DefaultState")
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "Version")
            Lude.<*> (x Lude..:? "HoldoutPercent")
            Lude.<*> (x Lude..:? "TreatmentDescription")
            Lude.<*> (x Lude..:? "MessageConfiguration")
            Lude.<*> (x Lude..:? "Description")
            Lude.<*> (x Lude..:? "AdditionalTreatments" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "tags" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..: "LastModifiedDate")
            Lude.<*> (x Lude..: "CreationDate")
            Lude.<*> (x Lude..: "SegmentId")
            Lude.<*> (x Lude..: "SegmentVersion")
            Lude.<*> (x Lude..: "Id")
            Lude.<*> (x Lude..: "Arn")
            Lude.<*> (x Lude..: "ApplicationId")
      )
