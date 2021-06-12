{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.CampaignResponse
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.CampaignResponse where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.CampaignHook
import Network.AWS.Pinpoint.Types.CampaignLimits
import Network.AWS.Pinpoint.Types.CampaignState
import Network.AWS.Pinpoint.Types.CustomDeliveryConfiguration
import Network.AWS.Pinpoint.Types.MessageConfiguration
import Network.AWS.Pinpoint.Types.Schedule
import Network.AWS.Pinpoint.Types.TemplateConfiguration
import Network.AWS.Pinpoint.Types.TreatmentResource

-- | Provides information about the status, configuration, and other settings
-- for a campaign.
--
-- /See:/ 'newCampaignResponse' smart constructor.
data CampaignResponse = CampaignResponse'
  { -- | An array of responses, one for each treatment that you defined for the
    -- campaign, in addition to the default treatment.
    additionalTreatments :: Core.Maybe [TreatmentResource],
    -- | The settings for the AWS Lambda function to use as a code hook for the
    -- campaign. You can use this hook to customize the segment that\'s used by
    -- the campaign.
    hook :: Core.Maybe CampaignHook,
    -- | The version number of the campaign.
    version :: Core.Maybe Core.Int,
    -- | The delivery configuration settings for sending the campaign through a
    -- custom channel.
    customDeliveryConfiguration :: Core.Maybe CustomDeliveryConfiguration,
    -- | The current status of the campaign.
    state :: Core.Maybe CampaignState,
    -- | The name of the campaign.
    name :: Core.Maybe Core.Text,
    -- | The current status of the campaign\'s default treatment. This value
    -- exists only for campaigns that have more than one treatment.
    defaultState :: Core.Maybe CampaignState,
    -- | Specifies whether the campaign is paused. A paused campaign doesn\'t run
    -- unless you resume it by changing this value to false.
    isPaused :: Core.Maybe Core.Bool,
    -- | A string-to-string map of key-value pairs that identifies the tags that
    -- are associated with the campaign. Each tag consists of a required tag
    -- key and an associated tag value.
    tags :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The messaging limits for the campaign.
    limits :: Core.Maybe CampaignLimits,
    -- | The custom description of the campaign.
    description :: Core.Maybe Core.Text,
    -- | The custom name of the default treatment for the campaign, if the
    -- campaign has multiple treatments. A /treatment/ is a variation of a
    -- campaign that\'s used for A\/B testing.
    treatmentName :: Core.Maybe Core.Text,
    -- | The message configuration settings for the campaign.
    messageConfiguration :: Core.Maybe MessageConfiguration,
    -- | The message template that’s used for the campaign.
    templateConfiguration :: Core.Maybe TemplateConfiguration,
    -- | The schedule settings for the campaign.
    schedule :: Core.Maybe Schedule,
    -- | The allocated percentage of users (segment members) who shouldn\'t
    -- receive messages from the campaign.
    holdoutPercent :: Core.Maybe Core.Int,
    -- | The custom description of the default treatment for the campaign.
    treatmentDescription :: Core.Maybe Core.Text,
    -- | The date, in ISO 8601 format, when the campaign was last modified.
    lastModifiedDate :: Core.Text,
    -- | The date, in ISO 8601 format, when the campaign was created.
    creationDate :: Core.Text,
    -- | The unique identifier for the segment that\'s associated with the
    -- campaign.
    segmentId :: Core.Text,
    -- | The version number of the segment that\'s associated with the campaign.
    segmentVersion :: Core.Int,
    -- | The unique identifier for the campaign.
    id :: Core.Text,
    -- | The Amazon Resource Name (ARN) of the campaign.
    arn :: Core.Text,
    -- | The unique identifier for the application that the campaign applies to.
    applicationId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CampaignResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'additionalTreatments', 'campaignResponse_additionalTreatments' - An array of responses, one for each treatment that you defined for the
-- campaign, in addition to the default treatment.
--
-- 'hook', 'campaignResponse_hook' - The settings for the AWS Lambda function to use as a code hook for the
-- campaign. You can use this hook to customize the segment that\'s used by
-- the campaign.
--
-- 'version', 'campaignResponse_version' - The version number of the campaign.
--
-- 'customDeliveryConfiguration', 'campaignResponse_customDeliveryConfiguration' - The delivery configuration settings for sending the campaign through a
-- custom channel.
--
-- 'state', 'campaignResponse_state' - The current status of the campaign.
--
-- 'name', 'campaignResponse_name' - The name of the campaign.
--
-- 'defaultState', 'campaignResponse_defaultState' - The current status of the campaign\'s default treatment. This value
-- exists only for campaigns that have more than one treatment.
--
-- 'isPaused', 'campaignResponse_isPaused' - Specifies whether the campaign is paused. A paused campaign doesn\'t run
-- unless you resume it by changing this value to false.
--
-- 'tags', 'campaignResponse_tags' - A string-to-string map of key-value pairs that identifies the tags that
-- are associated with the campaign. Each tag consists of a required tag
-- key and an associated tag value.
--
-- 'limits', 'campaignResponse_limits' - The messaging limits for the campaign.
--
-- 'description', 'campaignResponse_description' - The custom description of the campaign.
--
-- 'treatmentName', 'campaignResponse_treatmentName' - The custom name of the default treatment for the campaign, if the
-- campaign has multiple treatments. A /treatment/ is a variation of a
-- campaign that\'s used for A\/B testing.
--
-- 'messageConfiguration', 'campaignResponse_messageConfiguration' - The message configuration settings for the campaign.
--
-- 'templateConfiguration', 'campaignResponse_templateConfiguration' - The message template that’s used for the campaign.
--
-- 'schedule', 'campaignResponse_schedule' - The schedule settings for the campaign.
--
-- 'holdoutPercent', 'campaignResponse_holdoutPercent' - The allocated percentage of users (segment members) who shouldn\'t
-- receive messages from the campaign.
--
-- 'treatmentDescription', 'campaignResponse_treatmentDescription' - The custom description of the default treatment for the campaign.
--
-- 'lastModifiedDate', 'campaignResponse_lastModifiedDate' - The date, in ISO 8601 format, when the campaign was last modified.
--
-- 'creationDate', 'campaignResponse_creationDate' - The date, in ISO 8601 format, when the campaign was created.
--
-- 'segmentId', 'campaignResponse_segmentId' - The unique identifier for the segment that\'s associated with the
-- campaign.
--
-- 'segmentVersion', 'campaignResponse_segmentVersion' - The version number of the segment that\'s associated with the campaign.
--
-- 'id', 'campaignResponse_id' - The unique identifier for the campaign.
--
-- 'arn', 'campaignResponse_arn' - The Amazon Resource Name (ARN) of the campaign.
--
-- 'applicationId', 'campaignResponse_applicationId' - The unique identifier for the application that the campaign applies to.
newCampaignResponse ::
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
newCampaignResponse
  pLastModifiedDate_
  pCreationDate_
  pSegmentId_
  pSegmentVersion_
  pId_
  pArn_
  pApplicationId_ =
    CampaignResponse'
      { additionalTreatments =
          Core.Nothing,
        hook = Core.Nothing,
        version = Core.Nothing,
        customDeliveryConfiguration = Core.Nothing,
        state = Core.Nothing,
        name = Core.Nothing,
        defaultState = Core.Nothing,
        isPaused = Core.Nothing,
        tags = Core.Nothing,
        limits = Core.Nothing,
        description = Core.Nothing,
        treatmentName = Core.Nothing,
        messageConfiguration = Core.Nothing,
        templateConfiguration = Core.Nothing,
        schedule = Core.Nothing,
        holdoutPercent = Core.Nothing,
        treatmentDescription = Core.Nothing,
        lastModifiedDate = pLastModifiedDate_,
        creationDate = pCreationDate_,
        segmentId = pSegmentId_,
        segmentVersion = pSegmentVersion_,
        id = pId_,
        arn = pArn_,
        applicationId = pApplicationId_
      }

-- | An array of responses, one for each treatment that you defined for the
-- campaign, in addition to the default treatment.
campaignResponse_additionalTreatments :: Lens.Lens' CampaignResponse (Core.Maybe [TreatmentResource])
campaignResponse_additionalTreatments = Lens.lens (\CampaignResponse' {additionalTreatments} -> additionalTreatments) (\s@CampaignResponse' {} a -> s {additionalTreatments = a} :: CampaignResponse) Core.. Lens.mapping Lens._Coerce

-- | The settings for the AWS Lambda function to use as a code hook for the
-- campaign. You can use this hook to customize the segment that\'s used by
-- the campaign.
campaignResponse_hook :: Lens.Lens' CampaignResponse (Core.Maybe CampaignHook)
campaignResponse_hook = Lens.lens (\CampaignResponse' {hook} -> hook) (\s@CampaignResponse' {} a -> s {hook = a} :: CampaignResponse)

-- | The version number of the campaign.
campaignResponse_version :: Lens.Lens' CampaignResponse (Core.Maybe Core.Int)
campaignResponse_version = Lens.lens (\CampaignResponse' {version} -> version) (\s@CampaignResponse' {} a -> s {version = a} :: CampaignResponse)

-- | The delivery configuration settings for sending the campaign through a
-- custom channel.
campaignResponse_customDeliveryConfiguration :: Lens.Lens' CampaignResponse (Core.Maybe CustomDeliveryConfiguration)
campaignResponse_customDeliveryConfiguration = Lens.lens (\CampaignResponse' {customDeliveryConfiguration} -> customDeliveryConfiguration) (\s@CampaignResponse' {} a -> s {customDeliveryConfiguration = a} :: CampaignResponse)

-- | The current status of the campaign.
campaignResponse_state :: Lens.Lens' CampaignResponse (Core.Maybe CampaignState)
campaignResponse_state = Lens.lens (\CampaignResponse' {state} -> state) (\s@CampaignResponse' {} a -> s {state = a} :: CampaignResponse)

-- | The name of the campaign.
campaignResponse_name :: Lens.Lens' CampaignResponse (Core.Maybe Core.Text)
campaignResponse_name = Lens.lens (\CampaignResponse' {name} -> name) (\s@CampaignResponse' {} a -> s {name = a} :: CampaignResponse)

-- | The current status of the campaign\'s default treatment. This value
-- exists only for campaigns that have more than one treatment.
campaignResponse_defaultState :: Lens.Lens' CampaignResponse (Core.Maybe CampaignState)
campaignResponse_defaultState = Lens.lens (\CampaignResponse' {defaultState} -> defaultState) (\s@CampaignResponse' {} a -> s {defaultState = a} :: CampaignResponse)

-- | Specifies whether the campaign is paused. A paused campaign doesn\'t run
-- unless you resume it by changing this value to false.
campaignResponse_isPaused :: Lens.Lens' CampaignResponse (Core.Maybe Core.Bool)
campaignResponse_isPaused = Lens.lens (\CampaignResponse' {isPaused} -> isPaused) (\s@CampaignResponse' {} a -> s {isPaused = a} :: CampaignResponse)

-- | A string-to-string map of key-value pairs that identifies the tags that
-- are associated with the campaign. Each tag consists of a required tag
-- key and an associated tag value.
campaignResponse_tags :: Lens.Lens' CampaignResponse (Core.Maybe (Core.HashMap Core.Text Core.Text))
campaignResponse_tags = Lens.lens (\CampaignResponse' {tags} -> tags) (\s@CampaignResponse' {} a -> s {tags = a} :: CampaignResponse) Core.. Lens.mapping Lens._Coerce

-- | The messaging limits for the campaign.
campaignResponse_limits :: Lens.Lens' CampaignResponse (Core.Maybe CampaignLimits)
campaignResponse_limits = Lens.lens (\CampaignResponse' {limits} -> limits) (\s@CampaignResponse' {} a -> s {limits = a} :: CampaignResponse)

-- | The custom description of the campaign.
campaignResponse_description :: Lens.Lens' CampaignResponse (Core.Maybe Core.Text)
campaignResponse_description = Lens.lens (\CampaignResponse' {description} -> description) (\s@CampaignResponse' {} a -> s {description = a} :: CampaignResponse)

-- | The custom name of the default treatment for the campaign, if the
-- campaign has multiple treatments. A /treatment/ is a variation of a
-- campaign that\'s used for A\/B testing.
campaignResponse_treatmentName :: Lens.Lens' CampaignResponse (Core.Maybe Core.Text)
campaignResponse_treatmentName = Lens.lens (\CampaignResponse' {treatmentName} -> treatmentName) (\s@CampaignResponse' {} a -> s {treatmentName = a} :: CampaignResponse)

-- | The message configuration settings for the campaign.
campaignResponse_messageConfiguration :: Lens.Lens' CampaignResponse (Core.Maybe MessageConfiguration)
campaignResponse_messageConfiguration = Lens.lens (\CampaignResponse' {messageConfiguration} -> messageConfiguration) (\s@CampaignResponse' {} a -> s {messageConfiguration = a} :: CampaignResponse)

-- | The message template that’s used for the campaign.
campaignResponse_templateConfiguration :: Lens.Lens' CampaignResponse (Core.Maybe TemplateConfiguration)
campaignResponse_templateConfiguration = Lens.lens (\CampaignResponse' {templateConfiguration} -> templateConfiguration) (\s@CampaignResponse' {} a -> s {templateConfiguration = a} :: CampaignResponse)

-- | The schedule settings for the campaign.
campaignResponse_schedule :: Lens.Lens' CampaignResponse (Core.Maybe Schedule)
campaignResponse_schedule = Lens.lens (\CampaignResponse' {schedule} -> schedule) (\s@CampaignResponse' {} a -> s {schedule = a} :: CampaignResponse)

-- | The allocated percentage of users (segment members) who shouldn\'t
-- receive messages from the campaign.
campaignResponse_holdoutPercent :: Lens.Lens' CampaignResponse (Core.Maybe Core.Int)
campaignResponse_holdoutPercent = Lens.lens (\CampaignResponse' {holdoutPercent} -> holdoutPercent) (\s@CampaignResponse' {} a -> s {holdoutPercent = a} :: CampaignResponse)

-- | The custom description of the default treatment for the campaign.
campaignResponse_treatmentDescription :: Lens.Lens' CampaignResponse (Core.Maybe Core.Text)
campaignResponse_treatmentDescription = Lens.lens (\CampaignResponse' {treatmentDescription} -> treatmentDescription) (\s@CampaignResponse' {} a -> s {treatmentDescription = a} :: CampaignResponse)

-- | The date, in ISO 8601 format, when the campaign was last modified.
campaignResponse_lastModifiedDate :: Lens.Lens' CampaignResponse Core.Text
campaignResponse_lastModifiedDate = Lens.lens (\CampaignResponse' {lastModifiedDate} -> lastModifiedDate) (\s@CampaignResponse' {} a -> s {lastModifiedDate = a} :: CampaignResponse)

-- | The date, in ISO 8601 format, when the campaign was created.
campaignResponse_creationDate :: Lens.Lens' CampaignResponse Core.Text
campaignResponse_creationDate = Lens.lens (\CampaignResponse' {creationDate} -> creationDate) (\s@CampaignResponse' {} a -> s {creationDate = a} :: CampaignResponse)

-- | The unique identifier for the segment that\'s associated with the
-- campaign.
campaignResponse_segmentId :: Lens.Lens' CampaignResponse Core.Text
campaignResponse_segmentId = Lens.lens (\CampaignResponse' {segmentId} -> segmentId) (\s@CampaignResponse' {} a -> s {segmentId = a} :: CampaignResponse)

-- | The version number of the segment that\'s associated with the campaign.
campaignResponse_segmentVersion :: Lens.Lens' CampaignResponse Core.Int
campaignResponse_segmentVersion = Lens.lens (\CampaignResponse' {segmentVersion} -> segmentVersion) (\s@CampaignResponse' {} a -> s {segmentVersion = a} :: CampaignResponse)

-- | The unique identifier for the campaign.
campaignResponse_id :: Lens.Lens' CampaignResponse Core.Text
campaignResponse_id = Lens.lens (\CampaignResponse' {id} -> id) (\s@CampaignResponse' {} a -> s {id = a} :: CampaignResponse)

-- | The Amazon Resource Name (ARN) of the campaign.
campaignResponse_arn :: Lens.Lens' CampaignResponse Core.Text
campaignResponse_arn = Lens.lens (\CampaignResponse' {arn} -> arn) (\s@CampaignResponse' {} a -> s {arn = a} :: CampaignResponse)

-- | The unique identifier for the application that the campaign applies to.
campaignResponse_applicationId :: Lens.Lens' CampaignResponse Core.Text
campaignResponse_applicationId = Lens.lens (\CampaignResponse' {applicationId} -> applicationId) (\s@CampaignResponse' {} a -> s {applicationId = a} :: CampaignResponse)

instance Core.FromJSON CampaignResponse where
  parseJSON =
    Core.withObject
      "CampaignResponse"
      ( \x ->
          CampaignResponse'
            Core.<$> ( x Core..:? "AdditionalTreatments"
                         Core..!= Core.mempty
                     )
            Core.<*> (x Core..:? "Hook")
            Core.<*> (x Core..:? "Version")
            Core.<*> (x Core..:? "CustomDeliveryConfiguration")
            Core.<*> (x Core..:? "State")
            Core.<*> (x Core..:? "Name")
            Core.<*> (x Core..:? "DefaultState")
            Core.<*> (x Core..:? "IsPaused")
            Core.<*> (x Core..:? "tags" Core..!= Core.mempty)
            Core.<*> (x Core..:? "Limits")
            Core.<*> (x Core..:? "Description")
            Core.<*> (x Core..:? "TreatmentName")
            Core.<*> (x Core..:? "MessageConfiguration")
            Core.<*> (x Core..:? "TemplateConfiguration")
            Core.<*> (x Core..:? "Schedule")
            Core.<*> (x Core..:? "HoldoutPercent")
            Core.<*> (x Core..:? "TreatmentDescription")
            Core.<*> (x Core..: "LastModifiedDate")
            Core.<*> (x Core..: "CreationDate")
            Core.<*> (x Core..: "SegmentId")
            Core.<*> (x Core..: "SegmentVersion")
            Core.<*> (x Core..: "Id")
            Core.<*> (x Core..: "Arn")
            Core.<*> (x Core..: "ApplicationId")
      )

instance Core.Hashable CampaignResponse

instance Core.NFData CampaignResponse
