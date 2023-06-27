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
-- Module      : Amazonka.Pinpoint.Types.CampaignResponse
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.CampaignResponse where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Pinpoint.Types.CampaignHook
import Amazonka.Pinpoint.Types.CampaignLimits
import Amazonka.Pinpoint.Types.CampaignState
import Amazonka.Pinpoint.Types.CustomDeliveryConfiguration
import Amazonka.Pinpoint.Types.MessageConfiguration
import Amazonka.Pinpoint.Types.Schedule
import Amazonka.Pinpoint.Types.TemplateConfiguration
import Amazonka.Pinpoint.Types.TreatmentResource
import qualified Amazonka.Prelude as Prelude

-- | Provides information about the status, configuration, and other settings
-- for a campaign.
--
-- /See:/ 'newCampaignResponse' smart constructor.
data CampaignResponse = CampaignResponse'
  { -- | An array of responses, one for each treatment that you defined for the
    -- campaign, in addition to the default treatment.
    additionalTreatments :: Prelude.Maybe [TreatmentResource],
    -- | The delivery configuration settings for sending the campaign through a
    -- custom channel.
    customDeliveryConfiguration :: Prelude.Maybe CustomDeliveryConfiguration,
    -- | The current status of the campaign\'s default treatment. This value
    -- exists only for campaigns that have more than one treatment.
    defaultState :: Prelude.Maybe CampaignState,
    -- | The custom description of the campaign.
    description :: Prelude.Maybe Prelude.Text,
    -- | The allocated percentage of users (segment members) who shouldn\'t
    -- receive messages from the campaign.
    holdoutPercent :: Prelude.Maybe Prelude.Int,
    -- | The settings for the AWS Lambda function to use as a code hook for the
    -- campaign. You can use this hook to customize the segment that\'s used by
    -- the campaign.
    hook :: Prelude.Maybe CampaignHook,
    -- | Specifies whether the campaign is paused. A paused campaign doesn\'t run
    -- unless you resume it by changing this value to false.
    isPaused :: Prelude.Maybe Prelude.Bool,
    -- | The messaging limits for the campaign.
    limits :: Prelude.Maybe CampaignLimits,
    -- | The message configuration settings for the campaign.
    messageConfiguration :: Prelude.Maybe MessageConfiguration,
    -- | The name of the campaign.
    name :: Prelude.Maybe Prelude.Text,
    -- | Defines the priority of the campaign, used to decide the order of
    -- messages displayed to user if there are multiple messages scheduled to
    -- be displayed at the same moment.
    priority :: Prelude.Maybe Prelude.Int,
    -- | The schedule settings for the campaign.
    schedule :: Prelude.Maybe Schedule,
    -- | The current status of the campaign.
    state :: Prelude.Maybe CampaignState,
    -- | The message template that’s used for the campaign.
    templateConfiguration :: Prelude.Maybe TemplateConfiguration,
    -- | The custom description of the default treatment for the campaign.
    treatmentDescription :: Prelude.Maybe Prelude.Text,
    -- | The custom name of the default treatment for the campaign, if the
    -- campaign has multiple treatments. A /treatment/ is a variation of a
    -- campaign that\'s used for A\/B testing.
    treatmentName :: Prelude.Maybe Prelude.Text,
    -- | The version number of the campaign.
    version :: Prelude.Maybe Prelude.Int,
    -- | A string-to-string map of key-value pairs that identifies the tags that
    -- are associated with the campaign. Each tag consists of a required tag
    -- key and an associated tag value.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The date, in ISO 8601 format, when the campaign was last modified.
    lastModifiedDate :: Prelude.Text,
    -- | The date, in ISO 8601 format, when the campaign was created.
    creationDate :: Prelude.Text,
    -- | The unique identifier for the segment that\'s associated with the
    -- campaign.
    segmentId :: Prelude.Text,
    -- | The version number of the segment that\'s associated with the campaign.
    segmentVersion :: Prelude.Int,
    -- | The unique identifier for the campaign.
    id :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the campaign.
    arn :: Prelude.Text,
    -- | The unique identifier for the application that the campaign applies to.
    applicationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'customDeliveryConfiguration', 'campaignResponse_customDeliveryConfiguration' - The delivery configuration settings for sending the campaign through a
-- custom channel.
--
-- 'defaultState', 'campaignResponse_defaultState' - The current status of the campaign\'s default treatment. This value
-- exists only for campaigns that have more than one treatment.
--
-- 'description', 'campaignResponse_description' - The custom description of the campaign.
--
-- 'holdoutPercent', 'campaignResponse_holdoutPercent' - The allocated percentage of users (segment members) who shouldn\'t
-- receive messages from the campaign.
--
-- 'hook', 'campaignResponse_hook' - The settings for the AWS Lambda function to use as a code hook for the
-- campaign. You can use this hook to customize the segment that\'s used by
-- the campaign.
--
-- 'isPaused', 'campaignResponse_isPaused' - Specifies whether the campaign is paused. A paused campaign doesn\'t run
-- unless you resume it by changing this value to false.
--
-- 'limits', 'campaignResponse_limits' - The messaging limits for the campaign.
--
-- 'messageConfiguration', 'campaignResponse_messageConfiguration' - The message configuration settings for the campaign.
--
-- 'name', 'campaignResponse_name' - The name of the campaign.
--
-- 'priority', 'campaignResponse_priority' - Defines the priority of the campaign, used to decide the order of
-- messages displayed to user if there are multiple messages scheduled to
-- be displayed at the same moment.
--
-- 'schedule', 'campaignResponse_schedule' - The schedule settings for the campaign.
--
-- 'state', 'campaignResponse_state' - The current status of the campaign.
--
-- 'templateConfiguration', 'campaignResponse_templateConfiguration' - The message template that’s used for the campaign.
--
-- 'treatmentDescription', 'campaignResponse_treatmentDescription' - The custom description of the default treatment for the campaign.
--
-- 'treatmentName', 'campaignResponse_treatmentName' - The custom name of the default treatment for the campaign, if the
-- campaign has multiple treatments. A /treatment/ is a variation of a
-- campaign that\'s used for A\/B testing.
--
-- 'version', 'campaignResponse_version' - The version number of the campaign.
--
-- 'tags', 'campaignResponse_tags' - A string-to-string map of key-value pairs that identifies the tags that
-- are associated with the campaign. Each tag consists of a required tag
-- key and an associated tag value.
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
  Prelude.Text ->
  -- | 'creationDate'
  Prelude.Text ->
  -- | 'segmentId'
  Prelude.Text ->
  -- | 'segmentVersion'
  Prelude.Int ->
  -- | 'id'
  Prelude.Text ->
  -- | 'arn'
  Prelude.Text ->
  -- | 'applicationId'
  Prelude.Text ->
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
          Prelude.Nothing,
        customDeliveryConfiguration = Prelude.Nothing,
        defaultState = Prelude.Nothing,
        description = Prelude.Nothing,
        holdoutPercent = Prelude.Nothing,
        hook = Prelude.Nothing,
        isPaused = Prelude.Nothing,
        limits = Prelude.Nothing,
        messageConfiguration = Prelude.Nothing,
        name = Prelude.Nothing,
        priority = Prelude.Nothing,
        schedule = Prelude.Nothing,
        state = Prelude.Nothing,
        templateConfiguration = Prelude.Nothing,
        treatmentDescription = Prelude.Nothing,
        treatmentName = Prelude.Nothing,
        version = Prelude.Nothing,
        tags = Prelude.Nothing,
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
campaignResponse_additionalTreatments :: Lens.Lens' CampaignResponse (Prelude.Maybe [TreatmentResource])
campaignResponse_additionalTreatments = Lens.lens (\CampaignResponse' {additionalTreatments} -> additionalTreatments) (\s@CampaignResponse' {} a -> s {additionalTreatments = a} :: CampaignResponse) Prelude.. Lens.mapping Lens.coerced

-- | The delivery configuration settings for sending the campaign through a
-- custom channel.
campaignResponse_customDeliveryConfiguration :: Lens.Lens' CampaignResponse (Prelude.Maybe CustomDeliveryConfiguration)
campaignResponse_customDeliveryConfiguration = Lens.lens (\CampaignResponse' {customDeliveryConfiguration} -> customDeliveryConfiguration) (\s@CampaignResponse' {} a -> s {customDeliveryConfiguration = a} :: CampaignResponse)

-- | The current status of the campaign\'s default treatment. This value
-- exists only for campaigns that have more than one treatment.
campaignResponse_defaultState :: Lens.Lens' CampaignResponse (Prelude.Maybe CampaignState)
campaignResponse_defaultState = Lens.lens (\CampaignResponse' {defaultState} -> defaultState) (\s@CampaignResponse' {} a -> s {defaultState = a} :: CampaignResponse)

-- | The custom description of the campaign.
campaignResponse_description :: Lens.Lens' CampaignResponse (Prelude.Maybe Prelude.Text)
campaignResponse_description = Lens.lens (\CampaignResponse' {description} -> description) (\s@CampaignResponse' {} a -> s {description = a} :: CampaignResponse)

-- | The allocated percentage of users (segment members) who shouldn\'t
-- receive messages from the campaign.
campaignResponse_holdoutPercent :: Lens.Lens' CampaignResponse (Prelude.Maybe Prelude.Int)
campaignResponse_holdoutPercent = Lens.lens (\CampaignResponse' {holdoutPercent} -> holdoutPercent) (\s@CampaignResponse' {} a -> s {holdoutPercent = a} :: CampaignResponse)

-- | The settings for the AWS Lambda function to use as a code hook for the
-- campaign. You can use this hook to customize the segment that\'s used by
-- the campaign.
campaignResponse_hook :: Lens.Lens' CampaignResponse (Prelude.Maybe CampaignHook)
campaignResponse_hook = Lens.lens (\CampaignResponse' {hook} -> hook) (\s@CampaignResponse' {} a -> s {hook = a} :: CampaignResponse)

-- | Specifies whether the campaign is paused. A paused campaign doesn\'t run
-- unless you resume it by changing this value to false.
campaignResponse_isPaused :: Lens.Lens' CampaignResponse (Prelude.Maybe Prelude.Bool)
campaignResponse_isPaused = Lens.lens (\CampaignResponse' {isPaused} -> isPaused) (\s@CampaignResponse' {} a -> s {isPaused = a} :: CampaignResponse)

-- | The messaging limits for the campaign.
campaignResponse_limits :: Lens.Lens' CampaignResponse (Prelude.Maybe CampaignLimits)
campaignResponse_limits = Lens.lens (\CampaignResponse' {limits} -> limits) (\s@CampaignResponse' {} a -> s {limits = a} :: CampaignResponse)

-- | The message configuration settings for the campaign.
campaignResponse_messageConfiguration :: Lens.Lens' CampaignResponse (Prelude.Maybe MessageConfiguration)
campaignResponse_messageConfiguration = Lens.lens (\CampaignResponse' {messageConfiguration} -> messageConfiguration) (\s@CampaignResponse' {} a -> s {messageConfiguration = a} :: CampaignResponse)

-- | The name of the campaign.
campaignResponse_name :: Lens.Lens' CampaignResponse (Prelude.Maybe Prelude.Text)
campaignResponse_name = Lens.lens (\CampaignResponse' {name} -> name) (\s@CampaignResponse' {} a -> s {name = a} :: CampaignResponse)

-- | Defines the priority of the campaign, used to decide the order of
-- messages displayed to user if there are multiple messages scheduled to
-- be displayed at the same moment.
campaignResponse_priority :: Lens.Lens' CampaignResponse (Prelude.Maybe Prelude.Int)
campaignResponse_priority = Lens.lens (\CampaignResponse' {priority} -> priority) (\s@CampaignResponse' {} a -> s {priority = a} :: CampaignResponse)

-- | The schedule settings for the campaign.
campaignResponse_schedule :: Lens.Lens' CampaignResponse (Prelude.Maybe Schedule)
campaignResponse_schedule = Lens.lens (\CampaignResponse' {schedule} -> schedule) (\s@CampaignResponse' {} a -> s {schedule = a} :: CampaignResponse)

-- | The current status of the campaign.
campaignResponse_state :: Lens.Lens' CampaignResponse (Prelude.Maybe CampaignState)
campaignResponse_state = Lens.lens (\CampaignResponse' {state} -> state) (\s@CampaignResponse' {} a -> s {state = a} :: CampaignResponse)

-- | The message template that’s used for the campaign.
campaignResponse_templateConfiguration :: Lens.Lens' CampaignResponse (Prelude.Maybe TemplateConfiguration)
campaignResponse_templateConfiguration = Lens.lens (\CampaignResponse' {templateConfiguration} -> templateConfiguration) (\s@CampaignResponse' {} a -> s {templateConfiguration = a} :: CampaignResponse)

-- | The custom description of the default treatment for the campaign.
campaignResponse_treatmentDescription :: Lens.Lens' CampaignResponse (Prelude.Maybe Prelude.Text)
campaignResponse_treatmentDescription = Lens.lens (\CampaignResponse' {treatmentDescription} -> treatmentDescription) (\s@CampaignResponse' {} a -> s {treatmentDescription = a} :: CampaignResponse)

-- | The custom name of the default treatment for the campaign, if the
-- campaign has multiple treatments. A /treatment/ is a variation of a
-- campaign that\'s used for A\/B testing.
campaignResponse_treatmentName :: Lens.Lens' CampaignResponse (Prelude.Maybe Prelude.Text)
campaignResponse_treatmentName = Lens.lens (\CampaignResponse' {treatmentName} -> treatmentName) (\s@CampaignResponse' {} a -> s {treatmentName = a} :: CampaignResponse)

-- | The version number of the campaign.
campaignResponse_version :: Lens.Lens' CampaignResponse (Prelude.Maybe Prelude.Int)
campaignResponse_version = Lens.lens (\CampaignResponse' {version} -> version) (\s@CampaignResponse' {} a -> s {version = a} :: CampaignResponse)

-- | A string-to-string map of key-value pairs that identifies the tags that
-- are associated with the campaign. Each tag consists of a required tag
-- key and an associated tag value.
campaignResponse_tags :: Lens.Lens' CampaignResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
campaignResponse_tags = Lens.lens (\CampaignResponse' {tags} -> tags) (\s@CampaignResponse' {} a -> s {tags = a} :: CampaignResponse) Prelude.. Lens.mapping Lens.coerced

-- | The date, in ISO 8601 format, when the campaign was last modified.
campaignResponse_lastModifiedDate :: Lens.Lens' CampaignResponse Prelude.Text
campaignResponse_lastModifiedDate = Lens.lens (\CampaignResponse' {lastModifiedDate} -> lastModifiedDate) (\s@CampaignResponse' {} a -> s {lastModifiedDate = a} :: CampaignResponse)

-- | The date, in ISO 8601 format, when the campaign was created.
campaignResponse_creationDate :: Lens.Lens' CampaignResponse Prelude.Text
campaignResponse_creationDate = Lens.lens (\CampaignResponse' {creationDate} -> creationDate) (\s@CampaignResponse' {} a -> s {creationDate = a} :: CampaignResponse)

-- | The unique identifier for the segment that\'s associated with the
-- campaign.
campaignResponse_segmentId :: Lens.Lens' CampaignResponse Prelude.Text
campaignResponse_segmentId = Lens.lens (\CampaignResponse' {segmentId} -> segmentId) (\s@CampaignResponse' {} a -> s {segmentId = a} :: CampaignResponse)

-- | The version number of the segment that\'s associated with the campaign.
campaignResponse_segmentVersion :: Lens.Lens' CampaignResponse Prelude.Int
campaignResponse_segmentVersion = Lens.lens (\CampaignResponse' {segmentVersion} -> segmentVersion) (\s@CampaignResponse' {} a -> s {segmentVersion = a} :: CampaignResponse)

-- | The unique identifier for the campaign.
campaignResponse_id :: Lens.Lens' CampaignResponse Prelude.Text
campaignResponse_id = Lens.lens (\CampaignResponse' {id} -> id) (\s@CampaignResponse' {} a -> s {id = a} :: CampaignResponse)

-- | The Amazon Resource Name (ARN) of the campaign.
campaignResponse_arn :: Lens.Lens' CampaignResponse Prelude.Text
campaignResponse_arn = Lens.lens (\CampaignResponse' {arn} -> arn) (\s@CampaignResponse' {} a -> s {arn = a} :: CampaignResponse)

-- | The unique identifier for the application that the campaign applies to.
campaignResponse_applicationId :: Lens.Lens' CampaignResponse Prelude.Text
campaignResponse_applicationId = Lens.lens (\CampaignResponse' {applicationId} -> applicationId) (\s@CampaignResponse' {} a -> s {applicationId = a} :: CampaignResponse)

instance Data.FromJSON CampaignResponse where
  parseJSON =
    Data.withObject
      "CampaignResponse"
      ( \x ->
          CampaignResponse'
            Prelude.<$> ( x
                            Data..:? "AdditionalTreatments"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "CustomDeliveryConfiguration")
            Prelude.<*> (x Data..:? "DefaultState")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "HoldoutPercent")
            Prelude.<*> (x Data..:? "Hook")
            Prelude.<*> (x Data..:? "IsPaused")
            Prelude.<*> (x Data..:? "Limits")
            Prelude.<*> (x Data..:? "MessageConfiguration")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "Priority")
            Prelude.<*> (x Data..:? "Schedule")
            Prelude.<*> (x Data..:? "State")
            Prelude.<*> (x Data..:? "TemplateConfiguration")
            Prelude.<*> (x Data..:? "TreatmentDescription")
            Prelude.<*> (x Data..:? "TreatmentName")
            Prelude.<*> (x Data..:? "Version")
            Prelude.<*> (x Data..:? "tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "LastModifiedDate")
            Prelude.<*> (x Data..: "CreationDate")
            Prelude.<*> (x Data..: "SegmentId")
            Prelude.<*> (x Data..: "SegmentVersion")
            Prelude.<*> (x Data..: "Id")
            Prelude.<*> (x Data..: "Arn")
            Prelude.<*> (x Data..: "ApplicationId")
      )

instance Prelude.Hashable CampaignResponse where
  hashWithSalt _salt CampaignResponse' {..} =
    _salt
      `Prelude.hashWithSalt` additionalTreatments
      `Prelude.hashWithSalt` customDeliveryConfiguration
      `Prelude.hashWithSalt` defaultState
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` holdoutPercent
      `Prelude.hashWithSalt` hook
      `Prelude.hashWithSalt` isPaused
      `Prelude.hashWithSalt` limits
      `Prelude.hashWithSalt` messageConfiguration
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` priority
      `Prelude.hashWithSalt` schedule
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` templateConfiguration
      `Prelude.hashWithSalt` treatmentDescription
      `Prelude.hashWithSalt` treatmentName
      `Prelude.hashWithSalt` version
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` lastModifiedDate
      `Prelude.hashWithSalt` creationDate
      `Prelude.hashWithSalt` segmentId
      `Prelude.hashWithSalt` segmentVersion
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` applicationId

instance Prelude.NFData CampaignResponse where
  rnf CampaignResponse' {..} =
    Prelude.rnf additionalTreatments
      `Prelude.seq` Prelude.rnf customDeliveryConfiguration
      `Prelude.seq` Prelude.rnf defaultState
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf holdoutPercent
      `Prelude.seq` Prelude.rnf hook
      `Prelude.seq` Prelude.rnf isPaused
      `Prelude.seq` Prelude.rnf limits
      `Prelude.seq` Prelude.rnf messageConfiguration
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf priority
      `Prelude.seq` Prelude.rnf schedule
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf templateConfiguration
      `Prelude.seq` Prelude.rnf treatmentDescription
      `Prelude.seq` Prelude.rnf treatmentName
      `Prelude.seq` Prelude.rnf version
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf lastModifiedDate
      `Prelude.seq` Prelude.rnf creationDate
      `Prelude.seq` Prelude.rnf segmentId
      `Prelude.seq` Prelude.rnf segmentVersion
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf
        applicationId
