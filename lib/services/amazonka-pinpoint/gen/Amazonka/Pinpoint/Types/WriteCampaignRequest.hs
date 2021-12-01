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
-- Module      : Amazonka.Pinpoint.Types.WriteCampaignRequest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.WriteCampaignRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Pinpoint.Types.CampaignHook
import Amazonka.Pinpoint.Types.CampaignLimits
import Amazonka.Pinpoint.Types.CustomDeliveryConfiguration
import Amazonka.Pinpoint.Types.MessageConfiguration
import Amazonka.Pinpoint.Types.Schedule
import Amazonka.Pinpoint.Types.TemplateConfiguration
import Amazonka.Pinpoint.Types.WriteTreatmentResource
import qualified Amazonka.Prelude as Prelude

-- | Specifies the configuration and other settings for a campaign.
--
-- /See:/ 'newWriteCampaignRequest' smart constructor.
data WriteCampaignRequest = WriteCampaignRequest'
  { -- | The delivery configuration settings for sending the campaign through a
    -- custom channel. This object is required if the MessageConfiguration
    -- object for the campaign specifies a CustomMessage object.
    customDeliveryConfiguration :: Prelude.Maybe CustomDeliveryConfiguration,
    -- | Defines the priority of the campaign, used to decide the order of
    -- messages displayed to user if there are multiple messages scheduled to
    -- be displayed at the same moment.
    priority :: Prelude.Maybe Prelude.Int,
    -- | The schedule settings for the campaign.
    schedule :: Prelude.Maybe Schedule,
    -- | The message template to use for the campaign.
    templateConfiguration :: Prelude.Maybe TemplateConfiguration,
    -- | The settings for the AWS Lambda function to invoke as a code hook for
    -- the campaign. You can use this hook to customize the segment that\'s
    -- used by the campaign.
    hook :: Prelude.Maybe CampaignHook,
    -- | A custom name of the default treatment for the campaign, if the campaign
    -- has multiple treatments. A /treatment/ is a variation of a campaign
    -- that\'s used for A\/B testing.
    treatmentName :: Prelude.Maybe Prelude.Text,
    -- | The messaging limits for the campaign.
    limits :: Prelude.Maybe CampaignLimits,
    -- | Specifies whether to pause the campaign. A paused campaign doesn\'t run
    -- unless you resume it by changing this value to false.
    isPaused :: Prelude.Maybe Prelude.Bool,
    -- | A custom name for the campaign.
    name :: Prelude.Maybe Prelude.Text,
    -- | The allocated percentage of users (segment members) who shouldn\'t
    -- receive messages from the campaign.
    holdoutPercent :: Prelude.Maybe Prelude.Int,
    -- | A custom description of the default treatment for the campaign.
    treatmentDescription :: Prelude.Maybe Prelude.Text,
    -- | The message configuration settings for the campaign.
    messageConfiguration :: Prelude.Maybe MessageConfiguration,
    -- | A custom description of the campaign.
    description :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the segment to associate with the campaign.
    segmentId :: Prelude.Maybe Prelude.Text,
    -- | An array of requests that defines additional treatments for the
    -- campaign, in addition to the default treatment for the campaign.
    additionalTreatments :: Prelude.Maybe [WriteTreatmentResource],
    -- | A string-to-string map of key-value pairs that defines the tags to
    -- associate with the campaign. Each tag consists of a required tag key and
    -- an associated tag value.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The version of the segment to associate with the campaign.
    segmentVersion :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WriteCampaignRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'customDeliveryConfiguration', 'writeCampaignRequest_customDeliveryConfiguration' - The delivery configuration settings for sending the campaign through a
-- custom channel. This object is required if the MessageConfiguration
-- object for the campaign specifies a CustomMessage object.
--
-- 'priority', 'writeCampaignRequest_priority' - Defines the priority of the campaign, used to decide the order of
-- messages displayed to user if there are multiple messages scheduled to
-- be displayed at the same moment.
--
-- 'schedule', 'writeCampaignRequest_schedule' - The schedule settings for the campaign.
--
-- 'templateConfiguration', 'writeCampaignRequest_templateConfiguration' - The message template to use for the campaign.
--
-- 'hook', 'writeCampaignRequest_hook' - The settings for the AWS Lambda function to invoke as a code hook for
-- the campaign. You can use this hook to customize the segment that\'s
-- used by the campaign.
--
-- 'treatmentName', 'writeCampaignRequest_treatmentName' - A custom name of the default treatment for the campaign, if the campaign
-- has multiple treatments. A /treatment/ is a variation of a campaign
-- that\'s used for A\/B testing.
--
-- 'limits', 'writeCampaignRequest_limits' - The messaging limits for the campaign.
--
-- 'isPaused', 'writeCampaignRequest_isPaused' - Specifies whether to pause the campaign. A paused campaign doesn\'t run
-- unless you resume it by changing this value to false.
--
-- 'name', 'writeCampaignRequest_name' - A custom name for the campaign.
--
-- 'holdoutPercent', 'writeCampaignRequest_holdoutPercent' - The allocated percentage of users (segment members) who shouldn\'t
-- receive messages from the campaign.
--
-- 'treatmentDescription', 'writeCampaignRequest_treatmentDescription' - A custom description of the default treatment for the campaign.
--
-- 'messageConfiguration', 'writeCampaignRequest_messageConfiguration' - The message configuration settings for the campaign.
--
-- 'description', 'writeCampaignRequest_description' - A custom description of the campaign.
--
-- 'segmentId', 'writeCampaignRequest_segmentId' - The unique identifier for the segment to associate with the campaign.
--
-- 'additionalTreatments', 'writeCampaignRequest_additionalTreatments' - An array of requests that defines additional treatments for the
-- campaign, in addition to the default treatment for the campaign.
--
-- 'tags', 'writeCampaignRequest_tags' - A string-to-string map of key-value pairs that defines the tags to
-- associate with the campaign. Each tag consists of a required tag key and
-- an associated tag value.
--
-- 'segmentVersion', 'writeCampaignRequest_segmentVersion' - The version of the segment to associate with the campaign.
newWriteCampaignRequest ::
  WriteCampaignRequest
newWriteCampaignRequest =
  WriteCampaignRequest'
    { customDeliveryConfiguration =
        Prelude.Nothing,
      priority = Prelude.Nothing,
      schedule = Prelude.Nothing,
      templateConfiguration = Prelude.Nothing,
      hook = Prelude.Nothing,
      treatmentName = Prelude.Nothing,
      limits = Prelude.Nothing,
      isPaused = Prelude.Nothing,
      name = Prelude.Nothing,
      holdoutPercent = Prelude.Nothing,
      treatmentDescription = Prelude.Nothing,
      messageConfiguration = Prelude.Nothing,
      description = Prelude.Nothing,
      segmentId = Prelude.Nothing,
      additionalTreatments = Prelude.Nothing,
      tags = Prelude.Nothing,
      segmentVersion = Prelude.Nothing
    }

-- | The delivery configuration settings for sending the campaign through a
-- custom channel. This object is required if the MessageConfiguration
-- object for the campaign specifies a CustomMessage object.
writeCampaignRequest_customDeliveryConfiguration :: Lens.Lens' WriteCampaignRequest (Prelude.Maybe CustomDeliveryConfiguration)
writeCampaignRequest_customDeliveryConfiguration = Lens.lens (\WriteCampaignRequest' {customDeliveryConfiguration} -> customDeliveryConfiguration) (\s@WriteCampaignRequest' {} a -> s {customDeliveryConfiguration = a} :: WriteCampaignRequest)

-- | Defines the priority of the campaign, used to decide the order of
-- messages displayed to user if there are multiple messages scheduled to
-- be displayed at the same moment.
writeCampaignRequest_priority :: Lens.Lens' WriteCampaignRequest (Prelude.Maybe Prelude.Int)
writeCampaignRequest_priority = Lens.lens (\WriteCampaignRequest' {priority} -> priority) (\s@WriteCampaignRequest' {} a -> s {priority = a} :: WriteCampaignRequest)

-- | The schedule settings for the campaign.
writeCampaignRequest_schedule :: Lens.Lens' WriteCampaignRequest (Prelude.Maybe Schedule)
writeCampaignRequest_schedule = Lens.lens (\WriteCampaignRequest' {schedule} -> schedule) (\s@WriteCampaignRequest' {} a -> s {schedule = a} :: WriteCampaignRequest)

-- | The message template to use for the campaign.
writeCampaignRequest_templateConfiguration :: Lens.Lens' WriteCampaignRequest (Prelude.Maybe TemplateConfiguration)
writeCampaignRequest_templateConfiguration = Lens.lens (\WriteCampaignRequest' {templateConfiguration} -> templateConfiguration) (\s@WriteCampaignRequest' {} a -> s {templateConfiguration = a} :: WriteCampaignRequest)

-- | The settings for the AWS Lambda function to invoke as a code hook for
-- the campaign. You can use this hook to customize the segment that\'s
-- used by the campaign.
writeCampaignRequest_hook :: Lens.Lens' WriteCampaignRequest (Prelude.Maybe CampaignHook)
writeCampaignRequest_hook = Lens.lens (\WriteCampaignRequest' {hook} -> hook) (\s@WriteCampaignRequest' {} a -> s {hook = a} :: WriteCampaignRequest)

-- | A custom name of the default treatment for the campaign, if the campaign
-- has multiple treatments. A /treatment/ is a variation of a campaign
-- that\'s used for A\/B testing.
writeCampaignRequest_treatmentName :: Lens.Lens' WriteCampaignRequest (Prelude.Maybe Prelude.Text)
writeCampaignRequest_treatmentName = Lens.lens (\WriteCampaignRequest' {treatmentName} -> treatmentName) (\s@WriteCampaignRequest' {} a -> s {treatmentName = a} :: WriteCampaignRequest)

-- | The messaging limits for the campaign.
writeCampaignRequest_limits :: Lens.Lens' WriteCampaignRequest (Prelude.Maybe CampaignLimits)
writeCampaignRequest_limits = Lens.lens (\WriteCampaignRequest' {limits} -> limits) (\s@WriteCampaignRequest' {} a -> s {limits = a} :: WriteCampaignRequest)

-- | Specifies whether to pause the campaign. A paused campaign doesn\'t run
-- unless you resume it by changing this value to false.
writeCampaignRequest_isPaused :: Lens.Lens' WriteCampaignRequest (Prelude.Maybe Prelude.Bool)
writeCampaignRequest_isPaused = Lens.lens (\WriteCampaignRequest' {isPaused} -> isPaused) (\s@WriteCampaignRequest' {} a -> s {isPaused = a} :: WriteCampaignRequest)

-- | A custom name for the campaign.
writeCampaignRequest_name :: Lens.Lens' WriteCampaignRequest (Prelude.Maybe Prelude.Text)
writeCampaignRequest_name = Lens.lens (\WriteCampaignRequest' {name} -> name) (\s@WriteCampaignRequest' {} a -> s {name = a} :: WriteCampaignRequest)

-- | The allocated percentage of users (segment members) who shouldn\'t
-- receive messages from the campaign.
writeCampaignRequest_holdoutPercent :: Lens.Lens' WriteCampaignRequest (Prelude.Maybe Prelude.Int)
writeCampaignRequest_holdoutPercent = Lens.lens (\WriteCampaignRequest' {holdoutPercent} -> holdoutPercent) (\s@WriteCampaignRequest' {} a -> s {holdoutPercent = a} :: WriteCampaignRequest)

-- | A custom description of the default treatment for the campaign.
writeCampaignRequest_treatmentDescription :: Lens.Lens' WriteCampaignRequest (Prelude.Maybe Prelude.Text)
writeCampaignRequest_treatmentDescription = Lens.lens (\WriteCampaignRequest' {treatmentDescription} -> treatmentDescription) (\s@WriteCampaignRequest' {} a -> s {treatmentDescription = a} :: WriteCampaignRequest)

-- | The message configuration settings for the campaign.
writeCampaignRequest_messageConfiguration :: Lens.Lens' WriteCampaignRequest (Prelude.Maybe MessageConfiguration)
writeCampaignRequest_messageConfiguration = Lens.lens (\WriteCampaignRequest' {messageConfiguration} -> messageConfiguration) (\s@WriteCampaignRequest' {} a -> s {messageConfiguration = a} :: WriteCampaignRequest)

-- | A custom description of the campaign.
writeCampaignRequest_description :: Lens.Lens' WriteCampaignRequest (Prelude.Maybe Prelude.Text)
writeCampaignRequest_description = Lens.lens (\WriteCampaignRequest' {description} -> description) (\s@WriteCampaignRequest' {} a -> s {description = a} :: WriteCampaignRequest)

-- | The unique identifier for the segment to associate with the campaign.
writeCampaignRequest_segmentId :: Lens.Lens' WriteCampaignRequest (Prelude.Maybe Prelude.Text)
writeCampaignRequest_segmentId = Lens.lens (\WriteCampaignRequest' {segmentId} -> segmentId) (\s@WriteCampaignRequest' {} a -> s {segmentId = a} :: WriteCampaignRequest)

-- | An array of requests that defines additional treatments for the
-- campaign, in addition to the default treatment for the campaign.
writeCampaignRequest_additionalTreatments :: Lens.Lens' WriteCampaignRequest (Prelude.Maybe [WriteTreatmentResource])
writeCampaignRequest_additionalTreatments = Lens.lens (\WriteCampaignRequest' {additionalTreatments} -> additionalTreatments) (\s@WriteCampaignRequest' {} a -> s {additionalTreatments = a} :: WriteCampaignRequest) Prelude.. Lens.mapping Lens.coerced

-- | A string-to-string map of key-value pairs that defines the tags to
-- associate with the campaign. Each tag consists of a required tag key and
-- an associated tag value.
writeCampaignRequest_tags :: Lens.Lens' WriteCampaignRequest (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
writeCampaignRequest_tags = Lens.lens (\WriteCampaignRequest' {tags} -> tags) (\s@WriteCampaignRequest' {} a -> s {tags = a} :: WriteCampaignRequest) Prelude.. Lens.mapping Lens.coerced

-- | The version of the segment to associate with the campaign.
writeCampaignRequest_segmentVersion :: Lens.Lens' WriteCampaignRequest (Prelude.Maybe Prelude.Int)
writeCampaignRequest_segmentVersion = Lens.lens (\WriteCampaignRequest' {segmentVersion} -> segmentVersion) (\s@WriteCampaignRequest' {} a -> s {segmentVersion = a} :: WriteCampaignRequest)

instance Prelude.Hashable WriteCampaignRequest where
  hashWithSalt salt' WriteCampaignRequest' {..} =
    salt' `Prelude.hashWithSalt` segmentVersion
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` additionalTreatments
      `Prelude.hashWithSalt` segmentId
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` messageConfiguration
      `Prelude.hashWithSalt` treatmentDescription
      `Prelude.hashWithSalt` holdoutPercent
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` isPaused
      `Prelude.hashWithSalt` limits
      `Prelude.hashWithSalt` treatmentName
      `Prelude.hashWithSalt` hook
      `Prelude.hashWithSalt` templateConfiguration
      `Prelude.hashWithSalt` schedule
      `Prelude.hashWithSalt` priority
      `Prelude.hashWithSalt` customDeliveryConfiguration

instance Prelude.NFData WriteCampaignRequest where
  rnf WriteCampaignRequest' {..} =
    Prelude.rnf customDeliveryConfiguration
      `Prelude.seq` Prelude.rnf segmentVersion
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf additionalTreatments
      `Prelude.seq` Prelude.rnf segmentId
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf messageConfiguration
      `Prelude.seq` Prelude.rnf treatmentDescription
      `Prelude.seq` Prelude.rnf holdoutPercent
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf isPaused
      `Prelude.seq` Prelude.rnf limits
      `Prelude.seq` Prelude.rnf treatmentName
      `Prelude.seq` Prelude.rnf hook
      `Prelude.seq` Prelude.rnf templateConfiguration
      `Prelude.seq` Prelude.rnf schedule
      `Prelude.seq` Prelude.rnf priority

instance Core.ToJSON WriteCampaignRequest where
  toJSON WriteCampaignRequest' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("CustomDeliveryConfiguration" Core..=)
              Prelude.<$> customDeliveryConfiguration,
            ("Priority" Core..=) Prelude.<$> priority,
            ("Schedule" Core..=) Prelude.<$> schedule,
            ("TemplateConfiguration" Core..=)
              Prelude.<$> templateConfiguration,
            ("Hook" Core..=) Prelude.<$> hook,
            ("TreatmentName" Core..=) Prelude.<$> treatmentName,
            ("Limits" Core..=) Prelude.<$> limits,
            ("IsPaused" Core..=) Prelude.<$> isPaused,
            ("Name" Core..=) Prelude.<$> name,
            ("HoldoutPercent" Core..=)
              Prelude.<$> holdoutPercent,
            ("TreatmentDescription" Core..=)
              Prelude.<$> treatmentDescription,
            ("MessageConfiguration" Core..=)
              Prelude.<$> messageConfiguration,
            ("Description" Core..=) Prelude.<$> description,
            ("SegmentId" Core..=) Prelude.<$> segmentId,
            ("AdditionalTreatments" Core..=)
              Prelude.<$> additionalTreatments,
            ("tags" Core..=) Prelude.<$> tags,
            ("SegmentVersion" Core..=)
              Prelude.<$> segmentVersion
          ]
      )
