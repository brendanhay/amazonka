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
-- Module      : Network.AWS.Pinpoint.Types.WriteCampaignRequest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.WriteCampaignRequest where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.CampaignHook
import Network.AWS.Pinpoint.Types.CampaignLimits
import Network.AWS.Pinpoint.Types.CustomDeliveryConfiguration
import Network.AWS.Pinpoint.Types.MessageConfiguration
import Network.AWS.Pinpoint.Types.Schedule
import Network.AWS.Pinpoint.Types.TemplateConfiguration
import Network.AWS.Pinpoint.Types.WriteTreatmentResource
import qualified Network.AWS.Prelude as Prelude

-- | Specifies the configuration and other settings for a campaign.
--
-- /See:/ 'newWriteCampaignRequest' smart constructor.
data WriteCampaignRequest = WriteCampaignRequest'
  { -- | An array of requests that defines additional treatments for the
    -- campaign, in addition to the default treatment for the campaign.
    additionalTreatments :: Prelude.Maybe [WriteTreatmentResource],
    -- | The settings for the AWS Lambda function to invoke as a code hook for
    -- the campaign. You can use this hook to customize the segment that\'s
    -- used by the campaign.
    hook :: Prelude.Maybe CampaignHook,
    -- | The delivery configuration settings for sending the campaign through a
    -- custom channel. This object is required if the MessageConfiguration
    -- object for the campaign specifies a CustomMessage object.
    customDeliveryConfiguration :: Prelude.Maybe CustomDeliveryConfiguration,
    -- | A custom name for the campaign.
    name :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether to pause the campaign. A paused campaign doesn\'t run
    -- unless you resume it by changing this value to false.
    isPaused :: Prelude.Maybe Prelude.Bool,
    -- | A string-to-string map of key-value pairs that defines the tags to
    -- associate with the campaign. Each tag consists of a required tag key and
    -- an associated tag value.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The version of the segment to associate with the campaign.
    segmentVersion :: Prelude.Maybe Prelude.Int,
    -- | The messaging limits for the campaign.
    limits :: Prelude.Maybe CampaignLimits,
    -- | The unique identifier for the segment to associate with the campaign.
    segmentId :: Prelude.Maybe Prelude.Text,
    -- | A custom description of the campaign.
    description :: Prelude.Maybe Prelude.Text,
    -- | A custom name of the default treatment for the campaign, if the campaign
    -- has multiple treatments. A /treatment/ is a variation of a campaign
    -- that\'s used for A\/B testing.
    treatmentName :: Prelude.Maybe Prelude.Text,
    -- | The message configuration settings for the campaign.
    messageConfiguration :: Prelude.Maybe MessageConfiguration,
    -- | The message template to use for the campaign.
    templateConfiguration :: Prelude.Maybe TemplateConfiguration,
    -- | The schedule settings for the campaign.
    schedule :: Prelude.Maybe Schedule,
    -- | The allocated percentage of users (segment members) who shouldn\'t
    -- receive messages from the campaign.
    holdoutPercent :: Prelude.Maybe Prelude.Int,
    -- | A custom description of the default treatment for the campaign.
    treatmentDescription :: Prelude.Maybe Prelude.Text
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
-- 'additionalTreatments', 'writeCampaignRequest_additionalTreatments' - An array of requests that defines additional treatments for the
-- campaign, in addition to the default treatment for the campaign.
--
-- 'hook', 'writeCampaignRequest_hook' - The settings for the AWS Lambda function to invoke as a code hook for
-- the campaign. You can use this hook to customize the segment that\'s
-- used by the campaign.
--
-- 'customDeliveryConfiguration', 'writeCampaignRequest_customDeliveryConfiguration' - The delivery configuration settings for sending the campaign through a
-- custom channel. This object is required if the MessageConfiguration
-- object for the campaign specifies a CustomMessage object.
--
-- 'name', 'writeCampaignRequest_name' - A custom name for the campaign.
--
-- 'isPaused', 'writeCampaignRequest_isPaused' - Specifies whether to pause the campaign. A paused campaign doesn\'t run
-- unless you resume it by changing this value to false.
--
-- 'tags', 'writeCampaignRequest_tags' - A string-to-string map of key-value pairs that defines the tags to
-- associate with the campaign. Each tag consists of a required tag key and
-- an associated tag value.
--
-- 'segmentVersion', 'writeCampaignRequest_segmentVersion' - The version of the segment to associate with the campaign.
--
-- 'limits', 'writeCampaignRequest_limits' - The messaging limits for the campaign.
--
-- 'segmentId', 'writeCampaignRequest_segmentId' - The unique identifier for the segment to associate with the campaign.
--
-- 'description', 'writeCampaignRequest_description' - A custom description of the campaign.
--
-- 'treatmentName', 'writeCampaignRequest_treatmentName' - A custom name of the default treatment for the campaign, if the campaign
-- has multiple treatments. A /treatment/ is a variation of a campaign
-- that\'s used for A\/B testing.
--
-- 'messageConfiguration', 'writeCampaignRequest_messageConfiguration' - The message configuration settings for the campaign.
--
-- 'templateConfiguration', 'writeCampaignRequest_templateConfiguration' - The message template to use for the campaign.
--
-- 'schedule', 'writeCampaignRequest_schedule' - The schedule settings for the campaign.
--
-- 'holdoutPercent', 'writeCampaignRequest_holdoutPercent' - The allocated percentage of users (segment members) who shouldn\'t
-- receive messages from the campaign.
--
-- 'treatmentDescription', 'writeCampaignRequest_treatmentDescription' - A custom description of the default treatment for the campaign.
newWriteCampaignRequest ::
  WriteCampaignRequest
newWriteCampaignRequest =
  WriteCampaignRequest'
    { additionalTreatments =
        Prelude.Nothing,
      hook = Prelude.Nothing,
      customDeliveryConfiguration = Prelude.Nothing,
      name = Prelude.Nothing,
      isPaused = Prelude.Nothing,
      tags = Prelude.Nothing,
      segmentVersion = Prelude.Nothing,
      limits = Prelude.Nothing,
      segmentId = Prelude.Nothing,
      description = Prelude.Nothing,
      treatmentName = Prelude.Nothing,
      messageConfiguration = Prelude.Nothing,
      templateConfiguration = Prelude.Nothing,
      schedule = Prelude.Nothing,
      holdoutPercent = Prelude.Nothing,
      treatmentDescription = Prelude.Nothing
    }

-- | An array of requests that defines additional treatments for the
-- campaign, in addition to the default treatment for the campaign.
writeCampaignRequest_additionalTreatments :: Lens.Lens' WriteCampaignRequest (Prelude.Maybe [WriteTreatmentResource])
writeCampaignRequest_additionalTreatments = Lens.lens (\WriteCampaignRequest' {additionalTreatments} -> additionalTreatments) (\s@WriteCampaignRequest' {} a -> s {additionalTreatments = a} :: WriteCampaignRequest) Prelude.. Lens.mapping Lens._Coerce

-- | The settings for the AWS Lambda function to invoke as a code hook for
-- the campaign. You can use this hook to customize the segment that\'s
-- used by the campaign.
writeCampaignRequest_hook :: Lens.Lens' WriteCampaignRequest (Prelude.Maybe CampaignHook)
writeCampaignRequest_hook = Lens.lens (\WriteCampaignRequest' {hook} -> hook) (\s@WriteCampaignRequest' {} a -> s {hook = a} :: WriteCampaignRequest)

-- | The delivery configuration settings for sending the campaign through a
-- custom channel. This object is required if the MessageConfiguration
-- object for the campaign specifies a CustomMessage object.
writeCampaignRequest_customDeliveryConfiguration :: Lens.Lens' WriteCampaignRequest (Prelude.Maybe CustomDeliveryConfiguration)
writeCampaignRequest_customDeliveryConfiguration = Lens.lens (\WriteCampaignRequest' {customDeliveryConfiguration} -> customDeliveryConfiguration) (\s@WriteCampaignRequest' {} a -> s {customDeliveryConfiguration = a} :: WriteCampaignRequest)

-- | A custom name for the campaign.
writeCampaignRequest_name :: Lens.Lens' WriteCampaignRequest (Prelude.Maybe Prelude.Text)
writeCampaignRequest_name = Lens.lens (\WriteCampaignRequest' {name} -> name) (\s@WriteCampaignRequest' {} a -> s {name = a} :: WriteCampaignRequest)

-- | Specifies whether to pause the campaign. A paused campaign doesn\'t run
-- unless you resume it by changing this value to false.
writeCampaignRequest_isPaused :: Lens.Lens' WriteCampaignRequest (Prelude.Maybe Prelude.Bool)
writeCampaignRequest_isPaused = Lens.lens (\WriteCampaignRequest' {isPaused} -> isPaused) (\s@WriteCampaignRequest' {} a -> s {isPaused = a} :: WriteCampaignRequest)

-- | A string-to-string map of key-value pairs that defines the tags to
-- associate with the campaign. Each tag consists of a required tag key and
-- an associated tag value.
writeCampaignRequest_tags :: Lens.Lens' WriteCampaignRequest (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
writeCampaignRequest_tags = Lens.lens (\WriteCampaignRequest' {tags} -> tags) (\s@WriteCampaignRequest' {} a -> s {tags = a} :: WriteCampaignRequest) Prelude.. Lens.mapping Lens._Coerce

-- | The version of the segment to associate with the campaign.
writeCampaignRequest_segmentVersion :: Lens.Lens' WriteCampaignRequest (Prelude.Maybe Prelude.Int)
writeCampaignRequest_segmentVersion = Lens.lens (\WriteCampaignRequest' {segmentVersion} -> segmentVersion) (\s@WriteCampaignRequest' {} a -> s {segmentVersion = a} :: WriteCampaignRequest)

-- | The messaging limits for the campaign.
writeCampaignRequest_limits :: Lens.Lens' WriteCampaignRequest (Prelude.Maybe CampaignLimits)
writeCampaignRequest_limits = Lens.lens (\WriteCampaignRequest' {limits} -> limits) (\s@WriteCampaignRequest' {} a -> s {limits = a} :: WriteCampaignRequest)

-- | The unique identifier for the segment to associate with the campaign.
writeCampaignRequest_segmentId :: Lens.Lens' WriteCampaignRequest (Prelude.Maybe Prelude.Text)
writeCampaignRequest_segmentId = Lens.lens (\WriteCampaignRequest' {segmentId} -> segmentId) (\s@WriteCampaignRequest' {} a -> s {segmentId = a} :: WriteCampaignRequest)

-- | A custom description of the campaign.
writeCampaignRequest_description :: Lens.Lens' WriteCampaignRequest (Prelude.Maybe Prelude.Text)
writeCampaignRequest_description = Lens.lens (\WriteCampaignRequest' {description} -> description) (\s@WriteCampaignRequest' {} a -> s {description = a} :: WriteCampaignRequest)

-- | A custom name of the default treatment for the campaign, if the campaign
-- has multiple treatments. A /treatment/ is a variation of a campaign
-- that\'s used for A\/B testing.
writeCampaignRequest_treatmentName :: Lens.Lens' WriteCampaignRequest (Prelude.Maybe Prelude.Text)
writeCampaignRequest_treatmentName = Lens.lens (\WriteCampaignRequest' {treatmentName} -> treatmentName) (\s@WriteCampaignRequest' {} a -> s {treatmentName = a} :: WriteCampaignRequest)

-- | The message configuration settings for the campaign.
writeCampaignRequest_messageConfiguration :: Lens.Lens' WriteCampaignRequest (Prelude.Maybe MessageConfiguration)
writeCampaignRequest_messageConfiguration = Lens.lens (\WriteCampaignRequest' {messageConfiguration} -> messageConfiguration) (\s@WriteCampaignRequest' {} a -> s {messageConfiguration = a} :: WriteCampaignRequest)

-- | The message template to use for the campaign.
writeCampaignRequest_templateConfiguration :: Lens.Lens' WriteCampaignRequest (Prelude.Maybe TemplateConfiguration)
writeCampaignRequest_templateConfiguration = Lens.lens (\WriteCampaignRequest' {templateConfiguration} -> templateConfiguration) (\s@WriteCampaignRequest' {} a -> s {templateConfiguration = a} :: WriteCampaignRequest)

-- | The schedule settings for the campaign.
writeCampaignRequest_schedule :: Lens.Lens' WriteCampaignRequest (Prelude.Maybe Schedule)
writeCampaignRequest_schedule = Lens.lens (\WriteCampaignRequest' {schedule} -> schedule) (\s@WriteCampaignRequest' {} a -> s {schedule = a} :: WriteCampaignRequest)

-- | The allocated percentage of users (segment members) who shouldn\'t
-- receive messages from the campaign.
writeCampaignRequest_holdoutPercent :: Lens.Lens' WriteCampaignRequest (Prelude.Maybe Prelude.Int)
writeCampaignRequest_holdoutPercent = Lens.lens (\WriteCampaignRequest' {holdoutPercent} -> holdoutPercent) (\s@WriteCampaignRequest' {} a -> s {holdoutPercent = a} :: WriteCampaignRequest)

-- | A custom description of the default treatment for the campaign.
writeCampaignRequest_treatmentDescription :: Lens.Lens' WriteCampaignRequest (Prelude.Maybe Prelude.Text)
writeCampaignRequest_treatmentDescription = Lens.lens (\WriteCampaignRequest' {treatmentDescription} -> treatmentDescription) (\s@WriteCampaignRequest' {} a -> s {treatmentDescription = a} :: WriteCampaignRequest)

instance Prelude.Hashable WriteCampaignRequest

instance Prelude.NFData WriteCampaignRequest

instance Core.ToJSON WriteCampaignRequest where
  toJSON WriteCampaignRequest' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("AdditionalTreatments" Core..=)
              Prelude.<$> additionalTreatments,
            ("Hook" Core..=) Prelude.<$> hook,
            ("CustomDeliveryConfiguration" Core..=)
              Prelude.<$> customDeliveryConfiguration,
            ("Name" Core..=) Prelude.<$> name,
            ("IsPaused" Core..=) Prelude.<$> isPaused,
            ("tags" Core..=) Prelude.<$> tags,
            ("SegmentVersion" Core..=)
              Prelude.<$> segmentVersion,
            ("Limits" Core..=) Prelude.<$> limits,
            ("SegmentId" Core..=) Prelude.<$> segmentId,
            ("Description" Core..=) Prelude.<$> description,
            ("TreatmentName" Core..=) Prelude.<$> treatmentName,
            ("MessageConfiguration" Core..=)
              Prelude.<$> messageConfiguration,
            ("TemplateConfiguration" Core..=)
              Prelude.<$> templateConfiguration,
            ("Schedule" Core..=) Prelude.<$> schedule,
            ("HoldoutPercent" Core..=)
              Prelude.<$> holdoutPercent,
            ("TreatmentDescription" Core..=)
              Prelude.<$> treatmentDescription
          ]
      )
