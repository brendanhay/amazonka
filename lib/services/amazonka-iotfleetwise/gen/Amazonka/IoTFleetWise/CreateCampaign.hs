{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.IoTFleetWise.CreateCampaign
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an orchestration of data collection rules. The Amazon Web
-- Services IoT FleetWise Edge Agent software running in vehicles uses
-- campaigns to decide how to collect and transfer data to the cloud. You
-- create campaigns in the cloud. After you or your team approve campaigns,
-- Amazon Web Services IoT FleetWise automatically deploys them to
-- vehicles.
--
-- For more information, see
-- <https://docs.aws.amazon.com/iot-fleetwise/latest/developerguide/campaigns.html Collect and transfer data with campaigns>
-- in the /Amazon Web Services IoT FleetWise Developer Guide/.
module Amazonka.IoTFleetWise.CreateCampaign
  ( -- * Creating a Request
    CreateCampaign (..),
    newCreateCampaign,

    -- * Request Lenses
    createCampaign_tags,
    createCampaign_compression,
    createCampaign_dataExtraDimensions,
    createCampaign_expiryTime,
    createCampaign_diagnosticsMode,
    createCampaign_description,
    createCampaign_spoolingMode,
    createCampaign_postTriggerCollectionDuration,
    createCampaign_priority,
    createCampaign_startTime,
    createCampaign_signalsToCollect,
    createCampaign_name,
    createCampaign_signalCatalogArn,
    createCampaign_targetArn,
    createCampaign_collectionScheme,

    -- * Destructuring the Response
    CreateCampaignResponse (..),
    newCreateCampaignResponse,

    -- * Response Lenses
    createCampaignResponse_name,
    createCampaignResponse_arn,
    createCampaignResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoTFleetWise.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateCampaign' smart constructor.
data CreateCampaign = CreateCampaign'
  { -- | Metadata that can be used to manage the campaign.
    tags :: Prelude.Maybe [Tag],
    -- | (Optional) Whether to compress signals before transmitting data to
    -- Amazon Web Services IoT FleetWise. If you don\'t want to compress the
    -- signals, use @OFF@. If it\'s not specified, @SNAPPY@ is used.
    --
    -- Default: @SNAPPY@
    compression :: Prelude.Maybe Compression,
    -- | (Optional) A list of vehicle attributes to associate with a campaign.
    --
    -- Enrich the data with specified vehicle attributes. For example, add
    -- @make@ and @model@ to the campaign, and Amazon Web Services IoT
    -- FleetWise will associate the data with those attributes as dimensions in
    -- Amazon Timestream. You can then query the data against @make@ and
    -- @model@.
    --
    -- Default: An empty array
    dataExtraDimensions :: Prelude.Maybe [Prelude.Text],
    -- | (Optional) The time the campaign expires, in seconds since epoch
    -- (January 1, 1970 at midnight UTC time). Vehicle data won\'t be collected
    -- after the campaign expires.
    --
    -- Default: 253402243200 (December 31, 9999, 00:00:00 UTC)
    expiryTime :: Prelude.Maybe Core.POSIX,
    -- | (Optional) Option for a vehicle to send diagnostic trouble codes to
    -- Amazon Web Services IoT FleetWise. If you want to send diagnostic
    -- trouble codes, use @SEND_ACTIVE_DTCS@. If it\'s not specified, @OFF@ is
    -- used.
    --
    -- Default: @OFF@
    diagnosticsMode :: Prelude.Maybe DiagnosticsMode,
    -- | An optional description of the campaign to help identify its purpose.
    description :: Prelude.Maybe Prelude.Text,
    -- | (Optional) Whether to store collected data after a vehicle lost a
    -- connection with the cloud. After a connection is re-established, the
    -- data is automatically forwarded to Amazon Web Services IoT FleetWise. If
    -- you want to store collected data when a vehicle loses connection with
    -- the cloud, use @TO_DISK@. If it\'s not specified, @OFF@ is used.
    --
    -- Default: @OFF@
    spoolingMode :: Prelude.Maybe SpoolingMode,
    -- | (Optional) How long (in milliseconds) to collect raw data after a
    -- triggering event initiates the collection. If it\'s not specified, @0@
    -- is used.
    --
    -- Default: @0@
    postTriggerCollectionDuration :: Prelude.Maybe Prelude.Natural,
    -- | (Optional) A number indicating the priority of one campaign over another
    -- campaign for a certain vehicle or fleet. A campaign with the lowest
    -- value is deployed to vehicles before any other campaigns. If it\'s not
    -- specified, @0@ is used.
    --
    -- Default: @0@
    priority :: Prelude.Maybe Prelude.Natural,
    -- | (Optional) The time, in milliseconds, to deliver a campaign after it was
    -- approved. If it\'s not specified, @0@ is used.
    --
    -- Default: @0@
    startTime :: Prelude.Maybe Core.POSIX,
    -- | (Optional) A list of information about signals to collect.
    signalsToCollect :: Prelude.Maybe [SignalInformation],
    -- | The name of the campaign to create.
    name :: Prelude.Text,
    -- | (Optional) The Amazon Resource Name (ARN) of the signal catalog to
    -- associate with the campaign.
    signalCatalogArn :: Prelude.Text,
    -- | The ARN of the vehicle or fleet to deploy a campaign to.
    targetArn :: Prelude.Text,
    -- | The data collection scheme associated with the campaign. You can specify
    -- a scheme that collects data based on time or an event.
    collectionScheme :: CollectionScheme
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateCampaign' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createCampaign_tags' - Metadata that can be used to manage the campaign.
--
-- 'compression', 'createCampaign_compression' - (Optional) Whether to compress signals before transmitting data to
-- Amazon Web Services IoT FleetWise. If you don\'t want to compress the
-- signals, use @OFF@. If it\'s not specified, @SNAPPY@ is used.
--
-- Default: @SNAPPY@
--
-- 'dataExtraDimensions', 'createCampaign_dataExtraDimensions' - (Optional) A list of vehicle attributes to associate with a campaign.
--
-- Enrich the data with specified vehicle attributes. For example, add
-- @make@ and @model@ to the campaign, and Amazon Web Services IoT
-- FleetWise will associate the data with those attributes as dimensions in
-- Amazon Timestream. You can then query the data against @make@ and
-- @model@.
--
-- Default: An empty array
--
-- 'expiryTime', 'createCampaign_expiryTime' - (Optional) The time the campaign expires, in seconds since epoch
-- (January 1, 1970 at midnight UTC time). Vehicle data won\'t be collected
-- after the campaign expires.
--
-- Default: 253402243200 (December 31, 9999, 00:00:00 UTC)
--
-- 'diagnosticsMode', 'createCampaign_diagnosticsMode' - (Optional) Option for a vehicle to send diagnostic trouble codes to
-- Amazon Web Services IoT FleetWise. If you want to send diagnostic
-- trouble codes, use @SEND_ACTIVE_DTCS@. If it\'s not specified, @OFF@ is
-- used.
--
-- Default: @OFF@
--
-- 'description', 'createCampaign_description' - An optional description of the campaign to help identify its purpose.
--
-- 'spoolingMode', 'createCampaign_spoolingMode' - (Optional) Whether to store collected data after a vehicle lost a
-- connection with the cloud. After a connection is re-established, the
-- data is automatically forwarded to Amazon Web Services IoT FleetWise. If
-- you want to store collected data when a vehicle loses connection with
-- the cloud, use @TO_DISK@. If it\'s not specified, @OFF@ is used.
--
-- Default: @OFF@
--
-- 'postTriggerCollectionDuration', 'createCampaign_postTriggerCollectionDuration' - (Optional) How long (in milliseconds) to collect raw data after a
-- triggering event initiates the collection. If it\'s not specified, @0@
-- is used.
--
-- Default: @0@
--
-- 'priority', 'createCampaign_priority' - (Optional) A number indicating the priority of one campaign over another
-- campaign for a certain vehicle or fleet. A campaign with the lowest
-- value is deployed to vehicles before any other campaigns. If it\'s not
-- specified, @0@ is used.
--
-- Default: @0@
--
-- 'startTime', 'createCampaign_startTime' - (Optional) The time, in milliseconds, to deliver a campaign after it was
-- approved. If it\'s not specified, @0@ is used.
--
-- Default: @0@
--
-- 'signalsToCollect', 'createCampaign_signalsToCollect' - (Optional) A list of information about signals to collect.
--
-- 'name', 'createCampaign_name' - The name of the campaign to create.
--
-- 'signalCatalogArn', 'createCampaign_signalCatalogArn' - (Optional) The Amazon Resource Name (ARN) of the signal catalog to
-- associate with the campaign.
--
-- 'targetArn', 'createCampaign_targetArn' - The ARN of the vehicle or fleet to deploy a campaign to.
--
-- 'collectionScheme', 'createCampaign_collectionScheme' - The data collection scheme associated with the campaign. You can specify
-- a scheme that collects data based on time or an event.
newCreateCampaign ::
  -- | 'name'
  Prelude.Text ->
  -- | 'signalCatalogArn'
  Prelude.Text ->
  -- | 'targetArn'
  Prelude.Text ->
  -- | 'collectionScheme'
  CollectionScheme ->
  CreateCampaign
newCreateCampaign
  pName_
  pSignalCatalogArn_
  pTargetArn_
  pCollectionScheme_ =
    CreateCampaign'
      { tags = Prelude.Nothing,
        compression = Prelude.Nothing,
        dataExtraDimensions = Prelude.Nothing,
        expiryTime = Prelude.Nothing,
        diagnosticsMode = Prelude.Nothing,
        description = Prelude.Nothing,
        spoolingMode = Prelude.Nothing,
        postTriggerCollectionDuration = Prelude.Nothing,
        priority = Prelude.Nothing,
        startTime = Prelude.Nothing,
        signalsToCollect = Prelude.Nothing,
        name = pName_,
        signalCatalogArn = pSignalCatalogArn_,
        targetArn = pTargetArn_,
        collectionScheme = pCollectionScheme_
      }

-- | Metadata that can be used to manage the campaign.
createCampaign_tags :: Lens.Lens' CreateCampaign (Prelude.Maybe [Tag])
createCampaign_tags = Lens.lens (\CreateCampaign' {tags} -> tags) (\s@CreateCampaign' {} a -> s {tags = a} :: CreateCampaign) Prelude.. Lens.mapping Lens.coerced

-- | (Optional) Whether to compress signals before transmitting data to
-- Amazon Web Services IoT FleetWise. If you don\'t want to compress the
-- signals, use @OFF@. If it\'s not specified, @SNAPPY@ is used.
--
-- Default: @SNAPPY@
createCampaign_compression :: Lens.Lens' CreateCampaign (Prelude.Maybe Compression)
createCampaign_compression = Lens.lens (\CreateCampaign' {compression} -> compression) (\s@CreateCampaign' {} a -> s {compression = a} :: CreateCampaign)

-- | (Optional) A list of vehicle attributes to associate with a campaign.
--
-- Enrich the data with specified vehicle attributes. For example, add
-- @make@ and @model@ to the campaign, and Amazon Web Services IoT
-- FleetWise will associate the data with those attributes as dimensions in
-- Amazon Timestream. You can then query the data against @make@ and
-- @model@.
--
-- Default: An empty array
createCampaign_dataExtraDimensions :: Lens.Lens' CreateCampaign (Prelude.Maybe [Prelude.Text])
createCampaign_dataExtraDimensions = Lens.lens (\CreateCampaign' {dataExtraDimensions} -> dataExtraDimensions) (\s@CreateCampaign' {} a -> s {dataExtraDimensions = a} :: CreateCampaign) Prelude.. Lens.mapping Lens.coerced

-- | (Optional) The time the campaign expires, in seconds since epoch
-- (January 1, 1970 at midnight UTC time). Vehicle data won\'t be collected
-- after the campaign expires.
--
-- Default: 253402243200 (December 31, 9999, 00:00:00 UTC)
createCampaign_expiryTime :: Lens.Lens' CreateCampaign (Prelude.Maybe Prelude.UTCTime)
createCampaign_expiryTime = Lens.lens (\CreateCampaign' {expiryTime} -> expiryTime) (\s@CreateCampaign' {} a -> s {expiryTime = a} :: CreateCampaign) Prelude.. Lens.mapping Core._Time

-- | (Optional) Option for a vehicle to send diagnostic trouble codes to
-- Amazon Web Services IoT FleetWise. If you want to send diagnostic
-- trouble codes, use @SEND_ACTIVE_DTCS@. If it\'s not specified, @OFF@ is
-- used.
--
-- Default: @OFF@
createCampaign_diagnosticsMode :: Lens.Lens' CreateCampaign (Prelude.Maybe DiagnosticsMode)
createCampaign_diagnosticsMode = Lens.lens (\CreateCampaign' {diagnosticsMode} -> diagnosticsMode) (\s@CreateCampaign' {} a -> s {diagnosticsMode = a} :: CreateCampaign)

-- | An optional description of the campaign to help identify its purpose.
createCampaign_description :: Lens.Lens' CreateCampaign (Prelude.Maybe Prelude.Text)
createCampaign_description = Lens.lens (\CreateCampaign' {description} -> description) (\s@CreateCampaign' {} a -> s {description = a} :: CreateCampaign)

-- | (Optional) Whether to store collected data after a vehicle lost a
-- connection with the cloud. After a connection is re-established, the
-- data is automatically forwarded to Amazon Web Services IoT FleetWise. If
-- you want to store collected data when a vehicle loses connection with
-- the cloud, use @TO_DISK@. If it\'s not specified, @OFF@ is used.
--
-- Default: @OFF@
createCampaign_spoolingMode :: Lens.Lens' CreateCampaign (Prelude.Maybe SpoolingMode)
createCampaign_spoolingMode = Lens.lens (\CreateCampaign' {spoolingMode} -> spoolingMode) (\s@CreateCampaign' {} a -> s {spoolingMode = a} :: CreateCampaign)

-- | (Optional) How long (in milliseconds) to collect raw data after a
-- triggering event initiates the collection. If it\'s not specified, @0@
-- is used.
--
-- Default: @0@
createCampaign_postTriggerCollectionDuration :: Lens.Lens' CreateCampaign (Prelude.Maybe Prelude.Natural)
createCampaign_postTriggerCollectionDuration = Lens.lens (\CreateCampaign' {postTriggerCollectionDuration} -> postTriggerCollectionDuration) (\s@CreateCampaign' {} a -> s {postTriggerCollectionDuration = a} :: CreateCampaign)

-- | (Optional) A number indicating the priority of one campaign over another
-- campaign for a certain vehicle or fleet. A campaign with the lowest
-- value is deployed to vehicles before any other campaigns. If it\'s not
-- specified, @0@ is used.
--
-- Default: @0@
createCampaign_priority :: Lens.Lens' CreateCampaign (Prelude.Maybe Prelude.Natural)
createCampaign_priority = Lens.lens (\CreateCampaign' {priority} -> priority) (\s@CreateCampaign' {} a -> s {priority = a} :: CreateCampaign)

-- | (Optional) The time, in milliseconds, to deliver a campaign after it was
-- approved. If it\'s not specified, @0@ is used.
--
-- Default: @0@
createCampaign_startTime :: Lens.Lens' CreateCampaign (Prelude.Maybe Prelude.UTCTime)
createCampaign_startTime = Lens.lens (\CreateCampaign' {startTime} -> startTime) (\s@CreateCampaign' {} a -> s {startTime = a} :: CreateCampaign) Prelude.. Lens.mapping Core._Time

-- | (Optional) A list of information about signals to collect.
createCampaign_signalsToCollect :: Lens.Lens' CreateCampaign (Prelude.Maybe [SignalInformation])
createCampaign_signalsToCollect = Lens.lens (\CreateCampaign' {signalsToCollect} -> signalsToCollect) (\s@CreateCampaign' {} a -> s {signalsToCollect = a} :: CreateCampaign) Prelude.. Lens.mapping Lens.coerced

-- | The name of the campaign to create.
createCampaign_name :: Lens.Lens' CreateCampaign Prelude.Text
createCampaign_name = Lens.lens (\CreateCampaign' {name} -> name) (\s@CreateCampaign' {} a -> s {name = a} :: CreateCampaign)

-- | (Optional) The Amazon Resource Name (ARN) of the signal catalog to
-- associate with the campaign.
createCampaign_signalCatalogArn :: Lens.Lens' CreateCampaign Prelude.Text
createCampaign_signalCatalogArn = Lens.lens (\CreateCampaign' {signalCatalogArn} -> signalCatalogArn) (\s@CreateCampaign' {} a -> s {signalCatalogArn = a} :: CreateCampaign)

-- | The ARN of the vehicle or fleet to deploy a campaign to.
createCampaign_targetArn :: Lens.Lens' CreateCampaign Prelude.Text
createCampaign_targetArn = Lens.lens (\CreateCampaign' {targetArn} -> targetArn) (\s@CreateCampaign' {} a -> s {targetArn = a} :: CreateCampaign)

-- | The data collection scheme associated with the campaign. You can specify
-- a scheme that collects data based on time or an event.
createCampaign_collectionScheme :: Lens.Lens' CreateCampaign CollectionScheme
createCampaign_collectionScheme = Lens.lens (\CreateCampaign' {collectionScheme} -> collectionScheme) (\s@CreateCampaign' {} a -> s {collectionScheme = a} :: CreateCampaign)

instance Core.AWSRequest CreateCampaign where
  type
    AWSResponse CreateCampaign =
      CreateCampaignResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateCampaignResponse'
            Prelude.<$> (x Core..?> "name")
            Prelude.<*> (x Core..?> "arn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateCampaign where
  hashWithSalt _salt CreateCampaign' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` compression
      `Prelude.hashWithSalt` dataExtraDimensions
      `Prelude.hashWithSalt` expiryTime
      `Prelude.hashWithSalt` diagnosticsMode
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` spoolingMode
      `Prelude.hashWithSalt` postTriggerCollectionDuration
      `Prelude.hashWithSalt` priority
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` signalsToCollect
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` signalCatalogArn
      `Prelude.hashWithSalt` targetArn
      `Prelude.hashWithSalt` collectionScheme

instance Prelude.NFData CreateCampaign where
  rnf CreateCampaign' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf compression
      `Prelude.seq` Prelude.rnf dataExtraDimensions
      `Prelude.seq` Prelude.rnf expiryTime
      `Prelude.seq` Prelude.rnf diagnosticsMode
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf spoolingMode
      `Prelude.seq` Prelude.rnf postTriggerCollectionDuration
      `Prelude.seq` Prelude.rnf priority
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf signalsToCollect
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf signalCatalogArn
      `Prelude.seq` Prelude.rnf targetArn
      `Prelude.seq` Prelude.rnf collectionScheme

instance Core.ToHeaders CreateCampaign where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "IoTAutobahnControlPlane.CreateCampaign" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateCampaign where
  toJSON CreateCampaign' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("tags" Core..=) Prelude.<$> tags,
            ("compression" Core..=) Prelude.<$> compression,
            ("dataExtraDimensions" Core..=)
              Prelude.<$> dataExtraDimensions,
            ("expiryTime" Core..=) Prelude.<$> expiryTime,
            ("diagnosticsMode" Core..=)
              Prelude.<$> diagnosticsMode,
            ("description" Core..=) Prelude.<$> description,
            ("spoolingMode" Core..=) Prelude.<$> spoolingMode,
            ("postTriggerCollectionDuration" Core..=)
              Prelude.<$> postTriggerCollectionDuration,
            ("priority" Core..=) Prelude.<$> priority,
            ("startTime" Core..=) Prelude.<$> startTime,
            ("signalsToCollect" Core..=)
              Prelude.<$> signalsToCollect,
            Prelude.Just ("name" Core..= name),
            Prelude.Just
              ("signalCatalogArn" Core..= signalCatalogArn),
            Prelude.Just ("targetArn" Core..= targetArn),
            Prelude.Just
              ("collectionScheme" Core..= collectionScheme)
          ]
      )

instance Core.ToPath CreateCampaign where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateCampaign where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateCampaignResponse' smart constructor.
data CreateCampaignResponse = CreateCampaignResponse'
  { -- | The name of the created campaign.
    name :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the created campaign.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateCampaignResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'createCampaignResponse_name' - The name of the created campaign.
--
-- 'arn', 'createCampaignResponse_arn' - The ARN of the created campaign.
--
-- 'httpStatus', 'createCampaignResponse_httpStatus' - The response's http status code.
newCreateCampaignResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateCampaignResponse
newCreateCampaignResponse pHttpStatus_ =
  CreateCampaignResponse'
    { name = Prelude.Nothing,
      arn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The name of the created campaign.
createCampaignResponse_name :: Lens.Lens' CreateCampaignResponse (Prelude.Maybe Prelude.Text)
createCampaignResponse_name = Lens.lens (\CreateCampaignResponse' {name} -> name) (\s@CreateCampaignResponse' {} a -> s {name = a} :: CreateCampaignResponse)

-- | The ARN of the created campaign.
createCampaignResponse_arn :: Lens.Lens' CreateCampaignResponse (Prelude.Maybe Prelude.Text)
createCampaignResponse_arn = Lens.lens (\CreateCampaignResponse' {arn} -> arn) (\s@CreateCampaignResponse' {} a -> s {arn = a} :: CreateCampaignResponse)

-- | The response's http status code.
createCampaignResponse_httpStatus :: Lens.Lens' CreateCampaignResponse Prelude.Int
createCampaignResponse_httpStatus = Lens.lens (\CreateCampaignResponse' {httpStatus} -> httpStatus) (\s@CreateCampaignResponse' {} a -> s {httpStatus = a} :: CreateCampaignResponse)

instance Prelude.NFData CreateCampaignResponse where
  rnf CreateCampaignResponse' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf httpStatus
