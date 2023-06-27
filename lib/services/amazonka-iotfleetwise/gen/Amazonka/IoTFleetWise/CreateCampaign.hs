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
-- Copyright   : (c) 2013-2023 Brendan Hay
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
    createCampaign_compression,
    createCampaign_dataDestinationConfigs,
    createCampaign_dataExtraDimensions,
    createCampaign_description,
    createCampaign_diagnosticsMode,
    createCampaign_expiryTime,
    createCampaign_postTriggerCollectionDuration,
    createCampaign_priority,
    createCampaign_signalsToCollect,
    createCampaign_spoolingMode,
    createCampaign_startTime,
    createCampaign_tags,
    createCampaign_name,
    createCampaign_signalCatalogArn,
    createCampaign_targetArn,
    createCampaign_collectionScheme,

    -- * Destructuring the Response
    CreateCampaignResponse (..),
    newCreateCampaignResponse,

    -- * Response Lenses
    createCampaignResponse_arn,
    createCampaignResponse_name,
    createCampaignResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTFleetWise.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateCampaign' smart constructor.
data CreateCampaign = CreateCampaign'
  { -- | (Optional) Whether to compress signals before transmitting data to
    -- Amazon Web Services IoT FleetWise. If you don\'t want to compress the
    -- signals, use @OFF@. If it\'s not specified, @SNAPPY@ is used.
    --
    -- Default: @SNAPPY@
    compression :: Prelude.Maybe Compression,
    -- | The destination where the campaign sends data. You can choose to send
    -- data to be stored in Amazon S3 or Amazon Timestream.
    --
    -- Amazon S3 optimizes the cost of data storage and provides additional
    -- mechanisms to use vehicle data, such as data lakes, centralized data
    -- storage, data processing pipelines, and analytics.
    --
    -- You can use Amazon Timestream to access and analyze time series data,
    -- and Timestream to query vehicle data so that you can identify trends and
    -- patterns.
    dataDestinationConfigs :: Prelude.Maybe (Prelude.NonEmpty DataDestinationConfig),
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
    -- | An optional description of the campaign to help identify its purpose.
    description :: Prelude.Maybe Prelude.Text,
    -- | (Optional) Option for a vehicle to send diagnostic trouble codes to
    -- Amazon Web Services IoT FleetWise. If you want to send diagnostic
    -- trouble codes, use @SEND_ACTIVE_DTCS@. If it\'s not specified, @OFF@ is
    -- used.
    --
    -- Default: @OFF@
    diagnosticsMode :: Prelude.Maybe DiagnosticsMode,
    -- | (Optional) The time the campaign expires, in seconds since epoch
    -- (January 1, 1970 at midnight UTC time). Vehicle data isn\'t collected
    -- after the campaign expires.
    --
    -- Default: 253402214400 (December 31, 9999, 00:00:00 UTC)
    expiryTime :: Prelude.Maybe Data.POSIX,
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
    -- | (Optional) A list of information about signals to collect.
    signalsToCollect :: Prelude.Maybe [SignalInformation],
    -- | (Optional) Whether to store collected data after a vehicle lost a
    -- connection with the cloud. After a connection is re-established, the
    -- data is automatically forwarded to Amazon Web Services IoT FleetWise. If
    -- you want to store collected data when a vehicle loses connection with
    -- the cloud, use @TO_DISK@. If it\'s not specified, @OFF@ is used.
    --
    -- Default: @OFF@
    spoolingMode :: Prelude.Maybe SpoolingMode,
    -- | (Optional) The time, in milliseconds, to deliver a campaign after it was
    -- approved. If it\'s not specified, @0@ is used.
    --
    -- Default: @0@
    startTime :: Prelude.Maybe Data.POSIX,
    -- | Metadata that can be used to manage the campaign.
    tags :: Prelude.Maybe [Tag],
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
-- 'compression', 'createCampaign_compression' - (Optional) Whether to compress signals before transmitting data to
-- Amazon Web Services IoT FleetWise. If you don\'t want to compress the
-- signals, use @OFF@. If it\'s not specified, @SNAPPY@ is used.
--
-- Default: @SNAPPY@
--
-- 'dataDestinationConfigs', 'createCampaign_dataDestinationConfigs' - The destination where the campaign sends data. You can choose to send
-- data to be stored in Amazon S3 or Amazon Timestream.
--
-- Amazon S3 optimizes the cost of data storage and provides additional
-- mechanisms to use vehicle data, such as data lakes, centralized data
-- storage, data processing pipelines, and analytics.
--
-- You can use Amazon Timestream to access and analyze time series data,
-- and Timestream to query vehicle data so that you can identify trends and
-- patterns.
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
-- 'description', 'createCampaign_description' - An optional description of the campaign to help identify its purpose.
--
-- 'diagnosticsMode', 'createCampaign_diagnosticsMode' - (Optional) Option for a vehicle to send diagnostic trouble codes to
-- Amazon Web Services IoT FleetWise. If you want to send diagnostic
-- trouble codes, use @SEND_ACTIVE_DTCS@. If it\'s not specified, @OFF@ is
-- used.
--
-- Default: @OFF@
--
-- 'expiryTime', 'createCampaign_expiryTime' - (Optional) The time the campaign expires, in seconds since epoch
-- (January 1, 1970 at midnight UTC time). Vehicle data isn\'t collected
-- after the campaign expires.
--
-- Default: 253402214400 (December 31, 9999, 00:00:00 UTC)
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
-- 'signalsToCollect', 'createCampaign_signalsToCollect' - (Optional) A list of information about signals to collect.
--
-- 'spoolingMode', 'createCampaign_spoolingMode' - (Optional) Whether to store collected data after a vehicle lost a
-- connection with the cloud. After a connection is re-established, the
-- data is automatically forwarded to Amazon Web Services IoT FleetWise. If
-- you want to store collected data when a vehicle loses connection with
-- the cloud, use @TO_DISK@. If it\'s not specified, @OFF@ is used.
--
-- Default: @OFF@
--
-- 'startTime', 'createCampaign_startTime' - (Optional) The time, in milliseconds, to deliver a campaign after it was
-- approved. If it\'s not specified, @0@ is used.
--
-- Default: @0@
--
-- 'tags', 'createCampaign_tags' - Metadata that can be used to manage the campaign.
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
      { compression = Prelude.Nothing,
        dataDestinationConfigs = Prelude.Nothing,
        dataExtraDimensions = Prelude.Nothing,
        description = Prelude.Nothing,
        diagnosticsMode = Prelude.Nothing,
        expiryTime = Prelude.Nothing,
        postTriggerCollectionDuration = Prelude.Nothing,
        priority = Prelude.Nothing,
        signalsToCollect = Prelude.Nothing,
        spoolingMode = Prelude.Nothing,
        startTime = Prelude.Nothing,
        tags = Prelude.Nothing,
        name = pName_,
        signalCatalogArn = pSignalCatalogArn_,
        targetArn = pTargetArn_,
        collectionScheme = pCollectionScheme_
      }

-- | (Optional) Whether to compress signals before transmitting data to
-- Amazon Web Services IoT FleetWise. If you don\'t want to compress the
-- signals, use @OFF@. If it\'s not specified, @SNAPPY@ is used.
--
-- Default: @SNAPPY@
createCampaign_compression :: Lens.Lens' CreateCampaign (Prelude.Maybe Compression)
createCampaign_compression = Lens.lens (\CreateCampaign' {compression} -> compression) (\s@CreateCampaign' {} a -> s {compression = a} :: CreateCampaign)

-- | The destination where the campaign sends data. You can choose to send
-- data to be stored in Amazon S3 or Amazon Timestream.
--
-- Amazon S3 optimizes the cost of data storage and provides additional
-- mechanisms to use vehicle data, such as data lakes, centralized data
-- storage, data processing pipelines, and analytics.
--
-- You can use Amazon Timestream to access and analyze time series data,
-- and Timestream to query vehicle data so that you can identify trends and
-- patterns.
createCampaign_dataDestinationConfigs :: Lens.Lens' CreateCampaign (Prelude.Maybe (Prelude.NonEmpty DataDestinationConfig))
createCampaign_dataDestinationConfigs = Lens.lens (\CreateCampaign' {dataDestinationConfigs} -> dataDestinationConfigs) (\s@CreateCampaign' {} a -> s {dataDestinationConfigs = a} :: CreateCampaign) Prelude.. Lens.mapping Lens.coerced

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

-- | An optional description of the campaign to help identify its purpose.
createCampaign_description :: Lens.Lens' CreateCampaign (Prelude.Maybe Prelude.Text)
createCampaign_description = Lens.lens (\CreateCampaign' {description} -> description) (\s@CreateCampaign' {} a -> s {description = a} :: CreateCampaign)

-- | (Optional) Option for a vehicle to send diagnostic trouble codes to
-- Amazon Web Services IoT FleetWise. If you want to send diagnostic
-- trouble codes, use @SEND_ACTIVE_DTCS@. If it\'s not specified, @OFF@ is
-- used.
--
-- Default: @OFF@
createCampaign_diagnosticsMode :: Lens.Lens' CreateCampaign (Prelude.Maybe DiagnosticsMode)
createCampaign_diagnosticsMode = Lens.lens (\CreateCampaign' {diagnosticsMode} -> diagnosticsMode) (\s@CreateCampaign' {} a -> s {diagnosticsMode = a} :: CreateCampaign)

-- | (Optional) The time the campaign expires, in seconds since epoch
-- (January 1, 1970 at midnight UTC time). Vehicle data isn\'t collected
-- after the campaign expires.
--
-- Default: 253402214400 (December 31, 9999, 00:00:00 UTC)
createCampaign_expiryTime :: Lens.Lens' CreateCampaign (Prelude.Maybe Prelude.UTCTime)
createCampaign_expiryTime = Lens.lens (\CreateCampaign' {expiryTime} -> expiryTime) (\s@CreateCampaign' {} a -> s {expiryTime = a} :: CreateCampaign) Prelude.. Lens.mapping Data._Time

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

-- | (Optional) A list of information about signals to collect.
createCampaign_signalsToCollect :: Lens.Lens' CreateCampaign (Prelude.Maybe [SignalInformation])
createCampaign_signalsToCollect = Lens.lens (\CreateCampaign' {signalsToCollect} -> signalsToCollect) (\s@CreateCampaign' {} a -> s {signalsToCollect = a} :: CreateCampaign) Prelude.. Lens.mapping Lens.coerced

-- | (Optional) Whether to store collected data after a vehicle lost a
-- connection with the cloud. After a connection is re-established, the
-- data is automatically forwarded to Amazon Web Services IoT FleetWise. If
-- you want to store collected data when a vehicle loses connection with
-- the cloud, use @TO_DISK@. If it\'s not specified, @OFF@ is used.
--
-- Default: @OFF@
createCampaign_spoolingMode :: Lens.Lens' CreateCampaign (Prelude.Maybe SpoolingMode)
createCampaign_spoolingMode = Lens.lens (\CreateCampaign' {spoolingMode} -> spoolingMode) (\s@CreateCampaign' {} a -> s {spoolingMode = a} :: CreateCampaign)

-- | (Optional) The time, in milliseconds, to deliver a campaign after it was
-- approved. If it\'s not specified, @0@ is used.
--
-- Default: @0@
createCampaign_startTime :: Lens.Lens' CreateCampaign (Prelude.Maybe Prelude.UTCTime)
createCampaign_startTime = Lens.lens (\CreateCampaign' {startTime} -> startTime) (\s@CreateCampaign' {} a -> s {startTime = a} :: CreateCampaign) Prelude.. Lens.mapping Data._Time

-- | Metadata that can be used to manage the campaign.
createCampaign_tags :: Lens.Lens' CreateCampaign (Prelude.Maybe [Tag])
createCampaign_tags = Lens.lens (\CreateCampaign' {tags} -> tags) (\s@CreateCampaign' {} a -> s {tags = a} :: CreateCampaign) Prelude.. Lens.mapping Lens.coerced

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
            Prelude.<$> (x Data..?> "arn")
            Prelude.<*> (x Data..?> "name")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateCampaign where
  hashWithSalt _salt CreateCampaign' {..} =
    _salt
      `Prelude.hashWithSalt` compression
      `Prelude.hashWithSalt` dataDestinationConfigs
      `Prelude.hashWithSalt` dataExtraDimensions
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` diagnosticsMode
      `Prelude.hashWithSalt` expiryTime
      `Prelude.hashWithSalt` postTriggerCollectionDuration
      `Prelude.hashWithSalt` priority
      `Prelude.hashWithSalt` signalsToCollect
      `Prelude.hashWithSalt` spoolingMode
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` signalCatalogArn
      `Prelude.hashWithSalt` targetArn
      `Prelude.hashWithSalt` collectionScheme

instance Prelude.NFData CreateCampaign where
  rnf CreateCampaign' {..} =
    Prelude.rnf compression
      `Prelude.seq` Prelude.rnf dataDestinationConfigs
      `Prelude.seq` Prelude.rnf dataExtraDimensions
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf diagnosticsMode
      `Prelude.seq` Prelude.rnf expiryTime
      `Prelude.seq` Prelude.rnf postTriggerCollectionDuration
      `Prelude.seq` Prelude.rnf priority
      `Prelude.seq` Prelude.rnf signalsToCollect
      `Prelude.seq` Prelude.rnf spoolingMode
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf signalCatalogArn
      `Prelude.seq` Prelude.rnf targetArn
      `Prelude.seq` Prelude.rnf collectionScheme

instance Data.ToHeaders CreateCampaign where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "IoTAutobahnControlPlane.CreateCampaign" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateCampaign where
  toJSON CreateCampaign' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("compression" Data..=) Prelude.<$> compression,
            ("dataDestinationConfigs" Data..=)
              Prelude.<$> dataDestinationConfigs,
            ("dataExtraDimensions" Data..=)
              Prelude.<$> dataExtraDimensions,
            ("description" Data..=) Prelude.<$> description,
            ("diagnosticsMode" Data..=)
              Prelude.<$> diagnosticsMode,
            ("expiryTime" Data..=) Prelude.<$> expiryTime,
            ("postTriggerCollectionDuration" Data..=)
              Prelude.<$> postTriggerCollectionDuration,
            ("priority" Data..=) Prelude.<$> priority,
            ("signalsToCollect" Data..=)
              Prelude.<$> signalsToCollect,
            ("spoolingMode" Data..=) Prelude.<$> spoolingMode,
            ("startTime" Data..=) Prelude.<$> startTime,
            ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("name" Data..= name),
            Prelude.Just
              ("signalCatalogArn" Data..= signalCatalogArn),
            Prelude.Just ("targetArn" Data..= targetArn),
            Prelude.Just
              ("collectionScheme" Data..= collectionScheme)
          ]
      )

instance Data.ToPath CreateCampaign where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateCampaign where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateCampaignResponse' smart constructor.
data CreateCampaignResponse = CreateCampaignResponse'
  { -- | The ARN of the created campaign.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The name of the created campaign.
    name :: Prelude.Maybe Prelude.Text,
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
-- 'arn', 'createCampaignResponse_arn' - The ARN of the created campaign.
--
-- 'name', 'createCampaignResponse_name' - The name of the created campaign.
--
-- 'httpStatus', 'createCampaignResponse_httpStatus' - The response's http status code.
newCreateCampaignResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateCampaignResponse
newCreateCampaignResponse pHttpStatus_ =
  CreateCampaignResponse'
    { arn = Prelude.Nothing,
      name = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the created campaign.
createCampaignResponse_arn :: Lens.Lens' CreateCampaignResponse (Prelude.Maybe Prelude.Text)
createCampaignResponse_arn = Lens.lens (\CreateCampaignResponse' {arn} -> arn) (\s@CreateCampaignResponse' {} a -> s {arn = a} :: CreateCampaignResponse)

-- | The name of the created campaign.
createCampaignResponse_name :: Lens.Lens' CreateCampaignResponse (Prelude.Maybe Prelude.Text)
createCampaignResponse_name = Lens.lens (\CreateCampaignResponse' {name} -> name) (\s@CreateCampaignResponse' {} a -> s {name = a} :: CreateCampaignResponse)

-- | The response's http status code.
createCampaignResponse_httpStatus :: Lens.Lens' CreateCampaignResponse Prelude.Int
createCampaignResponse_httpStatus = Lens.lens (\CreateCampaignResponse' {httpStatus} -> httpStatus) (\s@CreateCampaignResponse' {} a -> s {httpStatus = a} :: CreateCampaignResponse)

instance Prelude.NFData CreateCampaignResponse where
  rnf CreateCampaignResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf httpStatus
