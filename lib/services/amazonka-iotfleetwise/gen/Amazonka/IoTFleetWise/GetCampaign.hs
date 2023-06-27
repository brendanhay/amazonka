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
-- Module      : Amazonka.IoTFleetWise.GetCampaign
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a campaign.
module Amazonka.IoTFleetWise.GetCampaign
  ( -- * Creating a Request
    GetCampaign (..),
    newGetCampaign,

    -- * Request Lenses
    getCampaign_name,

    -- * Destructuring the Response
    GetCampaignResponse (..),
    newGetCampaignResponse,

    -- * Response Lenses
    getCampaignResponse_arn,
    getCampaignResponse_collectionScheme,
    getCampaignResponse_compression,
    getCampaignResponse_creationTime,
    getCampaignResponse_dataDestinationConfigs,
    getCampaignResponse_dataExtraDimensions,
    getCampaignResponse_description,
    getCampaignResponse_diagnosticsMode,
    getCampaignResponse_expiryTime,
    getCampaignResponse_lastModificationTime,
    getCampaignResponse_name,
    getCampaignResponse_postTriggerCollectionDuration,
    getCampaignResponse_priority,
    getCampaignResponse_signalCatalogArn,
    getCampaignResponse_signalsToCollect,
    getCampaignResponse_spoolingMode,
    getCampaignResponse_startTime,
    getCampaignResponse_status,
    getCampaignResponse_targetArn,
    getCampaignResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTFleetWise.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetCampaign' smart constructor.
data GetCampaign = GetCampaign'
  { -- | The name of the campaign to retrieve information about.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetCampaign' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'getCampaign_name' - The name of the campaign to retrieve information about.
newGetCampaign ::
  -- | 'name'
  Prelude.Text ->
  GetCampaign
newGetCampaign pName_ = GetCampaign' {name = pName_}

-- | The name of the campaign to retrieve information about.
getCampaign_name :: Lens.Lens' GetCampaign Prelude.Text
getCampaign_name = Lens.lens (\GetCampaign' {name} -> name) (\s@GetCampaign' {} a -> s {name = a} :: GetCampaign)

instance Core.AWSRequest GetCampaign where
  type AWSResponse GetCampaign = GetCampaignResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetCampaignResponse'
            Prelude.<$> (x Data..?> "arn")
            Prelude.<*> (x Data..?> "collectionScheme")
            Prelude.<*> (x Data..?> "compression")
            Prelude.<*> (x Data..?> "creationTime")
            Prelude.<*> (x Data..?> "dataDestinationConfigs")
            Prelude.<*> ( x
                            Data..?> "dataExtraDimensions"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "description")
            Prelude.<*> (x Data..?> "diagnosticsMode")
            Prelude.<*> (x Data..?> "expiryTime")
            Prelude.<*> (x Data..?> "lastModificationTime")
            Prelude.<*> (x Data..?> "name")
            Prelude.<*> (x Data..?> "postTriggerCollectionDuration")
            Prelude.<*> (x Data..?> "priority")
            Prelude.<*> (x Data..?> "signalCatalogArn")
            Prelude.<*> ( x
                            Data..?> "signalsToCollect"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "spoolingMode")
            Prelude.<*> (x Data..?> "startTime")
            Prelude.<*> (x Data..?> "status")
            Prelude.<*> (x Data..?> "targetArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetCampaign where
  hashWithSalt _salt GetCampaign' {..} =
    _salt `Prelude.hashWithSalt` name

instance Prelude.NFData GetCampaign where
  rnf GetCampaign' {..} = Prelude.rnf name

instance Data.ToHeaders GetCampaign where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "IoTAutobahnControlPlane.GetCampaign" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetCampaign where
  toJSON GetCampaign' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("name" Data..= name)]
      )

instance Data.ToPath GetCampaign where
  toPath = Prelude.const "/"

instance Data.ToQuery GetCampaign where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetCampaignResponse' smart constructor.
data GetCampaignResponse = GetCampaignResponse'
  { -- | The Amazon Resource Name (ARN) of the campaign.
    arn :: Prelude.Maybe Prelude.Text,
    -- | Information about the data collection scheme associated with the
    -- campaign.
    collectionScheme :: Prelude.Maybe CollectionScheme,
    -- | Whether to compress signals before transmitting data to Amazon Web
    -- Services IoT FleetWise. If @OFF@ is specified, the signals aren\'t
    -- compressed. If it\'s not specified, @SNAPPY@ is used.
    compression :: Prelude.Maybe Compression,
    -- | The time the campaign was created in seconds since epoch (January 1,
    -- 1970 at midnight UTC time).
    creationTime :: Prelude.Maybe Data.POSIX,
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
    -- | A list of vehicle attributes associated with the campaign.
    dataExtraDimensions :: Prelude.Maybe [Prelude.Text],
    -- | The description of the campaign.
    description :: Prelude.Maybe Prelude.Text,
    -- | Option for a vehicle to send diagnostic trouble codes to Amazon Web
    -- Services IoT FleetWise.
    diagnosticsMode :: Prelude.Maybe DiagnosticsMode,
    -- | The time the campaign expires, in seconds since epoch (January 1, 1970
    -- at midnight UTC time). Vehicle data won\'t be collected after the
    -- campaign expires.
    expiryTime :: Prelude.Maybe Data.POSIX,
    -- | The last time the campaign was modified.
    lastModificationTime :: Prelude.Maybe Data.POSIX,
    -- | The name of the campaign.
    name :: Prelude.Maybe Prelude.Text,
    -- | How long (in seconds) to collect raw data after a triggering event
    -- initiates the collection.
    postTriggerCollectionDuration :: Prelude.Maybe Prelude.Natural,
    -- | A number indicating the priority of one campaign over another campaign
    -- for a certain vehicle or fleet. A campaign with the lowest value is
    -- deployed to vehicles before any other campaigns.
    priority :: Prelude.Maybe Prelude.Natural,
    -- | The ARN of a signal catalog.
    signalCatalogArn :: Prelude.Maybe Prelude.Text,
    -- | Information about a list of signals to collect data on.
    signalsToCollect :: Prelude.Maybe [SignalInformation],
    -- | Whether to store collected data after a vehicle lost a connection with
    -- the cloud. After a connection is re-established, the data is
    -- automatically forwarded to Amazon Web Services IoT FleetWise.
    spoolingMode :: Prelude.Maybe SpoolingMode,
    -- | The time, in milliseconds, to deliver a campaign after it was approved.
    startTime :: Prelude.Maybe Data.POSIX,
    -- | The state of the campaign. The status can be one of: @CREATING@,
    -- @WAITING_FOR_APPROVAL@, @RUNNING@, and @SUSPENDED@.
    status :: Prelude.Maybe CampaignStatus,
    -- | The ARN of the vehicle or the fleet targeted by the campaign.
    targetArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetCampaignResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'getCampaignResponse_arn' - The Amazon Resource Name (ARN) of the campaign.
--
-- 'collectionScheme', 'getCampaignResponse_collectionScheme' - Information about the data collection scheme associated with the
-- campaign.
--
-- 'compression', 'getCampaignResponse_compression' - Whether to compress signals before transmitting data to Amazon Web
-- Services IoT FleetWise. If @OFF@ is specified, the signals aren\'t
-- compressed. If it\'s not specified, @SNAPPY@ is used.
--
-- 'creationTime', 'getCampaignResponse_creationTime' - The time the campaign was created in seconds since epoch (January 1,
-- 1970 at midnight UTC time).
--
-- 'dataDestinationConfigs', 'getCampaignResponse_dataDestinationConfigs' - The destination where the campaign sends data. You can choose to send
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
-- 'dataExtraDimensions', 'getCampaignResponse_dataExtraDimensions' - A list of vehicle attributes associated with the campaign.
--
-- 'description', 'getCampaignResponse_description' - The description of the campaign.
--
-- 'diagnosticsMode', 'getCampaignResponse_diagnosticsMode' - Option for a vehicle to send diagnostic trouble codes to Amazon Web
-- Services IoT FleetWise.
--
-- 'expiryTime', 'getCampaignResponse_expiryTime' - The time the campaign expires, in seconds since epoch (January 1, 1970
-- at midnight UTC time). Vehicle data won\'t be collected after the
-- campaign expires.
--
-- 'lastModificationTime', 'getCampaignResponse_lastModificationTime' - The last time the campaign was modified.
--
-- 'name', 'getCampaignResponse_name' - The name of the campaign.
--
-- 'postTriggerCollectionDuration', 'getCampaignResponse_postTriggerCollectionDuration' - How long (in seconds) to collect raw data after a triggering event
-- initiates the collection.
--
-- 'priority', 'getCampaignResponse_priority' - A number indicating the priority of one campaign over another campaign
-- for a certain vehicle or fleet. A campaign with the lowest value is
-- deployed to vehicles before any other campaigns.
--
-- 'signalCatalogArn', 'getCampaignResponse_signalCatalogArn' - The ARN of a signal catalog.
--
-- 'signalsToCollect', 'getCampaignResponse_signalsToCollect' - Information about a list of signals to collect data on.
--
-- 'spoolingMode', 'getCampaignResponse_spoolingMode' - Whether to store collected data after a vehicle lost a connection with
-- the cloud. After a connection is re-established, the data is
-- automatically forwarded to Amazon Web Services IoT FleetWise.
--
-- 'startTime', 'getCampaignResponse_startTime' - The time, in milliseconds, to deliver a campaign after it was approved.
--
-- 'status', 'getCampaignResponse_status' - The state of the campaign. The status can be one of: @CREATING@,
-- @WAITING_FOR_APPROVAL@, @RUNNING@, and @SUSPENDED@.
--
-- 'targetArn', 'getCampaignResponse_targetArn' - The ARN of the vehicle or the fleet targeted by the campaign.
--
-- 'httpStatus', 'getCampaignResponse_httpStatus' - The response's http status code.
newGetCampaignResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetCampaignResponse
newGetCampaignResponse pHttpStatus_ =
  GetCampaignResponse'
    { arn = Prelude.Nothing,
      collectionScheme = Prelude.Nothing,
      compression = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      dataDestinationConfigs = Prelude.Nothing,
      dataExtraDimensions = Prelude.Nothing,
      description = Prelude.Nothing,
      diagnosticsMode = Prelude.Nothing,
      expiryTime = Prelude.Nothing,
      lastModificationTime = Prelude.Nothing,
      name = Prelude.Nothing,
      postTriggerCollectionDuration = Prelude.Nothing,
      priority = Prelude.Nothing,
      signalCatalogArn = Prelude.Nothing,
      signalsToCollect = Prelude.Nothing,
      spoolingMode = Prelude.Nothing,
      startTime = Prelude.Nothing,
      status = Prelude.Nothing,
      targetArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the campaign.
getCampaignResponse_arn :: Lens.Lens' GetCampaignResponse (Prelude.Maybe Prelude.Text)
getCampaignResponse_arn = Lens.lens (\GetCampaignResponse' {arn} -> arn) (\s@GetCampaignResponse' {} a -> s {arn = a} :: GetCampaignResponse)

-- | Information about the data collection scheme associated with the
-- campaign.
getCampaignResponse_collectionScheme :: Lens.Lens' GetCampaignResponse (Prelude.Maybe CollectionScheme)
getCampaignResponse_collectionScheme = Lens.lens (\GetCampaignResponse' {collectionScheme} -> collectionScheme) (\s@GetCampaignResponse' {} a -> s {collectionScheme = a} :: GetCampaignResponse)

-- | Whether to compress signals before transmitting data to Amazon Web
-- Services IoT FleetWise. If @OFF@ is specified, the signals aren\'t
-- compressed. If it\'s not specified, @SNAPPY@ is used.
getCampaignResponse_compression :: Lens.Lens' GetCampaignResponse (Prelude.Maybe Compression)
getCampaignResponse_compression = Lens.lens (\GetCampaignResponse' {compression} -> compression) (\s@GetCampaignResponse' {} a -> s {compression = a} :: GetCampaignResponse)

-- | The time the campaign was created in seconds since epoch (January 1,
-- 1970 at midnight UTC time).
getCampaignResponse_creationTime :: Lens.Lens' GetCampaignResponse (Prelude.Maybe Prelude.UTCTime)
getCampaignResponse_creationTime = Lens.lens (\GetCampaignResponse' {creationTime} -> creationTime) (\s@GetCampaignResponse' {} a -> s {creationTime = a} :: GetCampaignResponse) Prelude.. Lens.mapping Data._Time

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
getCampaignResponse_dataDestinationConfigs :: Lens.Lens' GetCampaignResponse (Prelude.Maybe (Prelude.NonEmpty DataDestinationConfig))
getCampaignResponse_dataDestinationConfigs = Lens.lens (\GetCampaignResponse' {dataDestinationConfigs} -> dataDestinationConfigs) (\s@GetCampaignResponse' {} a -> s {dataDestinationConfigs = a} :: GetCampaignResponse) Prelude.. Lens.mapping Lens.coerced

-- | A list of vehicle attributes associated with the campaign.
getCampaignResponse_dataExtraDimensions :: Lens.Lens' GetCampaignResponse (Prelude.Maybe [Prelude.Text])
getCampaignResponse_dataExtraDimensions = Lens.lens (\GetCampaignResponse' {dataExtraDimensions} -> dataExtraDimensions) (\s@GetCampaignResponse' {} a -> s {dataExtraDimensions = a} :: GetCampaignResponse) Prelude.. Lens.mapping Lens.coerced

-- | The description of the campaign.
getCampaignResponse_description :: Lens.Lens' GetCampaignResponse (Prelude.Maybe Prelude.Text)
getCampaignResponse_description = Lens.lens (\GetCampaignResponse' {description} -> description) (\s@GetCampaignResponse' {} a -> s {description = a} :: GetCampaignResponse)

-- | Option for a vehicle to send diagnostic trouble codes to Amazon Web
-- Services IoT FleetWise.
getCampaignResponse_diagnosticsMode :: Lens.Lens' GetCampaignResponse (Prelude.Maybe DiagnosticsMode)
getCampaignResponse_diagnosticsMode = Lens.lens (\GetCampaignResponse' {diagnosticsMode} -> diagnosticsMode) (\s@GetCampaignResponse' {} a -> s {diagnosticsMode = a} :: GetCampaignResponse)

-- | The time the campaign expires, in seconds since epoch (January 1, 1970
-- at midnight UTC time). Vehicle data won\'t be collected after the
-- campaign expires.
getCampaignResponse_expiryTime :: Lens.Lens' GetCampaignResponse (Prelude.Maybe Prelude.UTCTime)
getCampaignResponse_expiryTime = Lens.lens (\GetCampaignResponse' {expiryTime} -> expiryTime) (\s@GetCampaignResponse' {} a -> s {expiryTime = a} :: GetCampaignResponse) Prelude.. Lens.mapping Data._Time

-- | The last time the campaign was modified.
getCampaignResponse_lastModificationTime :: Lens.Lens' GetCampaignResponse (Prelude.Maybe Prelude.UTCTime)
getCampaignResponse_lastModificationTime = Lens.lens (\GetCampaignResponse' {lastModificationTime} -> lastModificationTime) (\s@GetCampaignResponse' {} a -> s {lastModificationTime = a} :: GetCampaignResponse) Prelude.. Lens.mapping Data._Time

-- | The name of the campaign.
getCampaignResponse_name :: Lens.Lens' GetCampaignResponse (Prelude.Maybe Prelude.Text)
getCampaignResponse_name = Lens.lens (\GetCampaignResponse' {name} -> name) (\s@GetCampaignResponse' {} a -> s {name = a} :: GetCampaignResponse)

-- | How long (in seconds) to collect raw data after a triggering event
-- initiates the collection.
getCampaignResponse_postTriggerCollectionDuration :: Lens.Lens' GetCampaignResponse (Prelude.Maybe Prelude.Natural)
getCampaignResponse_postTriggerCollectionDuration = Lens.lens (\GetCampaignResponse' {postTriggerCollectionDuration} -> postTriggerCollectionDuration) (\s@GetCampaignResponse' {} a -> s {postTriggerCollectionDuration = a} :: GetCampaignResponse)

-- | A number indicating the priority of one campaign over another campaign
-- for a certain vehicle or fleet. A campaign with the lowest value is
-- deployed to vehicles before any other campaigns.
getCampaignResponse_priority :: Lens.Lens' GetCampaignResponse (Prelude.Maybe Prelude.Natural)
getCampaignResponse_priority = Lens.lens (\GetCampaignResponse' {priority} -> priority) (\s@GetCampaignResponse' {} a -> s {priority = a} :: GetCampaignResponse)

-- | The ARN of a signal catalog.
getCampaignResponse_signalCatalogArn :: Lens.Lens' GetCampaignResponse (Prelude.Maybe Prelude.Text)
getCampaignResponse_signalCatalogArn = Lens.lens (\GetCampaignResponse' {signalCatalogArn} -> signalCatalogArn) (\s@GetCampaignResponse' {} a -> s {signalCatalogArn = a} :: GetCampaignResponse)

-- | Information about a list of signals to collect data on.
getCampaignResponse_signalsToCollect :: Lens.Lens' GetCampaignResponse (Prelude.Maybe [SignalInformation])
getCampaignResponse_signalsToCollect = Lens.lens (\GetCampaignResponse' {signalsToCollect} -> signalsToCollect) (\s@GetCampaignResponse' {} a -> s {signalsToCollect = a} :: GetCampaignResponse) Prelude.. Lens.mapping Lens.coerced

-- | Whether to store collected data after a vehicle lost a connection with
-- the cloud. After a connection is re-established, the data is
-- automatically forwarded to Amazon Web Services IoT FleetWise.
getCampaignResponse_spoolingMode :: Lens.Lens' GetCampaignResponse (Prelude.Maybe SpoolingMode)
getCampaignResponse_spoolingMode = Lens.lens (\GetCampaignResponse' {spoolingMode} -> spoolingMode) (\s@GetCampaignResponse' {} a -> s {spoolingMode = a} :: GetCampaignResponse)

-- | The time, in milliseconds, to deliver a campaign after it was approved.
getCampaignResponse_startTime :: Lens.Lens' GetCampaignResponse (Prelude.Maybe Prelude.UTCTime)
getCampaignResponse_startTime = Lens.lens (\GetCampaignResponse' {startTime} -> startTime) (\s@GetCampaignResponse' {} a -> s {startTime = a} :: GetCampaignResponse) Prelude.. Lens.mapping Data._Time

-- | The state of the campaign. The status can be one of: @CREATING@,
-- @WAITING_FOR_APPROVAL@, @RUNNING@, and @SUSPENDED@.
getCampaignResponse_status :: Lens.Lens' GetCampaignResponse (Prelude.Maybe CampaignStatus)
getCampaignResponse_status = Lens.lens (\GetCampaignResponse' {status} -> status) (\s@GetCampaignResponse' {} a -> s {status = a} :: GetCampaignResponse)

-- | The ARN of the vehicle or the fleet targeted by the campaign.
getCampaignResponse_targetArn :: Lens.Lens' GetCampaignResponse (Prelude.Maybe Prelude.Text)
getCampaignResponse_targetArn = Lens.lens (\GetCampaignResponse' {targetArn} -> targetArn) (\s@GetCampaignResponse' {} a -> s {targetArn = a} :: GetCampaignResponse)

-- | The response's http status code.
getCampaignResponse_httpStatus :: Lens.Lens' GetCampaignResponse Prelude.Int
getCampaignResponse_httpStatus = Lens.lens (\GetCampaignResponse' {httpStatus} -> httpStatus) (\s@GetCampaignResponse' {} a -> s {httpStatus = a} :: GetCampaignResponse)

instance Prelude.NFData GetCampaignResponse where
  rnf GetCampaignResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf collectionScheme
      `Prelude.seq` Prelude.rnf compression
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf dataDestinationConfigs
      `Prelude.seq` Prelude.rnf dataExtraDimensions
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf diagnosticsMode
      `Prelude.seq` Prelude.rnf expiryTime
      `Prelude.seq` Prelude.rnf lastModificationTime
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf postTriggerCollectionDuration
      `Prelude.seq` Prelude.rnf priority
      `Prelude.seq` Prelude.rnf signalCatalogArn
      `Prelude.seq` Prelude.rnf signalsToCollect
      `Prelude.seq` Prelude.rnf spoolingMode
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf targetArn
      `Prelude.seq` Prelude.rnf httpStatus
