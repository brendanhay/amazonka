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
-- Module      : Amazonka.Location.GetDevicePosition
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a device\'s most recent position according to its sample time.
--
-- Device positions are deleted after 30 days.
module Amazonka.Location.GetDevicePosition
  ( -- * Creating a Request
    GetDevicePosition (..),
    newGetDevicePosition,

    -- * Request Lenses
    getDevicePosition_deviceId,
    getDevicePosition_trackerName,

    -- * Destructuring the Response
    GetDevicePositionResponse (..),
    newGetDevicePositionResponse,

    -- * Response Lenses
    getDevicePositionResponse_deviceId,
    getDevicePositionResponse_accuracy,
    getDevicePositionResponse_positionProperties,
    getDevicePositionResponse_httpStatus,
    getDevicePositionResponse_position,
    getDevicePositionResponse_receivedTime,
    getDevicePositionResponse_sampleTime,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Location.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetDevicePosition' smart constructor.
data GetDevicePosition = GetDevicePosition'
  { -- | The device whose position you want to retrieve.
    deviceId :: Prelude.Text,
    -- | The tracker resource receiving the position update.
    trackerName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDevicePosition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deviceId', 'getDevicePosition_deviceId' - The device whose position you want to retrieve.
--
-- 'trackerName', 'getDevicePosition_trackerName' - The tracker resource receiving the position update.
newGetDevicePosition ::
  -- | 'deviceId'
  Prelude.Text ->
  -- | 'trackerName'
  Prelude.Text ->
  GetDevicePosition
newGetDevicePosition pDeviceId_ pTrackerName_ =
  GetDevicePosition'
    { deviceId = pDeviceId_,
      trackerName = pTrackerName_
    }

-- | The device whose position you want to retrieve.
getDevicePosition_deviceId :: Lens.Lens' GetDevicePosition Prelude.Text
getDevicePosition_deviceId = Lens.lens (\GetDevicePosition' {deviceId} -> deviceId) (\s@GetDevicePosition' {} a -> s {deviceId = a} :: GetDevicePosition)

-- | The tracker resource receiving the position update.
getDevicePosition_trackerName :: Lens.Lens' GetDevicePosition Prelude.Text
getDevicePosition_trackerName = Lens.lens (\GetDevicePosition' {trackerName} -> trackerName) (\s@GetDevicePosition' {} a -> s {trackerName = a} :: GetDevicePosition)

instance Core.AWSRequest GetDevicePosition where
  type
    AWSResponse GetDevicePosition =
      GetDevicePositionResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDevicePositionResponse'
            Prelude.<$> (x Core..?> "DeviceId")
            Prelude.<*> (x Core..?> "Accuracy")
            Prelude.<*> ( x Core..?> "PositionProperties"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "Position")
            Prelude.<*> (x Core..:> "ReceivedTime")
            Prelude.<*> (x Core..:> "SampleTime")
      )

instance Prelude.Hashable GetDevicePosition where
  hashWithSalt _salt GetDevicePosition' {..} =
    _salt `Prelude.hashWithSalt` deviceId
      `Prelude.hashWithSalt` trackerName

instance Prelude.NFData GetDevicePosition where
  rnf GetDevicePosition' {..} =
    Prelude.rnf deviceId
      `Prelude.seq` Prelude.rnf trackerName

instance Core.ToHeaders GetDevicePosition where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetDevicePosition where
  toPath GetDevicePosition' {..} =
    Prelude.mconcat
      [ "/tracking/v0/trackers/",
        Core.toBS trackerName,
        "/devices/",
        Core.toBS deviceId,
        "/positions/latest"
      ]

instance Core.ToQuery GetDevicePosition where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetDevicePositionResponse' smart constructor.
data GetDevicePositionResponse = GetDevicePositionResponse'
  { -- | The device whose position you retrieved.
    deviceId :: Prelude.Maybe Prelude.Text,
    -- | The accuracy of the device position.
    accuracy :: Prelude.Maybe PositionalAccuracy,
    -- | The properties associated with the position.
    positionProperties :: Prelude.Maybe (Core.Sensitive (Prelude.HashMap Prelude.Text Prelude.Text)),
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The last known device position.
    position :: Core.Sensitive (Prelude.NonEmpty Prelude.Double),
    -- | The timestamp for when the tracker resource received the device position
    -- in <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
    -- format: @YYYY-MM-DDThh:mm:ss.sssZ@.
    receivedTime :: Core.POSIX,
    -- | The timestamp at which the device\'s position was determined. Uses
    -- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
    -- format: @YYYY-MM-DDThh:mm:ss.sssZ@.
    sampleTime :: Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDevicePositionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deviceId', 'getDevicePositionResponse_deviceId' - The device whose position you retrieved.
--
-- 'accuracy', 'getDevicePositionResponse_accuracy' - The accuracy of the device position.
--
-- 'positionProperties', 'getDevicePositionResponse_positionProperties' - The properties associated with the position.
--
-- 'httpStatus', 'getDevicePositionResponse_httpStatus' - The response's http status code.
--
-- 'position', 'getDevicePositionResponse_position' - The last known device position.
--
-- 'receivedTime', 'getDevicePositionResponse_receivedTime' - The timestamp for when the tracker resource received the device position
-- in <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
-- format: @YYYY-MM-DDThh:mm:ss.sssZ@.
--
-- 'sampleTime', 'getDevicePositionResponse_sampleTime' - The timestamp at which the device\'s position was determined. Uses
-- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
-- format: @YYYY-MM-DDThh:mm:ss.sssZ@.
newGetDevicePositionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'position'
  Prelude.NonEmpty Prelude.Double ->
  -- | 'receivedTime'
  Prelude.UTCTime ->
  -- | 'sampleTime'
  Prelude.UTCTime ->
  GetDevicePositionResponse
newGetDevicePositionResponse
  pHttpStatus_
  pPosition_
  pReceivedTime_
  pSampleTime_ =
    GetDevicePositionResponse'
      { deviceId =
          Prelude.Nothing,
        accuracy = Prelude.Nothing,
        positionProperties = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        position =
          Core._Sensitive Prelude.. Lens.coerced
            Lens.# pPosition_,
        receivedTime = Core._Time Lens.# pReceivedTime_,
        sampleTime = Core._Time Lens.# pSampleTime_
      }

-- | The device whose position you retrieved.
getDevicePositionResponse_deviceId :: Lens.Lens' GetDevicePositionResponse (Prelude.Maybe Prelude.Text)
getDevicePositionResponse_deviceId = Lens.lens (\GetDevicePositionResponse' {deviceId} -> deviceId) (\s@GetDevicePositionResponse' {} a -> s {deviceId = a} :: GetDevicePositionResponse)

-- | The accuracy of the device position.
getDevicePositionResponse_accuracy :: Lens.Lens' GetDevicePositionResponse (Prelude.Maybe PositionalAccuracy)
getDevicePositionResponse_accuracy = Lens.lens (\GetDevicePositionResponse' {accuracy} -> accuracy) (\s@GetDevicePositionResponse' {} a -> s {accuracy = a} :: GetDevicePositionResponse)

-- | The properties associated with the position.
getDevicePositionResponse_positionProperties :: Lens.Lens' GetDevicePositionResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getDevicePositionResponse_positionProperties = Lens.lens (\GetDevicePositionResponse' {positionProperties} -> positionProperties) (\s@GetDevicePositionResponse' {} a -> s {positionProperties = a} :: GetDevicePositionResponse) Prelude.. Lens.mapping (Core._Sensitive Prelude.. Lens.coerced)

-- | The response's http status code.
getDevicePositionResponse_httpStatus :: Lens.Lens' GetDevicePositionResponse Prelude.Int
getDevicePositionResponse_httpStatus = Lens.lens (\GetDevicePositionResponse' {httpStatus} -> httpStatus) (\s@GetDevicePositionResponse' {} a -> s {httpStatus = a} :: GetDevicePositionResponse)

-- | The last known device position.
getDevicePositionResponse_position :: Lens.Lens' GetDevicePositionResponse (Prelude.NonEmpty Prelude.Double)
getDevicePositionResponse_position = Lens.lens (\GetDevicePositionResponse' {position} -> position) (\s@GetDevicePositionResponse' {} a -> s {position = a} :: GetDevicePositionResponse) Prelude.. Core._Sensitive Prelude.. Lens.coerced

-- | The timestamp for when the tracker resource received the device position
-- in <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
-- format: @YYYY-MM-DDThh:mm:ss.sssZ@.
getDevicePositionResponse_receivedTime :: Lens.Lens' GetDevicePositionResponse Prelude.UTCTime
getDevicePositionResponse_receivedTime = Lens.lens (\GetDevicePositionResponse' {receivedTime} -> receivedTime) (\s@GetDevicePositionResponse' {} a -> s {receivedTime = a} :: GetDevicePositionResponse) Prelude.. Core._Time

-- | The timestamp at which the device\'s position was determined. Uses
-- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
-- format: @YYYY-MM-DDThh:mm:ss.sssZ@.
getDevicePositionResponse_sampleTime :: Lens.Lens' GetDevicePositionResponse Prelude.UTCTime
getDevicePositionResponse_sampleTime = Lens.lens (\GetDevicePositionResponse' {sampleTime} -> sampleTime) (\s@GetDevicePositionResponse' {} a -> s {sampleTime = a} :: GetDevicePositionResponse) Prelude.. Core._Time

instance Prelude.NFData GetDevicePositionResponse where
  rnf GetDevicePositionResponse' {..} =
    Prelude.rnf deviceId
      `Prelude.seq` Prelude.rnf accuracy
      `Prelude.seq` Prelude.rnf positionProperties
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf position
      `Prelude.seq` Prelude.rnf receivedTime
      `Prelude.seq` Prelude.rnf sampleTime
