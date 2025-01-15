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
-- Module      : Amazonka.SageMaker.DescribeDeviceFleet
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- A description of the fleet the device belongs to.
module Amazonka.SageMaker.DescribeDeviceFleet
  ( -- * Creating a Request
    DescribeDeviceFleet (..),
    newDescribeDeviceFleet,

    -- * Request Lenses
    describeDeviceFleet_deviceFleetName,

    -- * Destructuring the Response
    DescribeDeviceFleetResponse (..),
    newDescribeDeviceFleetResponse,

    -- * Response Lenses
    describeDeviceFleetResponse_description,
    describeDeviceFleetResponse_iotRoleAlias,
    describeDeviceFleetResponse_roleArn,
    describeDeviceFleetResponse_httpStatus,
    describeDeviceFleetResponse_deviceFleetName,
    describeDeviceFleetResponse_deviceFleetArn,
    describeDeviceFleetResponse_outputConfig,
    describeDeviceFleetResponse_creationTime,
    describeDeviceFleetResponse_lastModifiedTime,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newDescribeDeviceFleet' smart constructor.
data DescribeDeviceFleet = DescribeDeviceFleet'
  { -- | The name of the fleet.
    deviceFleetName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDeviceFleet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deviceFleetName', 'describeDeviceFleet_deviceFleetName' - The name of the fleet.
newDescribeDeviceFleet ::
  -- | 'deviceFleetName'
  Prelude.Text ->
  DescribeDeviceFleet
newDescribeDeviceFleet pDeviceFleetName_ =
  DescribeDeviceFleet'
    { deviceFleetName =
        pDeviceFleetName_
    }

-- | The name of the fleet.
describeDeviceFleet_deviceFleetName :: Lens.Lens' DescribeDeviceFleet Prelude.Text
describeDeviceFleet_deviceFleetName = Lens.lens (\DescribeDeviceFleet' {deviceFleetName} -> deviceFleetName) (\s@DescribeDeviceFleet' {} a -> s {deviceFleetName = a} :: DescribeDeviceFleet)

instance Core.AWSRequest DescribeDeviceFleet where
  type
    AWSResponse DescribeDeviceFleet =
      DescribeDeviceFleetResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeDeviceFleetResponse'
            Prelude.<$> (x Data..?> "Description")
            Prelude.<*> (x Data..?> "IotRoleAlias")
            Prelude.<*> (x Data..?> "RoleArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "DeviceFleetName")
            Prelude.<*> (x Data..:> "DeviceFleetArn")
            Prelude.<*> (x Data..:> "OutputConfig")
            Prelude.<*> (x Data..:> "CreationTime")
            Prelude.<*> (x Data..:> "LastModifiedTime")
      )

instance Prelude.Hashable DescribeDeviceFleet where
  hashWithSalt _salt DescribeDeviceFleet' {..} =
    _salt `Prelude.hashWithSalt` deviceFleetName

instance Prelude.NFData DescribeDeviceFleet where
  rnf DescribeDeviceFleet' {..} =
    Prelude.rnf deviceFleetName

instance Data.ToHeaders DescribeDeviceFleet where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SageMaker.DescribeDeviceFleet" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeDeviceFleet where
  toJSON DescribeDeviceFleet' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("DeviceFleetName" Data..= deviceFleetName)
          ]
      )

instance Data.ToPath DescribeDeviceFleet where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeDeviceFleet where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeDeviceFleetResponse' smart constructor.
data DescribeDeviceFleetResponse = DescribeDeviceFleetResponse'
  { -- | A description of the fleet.
    description :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) alias created in Amazon Web Services
    -- Internet of Things (IoT).
    iotRoleAlias :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) that has access to Amazon Web Services
    -- Internet of Things (IoT).
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The name of the fleet.
    deviceFleetName :: Prelude.Text,
    -- | The The Amazon Resource Name (ARN) of the fleet.
    deviceFleetArn :: Prelude.Text,
    -- | The output configuration for storing sampled data.
    outputConfig :: EdgeOutputConfig,
    -- | Timestamp of when the device fleet was created.
    creationTime :: Data.POSIX,
    -- | Timestamp of when the device fleet was last updated.
    lastModifiedTime :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDeviceFleetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'describeDeviceFleetResponse_description' - A description of the fleet.
--
-- 'iotRoleAlias', 'describeDeviceFleetResponse_iotRoleAlias' - The Amazon Resource Name (ARN) alias created in Amazon Web Services
-- Internet of Things (IoT).
--
-- 'roleArn', 'describeDeviceFleetResponse_roleArn' - The Amazon Resource Name (ARN) that has access to Amazon Web Services
-- Internet of Things (IoT).
--
-- 'httpStatus', 'describeDeviceFleetResponse_httpStatus' - The response's http status code.
--
-- 'deviceFleetName', 'describeDeviceFleetResponse_deviceFleetName' - The name of the fleet.
--
-- 'deviceFleetArn', 'describeDeviceFleetResponse_deviceFleetArn' - The The Amazon Resource Name (ARN) of the fleet.
--
-- 'outputConfig', 'describeDeviceFleetResponse_outputConfig' - The output configuration for storing sampled data.
--
-- 'creationTime', 'describeDeviceFleetResponse_creationTime' - Timestamp of when the device fleet was created.
--
-- 'lastModifiedTime', 'describeDeviceFleetResponse_lastModifiedTime' - Timestamp of when the device fleet was last updated.
newDescribeDeviceFleetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'deviceFleetName'
  Prelude.Text ->
  -- | 'deviceFleetArn'
  Prelude.Text ->
  -- | 'outputConfig'
  EdgeOutputConfig ->
  -- | 'creationTime'
  Prelude.UTCTime ->
  -- | 'lastModifiedTime'
  Prelude.UTCTime ->
  DescribeDeviceFleetResponse
newDescribeDeviceFleetResponse
  pHttpStatus_
  pDeviceFleetName_
  pDeviceFleetArn_
  pOutputConfig_
  pCreationTime_
  pLastModifiedTime_ =
    DescribeDeviceFleetResponse'
      { description =
          Prelude.Nothing,
        iotRoleAlias = Prelude.Nothing,
        roleArn = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        deviceFleetName = pDeviceFleetName_,
        deviceFleetArn = pDeviceFleetArn_,
        outputConfig = pOutputConfig_,
        creationTime =
          Data._Time Lens.# pCreationTime_,
        lastModifiedTime =
          Data._Time Lens.# pLastModifiedTime_
      }

-- | A description of the fleet.
describeDeviceFleetResponse_description :: Lens.Lens' DescribeDeviceFleetResponse (Prelude.Maybe Prelude.Text)
describeDeviceFleetResponse_description = Lens.lens (\DescribeDeviceFleetResponse' {description} -> description) (\s@DescribeDeviceFleetResponse' {} a -> s {description = a} :: DescribeDeviceFleetResponse)

-- | The Amazon Resource Name (ARN) alias created in Amazon Web Services
-- Internet of Things (IoT).
describeDeviceFleetResponse_iotRoleAlias :: Lens.Lens' DescribeDeviceFleetResponse (Prelude.Maybe Prelude.Text)
describeDeviceFleetResponse_iotRoleAlias = Lens.lens (\DescribeDeviceFleetResponse' {iotRoleAlias} -> iotRoleAlias) (\s@DescribeDeviceFleetResponse' {} a -> s {iotRoleAlias = a} :: DescribeDeviceFleetResponse)

-- | The Amazon Resource Name (ARN) that has access to Amazon Web Services
-- Internet of Things (IoT).
describeDeviceFleetResponse_roleArn :: Lens.Lens' DescribeDeviceFleetResponse (Prelude.Maybe Prelude.Text)
describeDeviceFleetResponse_roleArn = Lens.lens (\DescribeDeviceFleetResponse' {roleArn} -> roleArn) (\s@DescribeDeviceFleetResponse' {} a -> s {roleArn = a} :: DescribeDeviceFleetResponse)

-- | The response's http status code.
describeDeviceFleetResponse_httpStatus :: Lens.Lens' DescribeDeviceFleetResponse Prelude.Int
describeDeviceFleetResponse_httpStatus = Lens.lens (\DescribeDeviceFleetResponse' {httpStatus} -> httpStatus) (\s@DescribeDeviceFleetResponse' {} a -> s {httpStatus = a} :: DescribeDeviceFleetResponse)

-- | The name of the fleet.
describeDeviceFleetResponse_deviceFleetName :: Lens.Lens' DescribeDeviceFleetResponse Prelude.Text
describeDeviceFleetResponse_deviceFleetName = Lens.lens (\DescribeDeviceFleetResponse' {deviceFleetName} -> deviceFleetName) (\s@DescribeDeviceFleetResponse' {} a -> s {deviceFleetName = a} :: DescribeDeviceFleetResponse)

-- | The The Amazon Resource Name (ARN) of the fleet.
describeDeviceFleetResponse_deviceFleetArn :: Lens.Lens' DescribeDeviceFleetResponse Prelude.Text
describeDeviceFleetResponse_deviceFleetArn = Lens.lens (\DescribeDeviceFleetResponse' {deviceFleetArn} -> deviceFleetArn) (\s@DescribeDeviceFleetResponse' {} a -> s {deviceFleetArn = a} :: DescribeDeviceFleetResponse)

-- | The output configuration for storing sampled data.
describeDeviceFleetResponse_outputConfig :: Lens.Lens' DescribeDeviceFleetResponse EdgeOutputConfig
describeDeviceFleetResponse_outputConfig = Lens.lens (\DescribeDeviceFleetResponse' {outputConfig} -> outputConfig) (\s@DescribeDeviceFleetResponse' {} a -> s {outputConfig = a} :: DescribeDeviceFleetResponse)

-- | Timestamp of when the device fleet was created.
describeDeviceFleetResponse_creationTime :: Lens.Lens' DescribeDeviceFleetResponse Prelude.UTCTime
describeDeviceFleetResponse_creationTime = Lens.lens (\DescribeDeviceFleetResponse' {creationTime} -> creationTime) (\s@DescribeDeviceFleetResponse' {} a -> s {creationTime = a} :: DescribeDeviceFleetResponse) Prelude.. Data._Time

-- | Timestamp of when the device fleet was last updated.
describeDeviceFleetResponse_lastModifiedTime :: Lens.Lens' DescribeDeviceFleetResponse Prelude.UTCTime
describeDeviceFleetResponse_lastModifiedTime = Lens.lens (\DescribeDeviceFleetResponse' {lastModifiedTime} -> lastModifiedTime) (\s@DescribeDeviceFleetResponse' {} a -> s {lastModifiedTime = a} :: DescribeDeviceFleetResponse) Prelude.. Data._Time

instance Prelude.NFData DescribeDeviceFleetResponse where
  rnf DescribeDeviceFleetResponse' {..} =
    Prelude.rnf description `Prelude.seq`
      Prelude.rnf iotRoleAlias `Prelude.seq`
        Prelude.rnf roleArn `Prelude.seq`
          Prelude.rnf httpStatus `Prelude.seq`
            Prelude.rnf deviceFleetName `Prelude.seq`
              Prelude.rnf deviceFleetArn `Prelude.seq`
                Prelude.rnf outputConfig `Prelude.seq`
                  Prelude.rnf creationTime `Prelude.seq`
                    Prelude.rnf lastModifiedTime
