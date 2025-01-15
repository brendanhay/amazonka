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
-- Module      : Amazonka.EC2.GetPasswordData
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the encrypted administrator password for a running Windows
-- instance.
--
-- The Windows password is generated at boot by the @EC2Config@ service or
-- @EC2Launch@ scripts (Windows Server 2016 and later). This usually only
-- happens the first time an instance is launched. For more information,
-- see
-- <https://docs.aws.amazon.com/AWSEC2/latest/WindowsGuide/UsingConfig_WinAMI.html EC2Config>
-- and
-- <https://docs.aws.amazon.com/AWSEC2/latest/WindowsGuide/ec2launch.html EC2Launch>
-- in the /Amazon EC2 User Guide/.
--
-- For the @EC2Config@ service, the password is not generated for rebundled
-- AMIs unless @Ec2SetPassword@ is enabled before bundling.
--
-- The password is encrypted using the key pair that you specified when you
-- launched the instance. You must provide the corresponding key pair file.
--
-- When you launch an instance, password generation and encryption may take
-- a few minutes. If you try to retrieve the password before it\'s
-- available, the output returns an empty string. We recommend that you
-- wait up to 15 minutes after launching an instance before trying to
-- retrieve the generated password.
module Amazonka.EC2.GetPasswordData
  ( -- * Creating a Request
    GetPasswordData (..),
    newGetPasswordData,

    -- * Request Lenses
    getPasswordData_dryRun,
    getPasswordData_instanceId,

    -- * Destructuring the Response
    GetPasswordDataResponse (..),
    newGetPasswordDataResponse,

    -- * Response Lenses
    getPasswordDataResponse_httpStatus,
    getPasswordDataResponse_instanceId,
    getPasswordDataResponse_passwordData,
    getPasswordDataResponse_timestamp,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetPasswordData' smart constructor.
data GetPasswordData = GetPasswordData'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the Windows instance.
    instanceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetPasswordData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'getPasswordData_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'instanceId', 'getPasswordData_instanceId' - The ID of the Windows instance.
newGetPasswordData ::
  -- | 'instanceId'
  Prelude.Text ->
  GetPasswordData
newGetPasswordData pInstanceId_ =
  GetPasswordData'
    { dryRun = Prelude.Nothing,
      instanceId = pInstanceId_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
getPasswordData_dryRun :: Lens.Lens' GetPasswordData (Prelude.Maybe Prelude.Bool)
getPasswordData_dryRun = Lens.lens (\GetPasswordData' {dryRun} -> dryRun) (\s@GetPasswordData' {} a -> s {dryRun = a} :: GetPasswordData)

-- | The ID of the Windows instance.
getPasswordData_instanceId :: Lens.Lens' GetPasswordData Prelude.Text
getPasswordData_instanceId = Lens.lens (\GetPasswordData' {instanceId} -> instanceId) (\s@GetPasswordData' {} a -> s {instanceId = a} :: GetPasswordData)

instance Core.AWSRequest GetPasswordData where
  type
    AWSResponse GetPasswordData =
      GetPasswordDataResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          GetPasswordDataResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..@ "instanceId")
            Prelude.<*> (x Data..@ "passwordData")
            Prelude.<*> (x Data..@ "timestamp")
      )

instance Prelude.Hashable GetPasswordData where
  hashWithSalt _salt GetPasswordData' {..} =
    _salt
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` instanceId

instance Prelude.NFData GetPasswordData where
  rnf GetPasswordData' {..} =
    Prelude.rnf dryRun `Prelude.seq`
      Prelude.rnf instanceId

instance Data.ToHeaders GetPasswordData where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetPasswordData where
  toPath = Prelude.const "/"

instance Data.ToQuery GetPasswordData where
  toQuery GetPasswordData' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("GetPasswordData" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        "InstanceId" Data.=: instanceId
      ]

-- | /See:/ 'newGetPasswordDataResponse' smart constructor.
data GetPasswordDataResponse = GetPasswordDataResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The ID of the Windows instance.
    instanceId :: Prelude.Text,
    -- | The password of the instance. Returns an empty string if the password is
    -- not available.
    passwordData :: Prelude.Text,
    -- | The time the data was last updated.
    timestamp :: Data.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetPasswordDataResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getPasswordDataResponse_httpStatus' - The response's http status code.
--
-- 'instanceId', 'getPasswordDataResponse_instanceId' - The ID of the Windows instance.
--
-- 'passwordData', 'getPasswordDataResponse_passwordData' - The password of the instance. Returns an empty string if the password is
-- not available.
--
-- 'timestamp', 'getPasswordDataResponse_timestamp' - The time the data was last updated.
newGetPasswordDataResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'instanceId'
  Prelude.Text ->
  -- | 'passwordData'
  Prelude.Text ->
  -- | 'timestamp'
  Prelude.UTCTime ->
  GetPasswordDataResponse
newGetPasswordDataResponse
  pHttpStatus_
  pInstanceId_
  pPasswordData_
  pTimestamp_ =
    GetPasswordDataResponse'
      { httpStatus = pHttpStatus_,
        instanceId = pInstanceId_,
        passwordData = pPasswordData_,
        timestamp = Data._Time Lens.# pTimestamp_
      }

-- | The response's http status code.
getPasswordDataResponse_httpStatus :: Lens.Lens' GetPasswordDataResponse Prelude.Int
getPasswordDataResponse_httpStatus = Lens.lens (\GetPasswordDataResponse' {httpStatus} -> httpStatus) (\s@GetPasswordDataResponse' {} a -> s {httpStatus = a} :: GetPasswordDataResponse)

-- | The ID of the Windows instance.
getPasswordDataResponse_instanceId :: Lens.Lens' GetPasswordDataResponse Prelude.Text
getPasswordDataResponse_instanceId = Lens.lens (\GetPasswordDataResponse' {instanceId} -> instanceId) (\s@GetPasswordDataResponse' {} a -> s {instanceId = a} :: GetPasswordDataResponse)

-- | The password of the instance. Returns an empty string if the password is
-- not available.
getPasswordDataResponse_passwordData :: Lens.Lens' GetPasswordDataResponse Prelude.Text
getPasswordDataResponse_passwordData = Lens.lens (\GetPasswordDataResponse' {passwordData} -> passwordData) (\s@GetPasswordDataResponse' {} a -> s {passwordData = a} :: GetPasswordDataResponse)

-- | The time the data was last updated.
getPasswordDataResponse_timestamp :: Lens.Lens' GetPasswordDataResponse Prelude.UTCTime
getPasswordDataResponse_timestamp = Lens.lens (\GetPasswordDataResponse' {timestamp} -> timestamp) (\s@GetPasswordDataResponse' {} a -> s {timestamp = a} :: GetPasswordDataResponse) Prelude.. Data._Time

instance Prelude.NFData GetPasswordDataResponse where
  rnf GetPasswordDataResponse' {..} =
    Prelude.rnf httpStatus `Prelude.seq`
      Prelude.rnf instanceId `Prelude.seq`
        Prelude.rnf passwordData `Prelude.seq`
          Prelude.rnf timestamp
