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
-- Module      : Network.AWS.EC2.GetPasswordData
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
module Network.AWS.EC2.GetPasswordData
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

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetPasswordData' smart constructor.
data GetPasswordData = GetPasswordData'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | The ID of the Windows instance.
    instanceId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  GetPasswordData
newGetPasswordData pInstanceId_ =
  GetPasswordData'
    { dryRun = Core.Nothing,
      instanceId = pInstanceId_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
getPasswordData_dryRun :: Lens.Lens' GetPasswordData (Core.Maybe Core.Bool)
getPasswordData_dryRun = Lens.lens (\GetPasswordData' {dryRun} -> dryRun) (\s@GetPasswordData' {} a -> s {dryRun = a} :: GetPasswordData)

-- | The ID of the Windows instance.
getPasswordData_instanceId :: Lens.Lens' GetPasswordData Core.Text
getPasswordData_instanceId = Lens.lens (\GetPasswordData' {instanceId} -> instanceId) (\s@GetPasswordData' {} a -> s {instanceId = a} :: GetPasswordData)

instance Core.AWSRequest GetPasswordData where
  type
    AWSResponse GetPasswordData =
      GetPasswordDataResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          GetPasswordDataResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..@ "instanceId")
            Core.<*> (x Core..@ "passwordData")
            Core.<*> (x Core..@ "timestamp")
      )

instance Core.Hashable GetPasswordData

instance Core.NFData GetPasswordData

instance Core.ToHeaders GetPasswordData where
  toHeaders = Core.const Core.mempty

instance Core.ToPath GetPasswordData where
  toPath = Core.const "/"

instance Core.ToQuery GetPasswordData where
  toQuery GetPasswordData' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("GetPasswordData" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "DryRun" Core.=: dryRun,
        "InstanceId" Core.=: instanceId
      ]

-- | /See:/ 'newGetPasswordDataResponse' smart constructor.
data GetPasswordDataResponse = GetPasswordDataResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | The ID of the Windows instance.
    instanceId :: Core.Text,
    -- | The password of the instance. Returns an empty string if the password is
    -- not available.
    passwordData :: Core.Text,
    -- | The time the data was last updated.
    timestamp :: Core.ISO8601
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  -- | 'instanceId'
  Core.Text ->
  -- | 'passwordData'
  Core.Text ->
  -- | 'timestamp'
  Core.UTCTime ->
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
        timestamp = Core._Time Lens.# pTimestamp_
      }

-- | The response's http status code.
getPasswordDataResponse_httpStatus :: Lens.Lens' GetPasswordDataResponse Core.Int
getPasswordDataResponse_httpStatus = Lens.lens (\GetPasswordDataResponse' {httpStatus} -> httpStatus) (\s@GetPasswordDataResponse' {} a -> s {httpStatus = a} :: GetPasswordDataResponse)

-- | The ID of the Windows instance.
getPasswordDataResponse_instanceId :: Lens.Lens' GetPasswordDataResponse Core.Text
getPasswordDataResponse_instanceId = Lens.lens (\GetPasswordDataResponse' {instanceId} -> instanceId) (\s@GetPasswordDataResponse' {} a -> s {instanceId = a} :: GetPasswordDataResponse)

-- | The password of the instance. Returns an empty string if the password is
-- not available.
getPasswordDataResponse_passwordData :: Lens.Lens' GetPasswordDataResponse Core.Text
getPasswordDataResponse_passwordData = Lens.lens (\GetPasswordDataResponse' {passwordData} -> passwordData) (\s@GetPasswordDataResponse' {} a -> s {passwordData = a} :: GetPasswordDataResponse)

-- | The time the data was last updated.
getPasswordDataResponse_timestamp :: Lens.Lens' GetPasswordDataResponse Core.UTCTime
getPasswordDataResponse_timestamp = Lens.lens (\GetPasswordDataResponse' {timestamp} -> timestamp) (\s@GetPasswordDataResponse' {} a -> s {timestamp = a} :: GetPasswordDataResponse) Core.. Core._Time

instance Core.NFData GetPasswordDataResponse
