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
-- Module      : Network.AWS.AlexaBusiness.AssociateDeviceWithNetworkProfile
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a device with the specified network profile.
module Network.AWS.AlexaBusiness.AssociateDeviceWithNetworkProfile
  ( -- * Creating a Request
    AssociateDeviceWithNetworkProfile (..),
    newAssociateDeviceWithNetworkProfile,

    -- * Request Lenses
    associateDeviceWithNetworkProfile_deviceArn,
    associateDeviceWithNetworkProfile_networkProfileArn,

    -- * Destructuring the Response
    AssociateDeviceWithNetworkProfileResponse (..),
    newAssociateDeviceWithNetworkProfileResponse,

    -- * Response Lenses
    associateDeviceWithNetworkProfileResponse_httpStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newAssociateDeviceWithNetworkProfile' smart constructor.
data AssociateDeviceWithNetworkProfile = AssociateDeviceWithNetworkProfile'
  { -- | The device ARN.
    deviceArn :: Core.Text,
    -- | The ARN of the network profile to associate with a device.
    networkProfileArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AssociateDeviceWithNetworkProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deviceArn', 'associateDeviceWithNetworkProfile_deviceArn' - The device ARN.
--
-- 'networkProfileArn', 'associateDeviceWithNetworkProfile_networkProfileArn' - The ARN of the network profile to associate with a device.
newAssociateDeviceWithNetworkProfile ::
  -- | 'deviceArn'
  Core.Text ->
  -- | 'networkProfileArn'
  Core.Text ->
  AssociateDeviceWithNetworkProfile
newAssociateDeviceWithNetworkProfile
  pDeviceArn_
  pNetworkProfileArn_ =
    AssociateDeviceWithNetworkProfile'
      { deviceArn =
          pDeviceArn_,
        networkProfileArn = pNetworkProfileArn_
      }

-- | The device ARN.
associateDeviceWithNetworkProfile_deviceArn :: Lens.Lens' AssociateDeviceWithNetworkProfile Core.Text
associateDeviceWithNetworkProfile_deviceArn = Lens.lens (\AssociateDeviceWithNetworkProfile' {deviceArn} -> deviceArn) (\s@AssociateDeviceWithNetworkProfile' {} a -> s {deviceArn = a} :: AssociateDeviceWithNetworkProfile)

-- | The ARN of the network profile to associate with a device.
associateDeviceWithNetworkProfile_networkProfileArn :: Lens.Lens' AssociateDeviceWithNetworkProfile Core.Text
associateDeviceWithNetworkProfile_networkProfileArn = Lens.lens (\AssociateDeviceWithNetworkProfile' {networkProfileArn} -> networkProfileArn) (\s@AssociateDeviceWithNetworkProfile' {} a -> s {networkProfileArn = a} :: AssociateDeviceWithNetworkProfile)

instance
  Core.AWSRequest
    AssociateDeviceWithNetworkProfile
  where
  type
    AWSResponse AssociateDeviceWithNetworkProfile =
      AssociateDeviceWithNetworkProfileResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          AssociateDeviceWithNetworkProfileResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    AssociateDeviceWithNetworkProfile

instance
  Core.NFData
    AssociateDeviceWithNetworkProfile

instance
  Core.ToHeaders
    AssociateDeviceWithNetworkProfile
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AlexaForBusiness.AssociateDeviceWithNetworkProfile" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance
  Core.ToJSON
    AssociateDeviceWithNetworkProfile
  where
  toJSON AssociateDeviceWithNetworkProfile' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("DeviceArn" Core..= deviceArn),
            Core.Just
              ("NetworkProfileArn" Core..= networkProfileArn)
          ]
      )

instance
  Core.ToPath
    AssociateDeviceWithNetworkProfile
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    AssociateDeviceWithNetworkProfile
  where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newAssociateDeviceWithNetworkProfileResponse' smart constructor.
data AssociateDeviceWithNetworkProfileResponse = AssociateDeviceWithNetworkProfileResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AssociateDeviceWithNetworkProfileResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'associateDeviceWithNetworkProfileResponse_httpStatus' - The response's http status code.
newAssociateDeviceWithNetworkProfileResponse ::
  -- | 'httpStatus'
  Core.Int ->
  AssociateDeviceWithNetworkProfileResponse
newAssociateDeviceWithNetworkProfileResponse
  pHttpStatus_ =
    AssociateDeviceWithNetworkProfileResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
associateDeviceWithNetworkProfileResponse_httpStatus :: Lens.Lens' AssociateDeviceWithNetworkProfileResponse Core.Int
associateDeviceWithNetworkProfileResponse_httpStatus = Lens.lens (\AssociateDeviceWithNetworkProfileResponse' {httpStatus} -> httpStatus) (\s@AssociateDeviceWithNetworkProfileResponse' {} a -> s {httpStatus = a} :: AssociateDeviceWithNetworkProfileResponse)

instance
  Core.NFData
    AssociateDeviceWithNetworkProfileResponse
