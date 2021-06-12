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
-- Module      : Network.AWS.AlexaBusiness.UpdateNetworkProfile
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a network profile by the network profile ARN.
module Network.AWS.AlexaBusiness.UpdateNetworkProfile
  ( -- * Creating a Request
    UpdateNetworkProfile (..),
    newUpdateNetworkProfile,

    -- * Request Lenses
    updateNetworkProfile_certificateAuthorityArn,
    updateNetworkProfile_trustAnchors,
    updateNetworkProfile_currentPassword,
    updateNetworkProfile_networkProfileName,
    updateNetworkProfile_description,
    updateNetworkProfile_nextPassword,
    updateNetworkProfile_networkProfileArn,

    -- * Destructuring the Response
    UpdateNetworkProfileResponse (..),
    newUpdateNetworkProfileResponse,

    -- * Response Lenses
    updateNetworkProfileResponse_httpStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateNetworkProfile' smart constructor.
data UpdateNetworkProfile = UpdateNetworkProfile'
  { -- | The ARN of the Private Certificate Authority (PCA) created in AWS
    -- Certificate Manager (ACM). This is used to issue certificates to the
    -- devices.
    certificateAuthorityArn :: Core.Maybe Core.Text,
    -- | The root certificate(s) of your authentication server that will be
    -- installed on your devices and used to trust your authentication server
    -- during EAP negotiation.
    trustAnchors :: Core.Maybe (Core.NonEmpty Core.Text),
    -- | The current password of the Wi-Fi network.
    currentPassword :: Core.Maybe (Core.Sensitive Core.Text),
    -- | The name of the network profile associated with a device.
    networkProfileName :: Core.Maybe Core.Text,
    -- | Detailed information about a device\'s network profile.
    description :: Core.Maybe Core.Text,
    -- | The next, or subsequent, password of the Wi-Fi network. This password is
    -- asynchronously transmitted to the device and is used when the password
    -- of the network changes to NextPassword.
    nextPassword :: Core.Maybe (Core.Sensitive Core.Text),
    -- | The ARN of the network profile associated with a device.
    networkProfileArn :: Core.Text
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateNetworkProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'certificateAuthorityArn', 'updateNetworkProfile_certificateAuthorityArn' - The ARN of the Private Certificate Authority (PCA) created in AWS
-- Certificate Manager (ACM). This is used to issue certificates to the
-- devices.
--
-- 'trustAnchors', 'updateNetworkProfile_trustAnchors' - The root certificate(s) of your authentication server that will be
-- installed on your devices and used to trust your authentication server
-- during EAP negotiation.
--
-- 'currentPassword', 'updateNetworkProfile_currentPassword' - The current password of the Wi-Fi network.
--
-- 'networkProfileName', 'updateNetworkProfile_networkProfileName' - The name of the network profile associated with a device.
--
-- 'description', 'updateNetworkProfile_description' - Detailed information about a device\'s network profile.
--
-- 'nextPassword', 'updateNetworkProfile_nextPassword' - The next, or subsequent, password of the Wi-Fi network. This password is
-- asynchronously transmitted to the device and is used when the password
-- of the network changes to NextPassword.
--
-- 'networkProfileArn', 'updateNetworkProfile_networkProfileArn' - The ARN of the network profile associated with a device.
newUpdateNetworkProfile ::
  -- | 'networkProfileArn'
  Core.Text ->
  UpdateNetworkProfile
newUpdateNetworkProfile pNetworkProfileArn_ =
  UpdateNetworkProfile'
    { certificateAuthorityArn =
        Core.Nothing,
      trustAnchors = Core.Nothing,
      currentPassword = Core.Nothing,
      networkProfileName = Core.Nothing,
      description = Core.Nothing,
      nextPassword = Core.Nothing,
      networkProfileArn = pNetworkProfileArn_
    }

-- | The ARN of the Private Certificate Authority (PCA) created in AWS
-- Certificate Manager (ACM). This is used to issue certificates to the
-- devices.
updateNetworkProfile_certificateAuthorityArn :: Lens.Lens' UpdateNetworkProfile (Core.Maybe Core.Text)
updateNetworkProfile_certificateAuthorityArn = Lens.lens (\UpdateNetworkProfile' {certificateAuthorityArn} -> certificateAuthorityArn) (\s@UpdateNetworkProfile' {} a -> s {certificateAuthorityArn = a} :: UpdateNetworkProfile)

-- | The root certificate(s) of your authentication server that will be
-- installed on your devices and used to trust your authentication server
-- during EAP negotiation.
updateNetworkProfile_trustAnchors :: Lens.Lens' UpdateNetworkProfile (Core.Maybe (Core.NonEmpty Core.Text))
updateNetworkProfile_trustAnchors = Lens.lens (\UpdateNetworkProfile' {trustAnchors} -> trustAnchors) (\s@UpdateNetworkProfile' {} a -> s {trustAnchors = a} :: UpdateNetworkProfile) Core.. Lens.mapping Lens._Coerce

-- | The current password of the Wi-Fi network.
updateNetworkProfile_currentPassword :: Lens.Lens' UpdateNetworkProfile (Core.Maybe Core.Text)
updateNetworkProfile_currentPassword = Lens.lens (\UpdateNetworkProfile' {currentPassword} -> currentPassword) (\s@UpdateNetworkProfile' {} a -> s {currentPassword = a} :: UpdateNetworkProfile) Core.. Lens.mapping Core._Sensitive

-- | The name of the network profile associated with a device.
updateNetworkProfile_networkProfileName :: Lens.Lens' UpdateNetworkProfile (Core.Maybe Core.Text)
updateNetworkProfile_networkProfileName = Lens.lens (\UpdateNetworkProfile' {networkProfileName} -> networkProfileName) (\s@UpdateNetworkProfile' {} a -> s {networkProfileName = a} :: UpdateNetworkProfile)

-- | Detailed information about a device\'s network profile.
updateNetworkProfile_description :: Lens.Lens' UpdateNetworkProfile (Core.Maybe Core.Text)
updateNetworkProfile_description = Lens.lens (\UpdateNetworkProfile' {description} -> description) (\s@UpdateNetworkProfile' {} a -> s {description = a} :: UpdateNetworkProfile)

-- | The next, or subsequent, password of the Wi-Fi network. This password is
-- asynchronously transmitted to the device and is used when the password
-- of the network changes to NextPassword.
updateNetworkProfile_nextPassword :: Lens.Lens' UpdateNetworkProfile (Core.Maybe Core.Text)
updateNetworkProfile_nextPassword = Lens.lens (\UpdateNetworkProfile' {nextPassword} -> nextPassword) (\s@UpdateNetworkProfile' {} a -> s {nextPassword = a} :: UpdateNetworkProfile) Core.. Lens.mapping Core._Sensitive

-- | The ARN of the network profile associated with a device.
updateNetworkProfile_networkProfileArn :: Lens.Lens' UpdateNetworkProfile Core.Text
updateNetworkProfile_networkProfileArn = Lens.lens (\UpdateNetworkProfile' {networkProfileArn} -> networkProfileArn) (\s@UpdateNetworkProfile' {} a -> s {networkProfileArn = a} :: UpdateNetworkProfile)

instance Core.AWSRequest UpdateNetworkProfile where
  type
    AWSResponse UpdateNetworkProfile =
      UpdateNetworkProfileResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateNetworkProfileResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateNetworkProfile

instance Core.NFData UpdateNetworkProfile

instance Core.ToHeaders UpdateNetworkProfile where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AlexaForBusiness.UpdateNetworkProfile" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateNetworkProfile where
  toJSON UpdateNetworkProfile' {..} =
    Core.object
      ( Core.catMaybes
          [ ("CertificateAuthorityArn" Core..=)
              Core.<$> certificateAuthorityArn,
            ("TrustAnchors" Core..=) Core.<$> trustAnchors,
            ("CurrentPassword" Core..=) Core.<$> currentPassword,
            ("NetworkProfileName" Core..=)
              Core.<$> networkProfileName,
            ("Description" Core..=) Core.<$> description,
            ("NextPassword" Core..=) Core.<$> nextPassword,
            Core.Just
              ("NetworkProfileArn" Core..= networkProfileArn)
          ]
      )

instance Core.ToPath UpdateNetworkProfile where
  toPath = Core.const "/"

instance Core.ToQuery UpdateNetworkProfile where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateNetworkProfileResponse' smart constructor.
data UpdateNetworkProfileResponse = UpdateNetworkProfileResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateNetworkProfileResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateNetworkProfileResponse_httpStatus' - The response's http status code.
newUpdateNetworkProfileResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdateNetworkProfileResponse
newUpdateNetworkProfileResponse pHttpStatus_ =
  UpdateNetworkProfileResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateNetworkProfileResponse_httpStatus :: Lens.Lens' UpdateNetworkProfileResponse Core.Int
updateNetworkProfileResponse_httpStatus = Lens.lens (\UpdateNetworkProfileResponse' {httpStatus} -> httpStatus) (\s@UpdateNetworkProfileResponse' {} a -> s {httpStatus = a} :: UpdateNetworkProfileResponse)

instance Core.NFData UpdateNetworkProfileResponse
