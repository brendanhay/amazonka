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
-- Module      : Network.AWS.AlexaBusiness.CreateNetworkProfile
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a network profile with the specified details.
module Network.AWS.AlexaBusiness.CreateNetworkProfile
  ( -- * Creating a Request
    CreateNetworkProfile (..),
    newCreateNetworkProfile,

    -- * Request Lenses
    createNetworkProfile_certificateAuthorityArn,
    createNetworkProfile_trustAnchors,
    createNetworkProfile_currentPassword,
    createNetworkProfile_eapMethod,
    createNetworkProfile_tags,
    createNetworkProfile_description,
    createNetworkProfile_nextPassword,
    createNetworkProfile_networkProfileName,
    createNetworkProfile_ssid,
    createNetworkProfile_securityType,
    createNetworkProfile_clientRequestToken,

    -- * Destructuring the Response
    CreateNetworkProfileResponse (..),
    newCreateNetworkProfileResponse,

    -- * Response Lenses
    createNetworkProfileResponse_networkProfileArn,
    createNetworkProfileResponse_httpStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateNetworkProfile' smart constructor.
data CreateNetworkProfile = CreateNetworkProfile'
  { -- | The ARN of the Private Certificate Authority (PCA) created in AWS
    -- Certificate Manager (ACM). This is used to issue certificates to the
    -- devices.
    certificateAuthorityArn :: Prelude.Maybe Prelude.Text,
    -- | The root certificates of your authentication server that is installed on
    -- your devices and used to trust your authentication server during EAP
    -- negotiation.
    trustAnchors :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The current password of the Wi-Fi network.
    currentPassword :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The authentication standard that is used in the EAP framework.
    -- Currently, EAP_TLS is supported.
    eapMethod :: Prelude.Maybe NetworkEapMethod,
    -- | The tags to be added to the specified resource. Do not provide system
    -- tags.
    tags :: Prelude.Maybe [Tag],
    -- | Detailed information about a device\'s network profile.
    description :: Prelude.Maybe Prelude.Text,
    -- | The next, or subsequent, password of the Wi-Fi network. This password is
    -- asynchronously transmitted to the device and is used when the password
    -- of the network changes to NextPassword.
    nextPassword :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The name of the network profile associated with a device.
    networkProfileName :: Prelude.Text,
    -- | The SSID of the Wi-Fi network.
    ssid :: Prelude.Text,
    -- | The security type of the Wi-Fi network. This can be WPA2_ENTERPRISE,
    -- WPA2_PSK, WPA_PSK, WEP, or OPEN.
    securityType :: NetworkSecurityType,
    clientRequestToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateNetworkProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'certificateAuthorityArn', 'createNetworkProfile_certificateAuthorityArn' - The ARN of the Private Certificate Authority (PCA) created in AWS
-- Certificate Manager (ACM). This is used to issue certificates to the
-- devices.
--
-- 'trustAnchors', 'createNetworkProfile_trustAnchors' - The root certificates of your authentication server that is installed on
-- your devices and used to trust your authentication server during EAP
-- negotiation.
--
-- 'currentPassword', 'createNetworkProfile_currentPassword' - The current password of the Wi-Fi network.
--
-- 'eapMethod', 'createNetworkProfile_eapMethod' - The authentication standard that is used in the EAP framework.
-- Currently, EAP_TLS is supported.
--
-- 'tags', 'createNetworkProfile_tags' - The tags to be added to the specified resource. Do not provide system
-- tags.
--
-- 'description', 'createNetworkProfile_description' - Detailed information about a device\'s network profile.
--
-- 'nextPassword', 'createNetworkProfile_nextPassword' - The next, or subsequent, password of the Wi-Fi network. This password is
-- asynchronously transmitted to the device and is used when the password
-- of the network changes to NextPassword.
--
-- 'networkProfileName', 'createNetworkProfile_networkProfileName' - The name of the network profile associated with a device.
--
-- 'ssid', 'createNetworkProfile_ssid' - The SSID of the Wi-Fi network.
--
-- 'securityType', 'createNetworkProfile_securityType' - The security type of the Wi-Fi network. This can be WPA2_ENTERPRISE,
-- WPA2_PSK, WPA_PSK, WEP, or OPEN.
--
-- 'clientRequestToken', 'createNetworkProfile_clientRequestToken' - Undocumented member.
newCreateNetworkProfile ::
  -- | 'networkProfileName'
  Prelude.Text ->
  -- | 'ssid'
  Prelude.Text ->
  -- | 'securityType'
  NetworkSecurityType ->
  -- | 'clientRequestToken'
  Prelude.Text ->
  CreateNetworkProfile
newCreateNetworkProfile
  pNetworkProfileName_
  pSsid_
  pSecurityType_
  pClientRequestToken_ =
    CreateNetworkProfile'
      { certificateAuthorityArn =
          Prelude.Nothing,
        trustAnchors = Prelude.Nothing,
        currentPassword = Prelude.Nothing,
        eapMethod = Prelude.Nothing,
        tags = Prelude.Nothing,
        description = Prelude.Nothing,
        nextPassword = Prelude.Nothing,
        networkProfileName = pNetworkProfileName_,
        ssid = pSsid_,
        securityType = pSecurityType_,
        clientRequestToken = pClientRequestToken_
      }

-- | The ARN of the Private Certificate Authority (PCA) created in AWS
-- Certificate Manager (ACM). This is used to issue certificates to the
-- devices.
createNetworkProfile_certificateAuthorityArn :: Lens.Lens' CreateNetworkProfile (Prelude.Maybe Prelude.Text)
createNetworkProfile_certificateAuthorityArn = Lens.lens (\CreateNetworkProfile' {certificateAuthorityArn} -> certificateAuthorityArn) (\s@CreateNetworkProfile' {} a -> s {certificateAuthorityArn = a} :: CreateNetworkProfile)

-- | The root certificates of your authentication server that is installed on
-- your devices and used to trust your authentication server during EAP
-- negotiation.
createNetworkProfile_trustAnchors :: Lens.Lens' CreateNetworkProfile (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
createNetworkProfile_trustAnchors = Lens.lens (\CreateNetworkProfile' {trustAnchors} -> trustAnchors) (\s@CreateNetworkProfile' {} a -> s {trustAnchors = a} :: CreateNetworkProfile) Prelude.. Lens.mapping Lens._Coerce

-- | The current password of the Wi-Fi network.
createNetworkProfile_currentPassword :: Lens.Lens' CreateNetworkProfile (Prelude.Maybe Prelude.Text)
createNetworkProfile_currentPassword = Lens.lens (\CreateNetworkProfile' {currentPassword} -> currentPassword) (\s@CreateNetworkProfile' {} a -> s {currentPassword = a} :: CreateNetworkProfile) Prelude.. Lens.mapping Core._Sensitive

-- | The authentication standard that is used in the EAP framework.
-- Currently, EAP_TLS is supported.
createNetworkProfile_eapMethod :: Lens.Lens' CreateNetworkProfile (Prelude.Maybe NetworkEapMethod)
createNetworkProfile_eapMethod = Lens.lens (\CreateNetworkProfile' {eapMethod} -> eapMethod) (\s@CreateNetworkProfile' {} a -> s {eapMethod = a} :: CreateNetworkProfile)

-- | The tags to be added to the specified resource. Do not provide system
-- tags.
createNetworkProfile_tags :: Lens.Lens' CreateNetworkProfile (Prelude.Maybe [Tag])
createNetworkProfile_tags = Lens.lens (\CreateNetworkProfile' {tags} -> tags) (\s@CreateNetworkProfile' {} a -> s {tags = a} :: CreateNetworkProfile) Prelude.. Lens.mapping Lens._Coerce

-- | Detailed information about a device\'s network profile.
createNetworkProfile_description :: Lens.Lens' CreateNetworkProfile (Prelude.Maybe Prelude.Text)
createNetworkProfile_description = Lens.lens (\CreateNetworkProfile' {description} -> description) (\s@CreateNetworkProfile' {} a -> s {description = a} :: CreateNetworkProfile)

-- | The next, or subsequent, password of the Wi-Fi network. This password is
-- asynchronously transmitted to the device and is used when the password
-- of the network changes to NextPassword.
createNetworkProfile_nextPassword :: Lens.Lens' CreateNetworkProfile (Prelude.Maybe Prelude.Text)
createNetworkProfile_nextPassword = Lens.lens (\CreateNetworkProfile' {nextPassword} -> nextPassword) (\s@CreateNetworkProfile' {} a -> s {nextPassword = a} :: CreateNetworkProfile) Prelude.. Lens.mapping Core._Sensitive

-- | The name of the network profile associated with a device.
createNetworkProfile_networkProfileName :: Lens.Lens' CreateNetworkProfile Prelude.Text
createNetworkProfile_networkProfileName = Lens.lens (\CreateNetworkProfile' {networkProfileName} -> networkProfileName) (\s@CreateNetworkProfile' {} a -> s {networkProfileName = a} :: CreateNetworkProfile)

-- | The SSID of the Wi-Fi network.
createNetworkProfile_ssid :: Lens.Lens' CreateNetworkProfile Prelude.Text
createNetworkProfile_ssid = Lens.lens (\CreateNetworkProfile' {ssid} -> ssid) (\s@CreateNetworkProfile' {} a -> s {ssid = a} :: CreateNetworkProfile)

-- | The security type of the Wi-Fi network. This can be WPA2_ENTERPRISE,
-- WPA2_PSK, WPA_PSK, WEP, or OPEN.
createNetworkProfile_securityType :: Lens.Lens' CreateNetworkProfile NetworkSecurityType
createNetworkProfile_securityType = Lens.lens (\CreateNetworkProfile' {securityType} -> securityType) (\s@CreateNetworkProfile' {} a -> s {securityType = a} :: CreateNetworkProfile)

-- | Undocumented member.
createNetworkProfile_clientRequestToken :: Lens.Lens' CreateNetworkProfile Prelude.Text
createNetworkProfile_clientRequestToken = Lens.lens (\CreateNetworkProfile' {clientRequestToken} -> clientRequestToken) (\s@CreateNetworkProfile' {} a -> s {clientRequestToken = a} :: CreateNetworkProfile)

instance Core.AWSRequest CreateNetworkProfile where
  type
    AWSResponse CreateNetworkProfile =
      CreateNetworkProfileResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateNetworkProfileResponse'
            Prelude.<$> (x Core..?> "NetworkProfileArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateNetworkProfile

instance Prelude.NFData CreateNetworkProfile

instance Core.ToHeaders CreateNetworkProfile where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AlexaForBusiness.CreateNetworkProfile" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateNetworkProfile where
  toJSON CreateNetworkProfile' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("CertificateAuthorityArn" Core..=)
              Prelude.<$> certificateAuthorityArn,
            ("TrustAnchors" Core..=) Prelude.<$> trustAnchors,
            ("CurrentPassword" Core..=)
              Prelude.<$> currentPassword,
            ("EapMethod" Core..=) Prelude.<$> eapMethod,
            ("Tags" Core..=) Prelude.<$> tags,
            ("Description" Core..=) Prelude.<$> description,
            ("NextPassword" Core..=) Prelude.<$> nextPassword,
            Prelude.Just
              ("NetworkProfileName" Core..= networkProfileName),
            Prelude.Just ("Ssid" Core..= ssid),
            Prelude.Just ("SecurityType" Core..= securityType),
            Prelude.Just
              ("ClientRequestToken" Core..= clientRequestToken)
          ]
      )

instance Core.ToPath CreateNetworkProfile where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateNetworkProfile where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateNetworkProfileResponse' smart constructor.
data CreateNetworkProfileResponse = CreateNetworkProfileResponse'
  { -- | The ARN of the network profile associated with a device.
    networkProfileArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateNetworkProfileResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'networkProfileArn', 'createNetworkProfileResponse_networkProfileArn' - The ARN of the network profile associated with a device.
--
-- 'httpStatus', 'createNetworkProfileResponse_httpStatus' - The response's http status code.
newCreateNetworkProfileResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateNetworkProfileResponse
newCreateNetworkProfileResponse pHttpStatus_ =
  CreateNetworkProfileResponse'
    { networkProfileArn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the network profile associated with a device.
createNetworkProfileResponse_networkProfileArn :: Lens.Lens' CreateNetworkProfileResponse (Prelude.Maybe Prelude.Text)
createNetworkProfileResponse_networkProfileArn = Lens.lens (\CreateNetworkProfileResponse' {networkProfileArn} -> networkProfileArn) (\s@CreateNetworkProfileResponse' {} a -> s {networkProfileArn = a} :: CreateNetworkProfileResponse)

-- | The response's http status code.
createNetworkProfileResponse_httpStatus :: Lens.Lens' CreateNetworkProfileResponse Prelude.Int
createNetworkProfileResponse_httpStatus = Lens.lens (\CreateNetworkProfileResponse' {httpStatus} -> httpStatus) (\s@CreateNetworkProfileResponse' {} a -> s {httpStatus = a} :: CreateNetworkProfileResponse)

instance Prelude.NFData CreateNetworkProfileResponse
