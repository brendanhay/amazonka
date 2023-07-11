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
-- Module      : Amazonka.AlexaBusiness.CreateNetworkProfile
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a network profile with the specified details.
module Amazonka.AlexaBusiness.CreateNetworkProfile
  ( -- * Creating a Request
    CreateNetworkProfile (..),
    newCreateNetworkProfile,

    -- * Request Lenses
    createNetworkProfile_certificateAuthorityArn,
    createNetworkProfile_currentPassword,
    createNetworkProfile_description,
    createNetworkProfile_eapMethod,
    createNetworkProfile_nextPassword,
    createNetworkProfile_tags,
    createNetworkProfile_trustAnchors,
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

import Amazonka.AlexaBusiness.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateNetworkProfile' smart constructor.
data CreateNetworkProfile = CreateNetworkProfile'
  { -- | The ARN of the Private Certificate Authority (PCA) created in AWS
    -- Certificate Manager (ACM). This is used to issue certificates to the
    -- devices.
    certificateAuthorityArn :: Prelude.Maybe Prelude.Text,
    -- | The current password of the Wi-Fi network.
    currentPassword :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | Detailed information about a device\'s network profile.
    description :: Prelude.Maybe Prelude.Text,
    -- | The authentication standard that is used in the EAP framework.
    -- Currently, EAP_TLS is supported.
    eapMethod :: Prelude.Maybe NetworkEapMethod,
    -- | The next, or subsequent, password of the Wi-Fi network. This password is
    -- asynchronously transmitted to the device and is used when the password
    -- of the network changes to NextPassword.
    nextPassword :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The tags to be added to the specified resource. Do not provide system
    -- tags.
    tags :: Prelude.Maybe [Tag],
    -- | The root certificates of your authentication server that is installed on
    -- your devices and used to trust your authentication server during EAP
    -- negotiation.
    trustAnchors :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
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
-- 'currentPassword', 'createNetworkProfile_currentPassword' - The current password of the Wi-Fi network.
--
-- 'description', 'createNetworkProfile_description' - Detailed information about a device\'s network profile.
--
-- 'eapMethod', 'createNetworkProfile_eapMethod' - The authentication standard that is used in the EAP framework.
-- Currently, EAP_TLS is supported.
--
-- 'nextPassword', 'createNetworkProfile_nextPassword' - The next, or subsequent, password of the Wi-Fi network. This password is
-- asynchronously transmitted to the device and is used when the password
-- of the network changes to NextPassword.
--
-- 'tags', 'createNetworkProfile_tags' - The tags to be added to the specified resource. Do not provide system
-- tags.
--
-- 'trustAnchors', 'createNetworkProfile_trustAnchors' - The root certificates of your authentication server that is installed on
-- your devices and used to trust your authentication server during EAP
-- negotiation.
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
        currentPassword = Prelude.Nothing,
        description = Prelude.Nothing,
        eapMethod = Prelude.Nothing,
        nextPassword = Prelude.Nothing,
        tags = Prelude.Nothing,
        trustAnchors = Prelude.Nothing,
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

-- | The current password of the Wi-Fi network.
createNetworkProfile_currentPassword :: Lens.Lens' CreateNetworkProfile (Prelude.Maybe Prelude.Text)
createNetworkProfile_currentPassword = Lens.lens (\CreateNetworkProfile' {currentPassword} -> currentPassword) (\s@CreateNetworkProfile' {} a -> s {currentPassword = a} :: CreateNetworkProfile) Prelude.. Lens.mapping Data._Sensitive

-- | Detailed information about a device\'s network profile.
createNetworkProfile_description :: Lens.Lens' CreateNetworkProfile (Prelude.Maybe Prelude.Text)
createNetworkProfile_description = Lens.lens (\CreateNetworkProfile' {description} -> description) (\s@CreateNetworkProfile' {} a -> s {description = a} :: CreateNetworkProfile)

-- | The authentication standard that is used in the EAP framework.
-- Currently, EAP_TLS is supported.
createNetworkProfile_eapMethod :: Lens.Lens' CreateNetworkProfile (Prelude.Maybe NetworkEapMethod)
createNetworkProfile_eapMethod = Lens.lens (\CreateNetworkProfile' {eapMethod} -> eapMethod) (\s@CreateNetworkProfile' {} a -> s {eapMethod = a} :: CreateNetworkProfile)

-- | The next, or subsequent, password of the Wi-Fi network. This password is
-- asynchronously transmitted to the device and is used when the password
-- of the network changes to NextPassword.
createNetworkProfile_nextPassword :: Lens.Lens' CreateNetworkProfile (Prelude.Maybe Prelude.Text)
createNetworkProfile_nextPassword = Lens.lens (\CreateNetworkProfile' {nextPassword} -> nextPassword) (\s@CreateNetworkProfile' {} a -> s {nextPassword = a} :: CreateNetworkProfile) Prelude.. Lens.mapping Data._Sensitive

-- | The tags to be added to the specified resource. Do not provide system
-- tags.
createNetworkProfile_tags :: Lens.Lens' CreateNetworkProfile (Prelude.Maybe [Tag])
createNetworkProfile_tags = Lens.lens (\CreateNetworkProfile' {tags} -> tags) (\s@CreateNetworkProfile' {} a -> s {tags = a} :: CreateNetworkProfile) Prelude.. Lens.mapping Lens.coerced

-- | The root certificates of your authentication server that is installed on
-- your devices and used to trust your authentication server during EAP
-- negotiation.
createNetworkProfile_trustAnchors :: Lens.Lens' CreateNetworkProfile (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
createNetworkProfile_trustAnchors = Lens.lens (\CreateNetworkProfile' {trustAnchors} -> trustAnchors) (\s@CreateNetworkProfile' {} a -> s {trustAnchors = a} :: CreateNetworkProfile) Prelude.. Lens.mapping Lens.coerced

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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateNetworkProfileResponse'
            Prelude.<$> (x Data..?> "NetworkProfileArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateNetworkProfile where
  hashWithSalt _salt CreateNetworkProfile' {..} =
    _salt
      `Prelude.hashWithSalt` certificateAuthorityArn
      `Prelude.hashWithSalt` currentPassword
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` eapMethod
      `Prelude.hashWithSalt` nextPassword
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` trustAnchors
      `Prelude.hashWithSalt` networkProfileName
      `Prelude.hashWithSalt` ssid
      `Prelude.hashWithSalt` securityType
      `Prelude.hashWithSalt` clientRequestToken

instance Prelude.NFData CreateNetworkProfile where
  rnf CreateNetworkProfile' {..} =
    Prelude.rnf certificateAuthorityArn
      `Prelude.seq` Prelude.rnf currentPassword
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf eapMethod
      `Prelude.seq` Prelude.rnf nextPassword
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf trustAnchors
      `Prelude.seq` Prelude.rnf networkProfileName
      `Prelude.seq` Prelude.rnf ssid
      `Prelude.seq` Prelude.rnf securityType
      `Prelude.seq` Prelude.rnf clientRequestToken

instance Data.ToHeaders CreateNetworkProfile where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AlexaForBusiness.CreateNetworkProfile" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateNetworkProfile where
  toJSON CreateNetworkProfile' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CertificateAuthorityArn" Data..=)
              Prelude.<$> certificateAuthorityArn,
            ("CurrentPassword" Data..=)
              Prelude.<$> currentPassword,
            ("Description" Data..=) Prelude.<$> description,
            ("EapMethod" Data..=) Prelude.<$> eapMethod,
            ("NextPassword" Data..=) Prelude.<$> nextPassword,
            ("Tags" Data..=) Prelude.<$> tags,
            ("TrustAnchors" Data..=) Prelude.<$> trustAnchors,
            Prelude.Just
              ("NetworkProfileName" Data..= networkProfileName),
            Prelude.Just ("Ssid" Data..= ssid),
            Prelude.Just ("SecurityType" Data..= securityType),
            Prelude.Just
              ("ClientRequestToken" Data..= clientRequestToken)
          ]
      )

instance Data.ToPath CreateNetworkProfile where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateNetworkProfile where
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

instance Prelude.NFData CreateNetworkProfileResponse where
  rnf CreateNetworkProfileResponse' {..} =
    Prelude.rnf networkProfileArn
      `Prelude.seq` Prelude.rnf httpStatus
