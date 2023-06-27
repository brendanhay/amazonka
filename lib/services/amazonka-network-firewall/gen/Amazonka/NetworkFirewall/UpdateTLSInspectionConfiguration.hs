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
-- Module      : Amazonka.NetworkFirewall.UpdateTLSInspectionConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the TLS inspection configuration settings for the specified TLS
-- inspection configuration. You use a TLS inspection configuration by
-- reference in one or more firewall policies. When you modify a TLS
-- inspection configuration, you modify all firewall policies that use the
-- TLS inspection configuration.
--
-- To update a TLS inspection configuration, first call
-- DescribeTLSInspectionConfiguration to retrieve the current
-- TLSInspectionConfiguration object, update the object as needed, and then
-- provide the updated object to this call.
module Amazonka.NetworkFirewall.UpdateTLSInspectionConfiguration
  ( -- * Creating a Request
    UpdateTLSInspectionConfiguration (..),
    newUpdateTLSInspectionConfiguration,

    -- * Request Lenses
    updateTLSInspectionConfiguration_description,
    updateTLSInspectionConfiguration_encryptionConfiguration,
    updateTLSInspectionConfiguration_tLSInspectionConfigurationArn,
    updateTLSInspectionConfiguration_tLSInspectionConfigurationName,
    updateTLSInspectionConfiguration_tLSInspectionConfiguration,
    updateTLSInspectionConfiguration_updateToken,

    -- * Destructuring the Response
    UpdateTLSInspectionConfigurationResponse (..),
    newUpdateTLSInspectionConfigurationResponse,

    -- * Response Lenses
    updateTLSInspectionConfigurationResponse_httpStatus,
    updateTLSInspectionConfigurationResponse_updateToken,
    updateTLSInspectionConfigurationResponse_tLSInspectionConfigurationResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.NetworkFirewall.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateTLSInspectionConfiguration' smart constructor.
data UpdateTLSInspectionConfiguration = UpdateTLSInspectionConfiguration'
  { -- | A description of the TLS inspection configuration.
    description :: Prelude.Maybe Prelude.Text,
    -- | A complex type that contains the Amazon Web Services KMS encryption
    -- configuration settings for your TLS inspection configuration.
    encryptionConfiguration :: Prelude.Maybe EncryptionConfiguration,
    -- | The Amazon Resource Name (ARN) of the TLS inspection configuration.
    tLSInspectionConfigurationArn :: Prelude.Maybe Prelude.Text,
    -- | The descriptive name of the TLS inspection configuration. You can\'t
    -- change the name of a TLS inspection configuration after you create it.
    tLSInspectionConfigurationName :: Prelude.Maybe Prelude.Text,
    -- | The object that defines a TLS inspection configuration. This, along with
    -- TLSInspectionConfigurationResponse, define the TLS inspection
    -- configuration. You can retrieve all objects for a TLS inspection
    -- configuration by calling DescribeTLSInspectionConfiguration.
    --
    -- Network Firewall uses a TLS inspection configuration to decrypt traffic.
    -- Network Firewall re-encrypts the traffic before sending it to its
    -- destination.
    --
    -- To use a TLS inspection configuration, you add it to a Network Firewall
    -- firewall policy, then you apply the firewall policy to a firewall.
    -- Network Firewall acts as a proxy service to decrypt and inspect inbound
    -- traffic. You can reference a TLS inspection configuration from more than
    -- one firewall policy, and you can use a firewall policy in more than one
    -- firewall. For more information about using TLS inspection
    -- configurations, see
    -- <https://docs.aws.amazon.com/network-firewall/latest/developerguide/tls-inspection.html Decrypting SSL\/TLS traffic with TLS inspection configurations>
    -- in the /Network Firewall Developer Guide/.
    tLSInspectionConfiguration :: TLSInspectionConfiguration,
    -- | A token used for optimistic locking. Network Firewall returns a token to
    -- your requests that access the TLS inspection configuration. The token
    -- marks the state of the TLS inspection configuration resource at the time
    -- of the request.
    --
    -- To make changes to the TLS inspection configuration, you provide the
    -- token in your request. Network Firewall uses the token to ensure that
    -- the TLS inspection configuration hasn\'t changed since you last
    -- retrieved it. If it has changed, the operation fails with an
    -- @InvalidTokenException@. If this happens, retrieve the TLS inspection
    -- configuration again to get a current copy of it with a current token.
    -- Reapply your changes as needed, then try the operation again using the
    -- new token.
    updateToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateTLSInspectionConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'updateTLSInspectionConfiguration_description' - A description of the TLS inspection configuration.
--
-- 'encryptionConfiguration', 'updateTLSInspectionConfiguration_encryptionConfiguration' - A complex type that contains the Amazon Web Services KMS encryption
-- configuration settings for your TLS inspection configuration.
--
-- 'tLSInspectionConfigurationArn', 'updateTLSInspectionConfiguration_tLSInspectionConfigurationArn' - The Amazon Resource Name (ARN) of the TLS inspection configuration.
--
-- 'tLSInspectionConfigurationName', 'updateTLSInspectionConfiguration_tLSInspectionConfigurationName' - The descriptive name of the TLS inspection configuration. You can\'t
-- change the name of a TLS inspection configuration after you create it.
--
-- 'tLSInspectionConfiguration', 'updateTLSInspectionConfiguration_tLSInspectionConfiguration' - The object that defines a TLS inspection configuration. This, along with
-- TLSInspectionConfigurationResponse, define the TLS inspection
-- configuration. You can retrieve all objects for a TLS inspection
-- configuration by calling DescribeTLSInspectionConfiguration.
--
-- Network Firewall uses a TLS inspection configuration to decrypt traffic.
-- Network Firewall re-encrypts the traffic before sending it to its
-- destination.
--
-- To use a TLS inspection configuration, you add it to a Network Firewall
-- firewall policy, then you apply the firewall policy to a firewall.
-- Network Firewall acts as a proxy service to decrypt and inspect inbound
-- traffic. You can reference a TLS inspection configuration from more than
-- one firewall policy, and you can use a firewall policy in more than one
-- firewall. For more information about using TLS inspection
-- configurations, see
-- <https://docs.aws.amazon.com/network-firewall/latest/developerguide/tls-inspection.html Decrypting SSL\/TLS traffic with TLS inspection configurations>
-- in the /Network Firewall Developer Guide/.
--
-- 'updateToken', 'updateTLSInspectionConfiguration_updateToken' - A token used for optimistic locking. Network Firewall returns a token to
-- your requests that access the TLS inspection configuration. The token
-- marks the state of the TLS inspection configuration resource at the time
-- of the request.
--
-- To make changes to the TLS inspection configuration, you provide the
-- token in your request. Network Firewall uses the token to ensure that
-- the TLS inspection configuration hasn\'t changed since you last
-- retrieved it. If it has changed, the operation fails with an
-- @InvalidTokenException@. If this happens, retrieve the TLS inspection
-- configuration again to get a current copy of it with a current token.
-- Reapply your changes as needed, then try the operation again using the
-- new token.
newUpdateTLSInspectionConfiguration ::
  -- | 'tLSInspectionConfiguration'
  TLSInspectionConfiguration ->
  -- | 'updateToken'
  Prelude.Text ->
  UpdateTLSInspectionConfiguration
newUpdateTLSInspectionConfiguration
  pTLSInspectionConfiguration_
  pUpdateToken_ =
    UpdateTLSInspectionConfiguration'
      { description =
          Prelude.Nothing,
        encryptionConfiguration = Prelude.Nothing,
        tLSInspectionConfigurationArn =
          Prelude.Nothing,
        tLSInspectionConfigurationName =
          Prelude.Nothing,
        tLSInspectionConfiguration =
          pTLSInspectionConfiguration_,
        updateToken = pUpdateToken_
      }

-- | A description of the TLS inspection configuration.
updateTLSInspectionConfiguration_description :: Lens.Lens' UpdateTLSInspectionConfiguration (Prelude.Maybe Prelude.Text)
updateTLSInspectionConfiguration_description = Lens.lens (\UpdateTLSInspectionConfiguration' {description} -> description) (\s@UpdateTLSInspectionConfiguration' {} a -> s {description = a} :: UpdateTLSInspectionConfiguration)

-- | A complex type that contains the Amazon Web Services KMS encryption
-- configuration settings for your TLS inspection configuration.
updateTLSInspectionConfiguration_encryptionConfiguration :: Lens.Lens' UpdateTLSInspectionConfiguration (Prelude.Maybe EncryptionConfiguration)
updateTLSInspectionConfiguration_encryptionConfiguration = Lens.lens (\UpdateTLSInspectionConfiguration' {encryptionConfiguration} -> encryptionConfiguration) (\s@UpdateTLSInspectionConfiguration' {} a -> s {encryptionConfiguration = a} :: UpdateTLSInspectionConfiguration)

-- | The Amazon Resource Name (ARN) of the TLS inspection configuration.
updateTLSInspectionConfiguration_tLSInspectionConfigurationArn :: Lens.Lens' UpdateTLSInspectionConfiguration (Prelude.Maybe Prelude.Text)
updateTLSInspectionConfiguration_tLSInspectionConfigurationArn = Lens.lens (\UpdateTLSInspectionConfiguration' {tLSInspectionConfigurationArn} -> tLSInspectionConfigurationArn) (\s@UpdateTLSInspectionConfiguration' {} a -> s {tLSInspectionConfigurationArn = a} :: UpdateTLSInspectionConfiguration)

-- | The descriptive name of the TLS inspection configuration. You can\'t
-- change the name of a TLS inspection configuration after you create it.
updateTLSInspectionConfiguration_tLSInspectionConfigurationName :: Lens.Lens' UpdateTLSInspectionConfiguration (Prelude.Maybe Prelude.Text)
updateTLSInspectionConfiguration_tLSInspectionConfigurationName = Lens.lens (\UpdateTLSInspectionConfiguration' {tLSInspectionConfigurationName} -> tLSInspectionConfigurationName) (\s@UpdateTLSInspectionConfiguration' {} a -> s {tLSInspectionConfigurationName = a} :: UpdateTLSInspectionConfiguration)

-- | The object that defines a TLS inspection configuration. This, along with
-- TLSInspectionConfigurationResponse, define the TLS inspection
-- configuration. You can retrieve all objects for a TLS inspection
-- configuration by calling DescribeTLSInspectionConfiguration.
--
-- Network Firewall uses a TLS inspection configuration to decrypt traffic.
-- Network Firewall re-encrypts the traffic before sending it to its
-- destination.
--
-- To use a TLS inspection configuration, you add it to a Network Firewall
-- firewall policy, then you apply the firewall policy to a firewall.
-- Network Firewall acts as a proxy service to decrypt and inspect inbound
-- traffic. You can reference a TLS inspection configuration from more than
-- one firewall policy, and you can use a firewall policy in more than one
-- firewall. For more information about using TLS inspection
-- configurations, see
-- <https://docs.aws.amazon.com/network-firewall/latest/developerguide/tls-inspection.html Decrypting SSL\/TLS traffic with TLS inspection configurations>
-- in the /Network Firewall Developer Guide/.
updateTLSInspectionConfiguration_tLSInspectionConfiguration :: Lens.Lens' UpdateTLSInspectionConfiguration TLSInspectionConfiguration
updateTLSInspectionConfiguration_tLSInspectionConfiguration = Lens.lens (\UpdateTLSInspectionConfiguration' {tLSInspectionConfiguration} -> tLSInspectionConfiguration) (\s@UpdateTLSInspectionConfiguration' {} a -> s {tLSInspectionConfiguration = a} :: UpdateTLSInspectionConfiguration)

-- | A token used for optimistic locking. Network Firewall returns a token to
-- your requests that access the TLS inspection configuration. The token
-- marks the state of the TLS inspection configuration resource at the time
-- of the request.
--
-- To make changes to the TLS inspection configuration, you provide the
-- token in your request. Network Firewall uses the token to ensure that
-- the TLS inspection configuration hasn\'t changed since you last
-- retrieved it. If it has changed, the operation fails with an
-- @InvalidTokenException@. If this happens, retrieve the TLS inspection
-- configuration again to get a current copy of it with a current token.
-- Reapply your changes as needed, then try the operation again using the
-- new token.
updateTLSInspectionConfiguration_updateToken :: Lens.Lens' UpdateTLSInspectionConfiguration Prelude.Text
updateTLSInspectionConfiguration_updateToken = Lens.lens (\UpdateTLSInspectionConfiguration' {updateToken} -> updateToken) (\s@UpdateTLSInspectionConfiguration' {} a -> s {updateToken = a} :: UpdateTLSInspectionConfiguration)

instance
  Core.AWSRequest
    UpdateTLSInspectionConfiguration
  where
  type
    AWSResponse UpdateTLSInspectionConfiguration =
      UpdateTLSInspectionConfigurationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateTLSInspectionConfigurationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "UpdateToken")
            Prelude.<*> (x Data..:> "TLSInspectionConfigurationResponse")
      )

instance
  Prelude.Hashable
    UpdateTLSInspectionConfiguration
  where
  hashWithSalt
    _salt
    UpdateTLSInspectionConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` description
        `Prelude.hashWithSalt` encryptionConfiguration
        `Prelude.hashWithSalt` tLSInspectionConfigurationArn
        `Prelude.hashWithSalt` tLSInspectionConfigurationName
        `Prelude.hashWithSalt` tLSInspectionConfiguration
        `Prelude.hashWithSalt` updateToken

instance
  Prelude.NFData
    UpdateTLSInspectionConfiguration
  where
  rnf UpdateTLSInspectionConfiguration' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf encryptionConfiguration
      `Prelude.seq` Prelude.rnf tLSInspectionConfigurationArn
      `Prelude.seq` Prelude.rnf tLSInspectionConfigurationName
      `Prelude.seq` Prelude.rnf tLSInspectionConfiguration
      `Prelude.seq` Prelude.rnf updateToken

instance
  Data.ToHeaders
    UpdateTLSInspectionConfiguration
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "NetworkFirewall_20201112.UpdateTLSInspectionConfiguration" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateTLSInspectionConfiguration where
  toJSON UpdateTLSInspectionConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Description" Data..=) Prelude.<$> description,
            ("EncryptionConfiguration" Data..=)
              Prelude.<$> encryptionConfiguration,
            ("TLSInspectionConfigurationArn" Data..=)
              Prelude.<$> tLSInspectionConfigurationArn,
            ("TLSInspectionConfigurationName" Data..=)
              Prelude.<$> tLSInspectionConfigurationName,
            Prelude.Just
              ( "TLSInspectionConfiguration"
                  Data..= tLSInspectionConfiguration
              ),
            Prelude.Just ("UpdateToken" Data..= updateToken)
          ]
      )

instance Data.ToPath UpdateTLSInspectionConfiguration where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    UpdateTLSInspectionConfiguration
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateTLSInspectionConfigurationResponse' smart constructor.
data UpdateTLSInspectionConfigurationResponse = UpdateTLSInspectionConfigurationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A token used for optimistic locking. Network Firewall returns a token to
    -- your requests that access the TLS inspection configuration. The token
    -- marks the state of the TLS inspection configuration resource at the time
    -- of the request.
    --
    -- To make changes to the TLS inspection configuration, you provide the
    -- token in your request. Network Firewall uses the token to ensure that
    -- the TLS inspection configuration hasn\'t changed since you last
    -- retrieved it. If it has changed, the operation fails with an
    -- @InvalidTokenException@. If this happens, retrieve the TLS inspection
    -- configuration again to get a current copy of it with a current token.
    -- Reapply your changes as needed, then try the operation again using the
    -- new token.
    updateToken :: Prelude.Text,
    -- | The high-level properties of a TLS inspection configuration. This, along
    -- with the TLSInspectionConfiguration, define the TLS inspection
    -- configuration. You can retrieve all objects for a TLS inspection
    -- configuration by calling DescribeTLSInspectionConfiguration.
    tLSInspectionConfigurationResponse :: TLSInspectionConfigurationResponse
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateTLSInspectionConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateTLSInspectionConfigurationResponse_httpStatus' - The response's http status code.
--
-- 'updateToken', 'updateTLSInspectionConfigurationResponse_updateToken' - A token used for optimistic locking. Network Firewall returns a token to
-- your requests that access the TLS inspection configuration. The token
-- marks the state of the TLS inspection configuration resource at the time
-- of the request.
--
-- To make changes to the TLS inspection configuration, you provide the
-- token in your request. Network Firewall uses the token to ensure that
-- the TLS inspection configuration hasn\'t changed since you last
-- retrieved it. If it has changed, the operation fails with an
-- @InvalidTokenException@. If this happens, retrieve the TLS inspection
-- configuration again to get a current copy of it with a current token.
-- Reapply your changes as needed, then try the operation again using the
-- new token.
--
-- 'tLSInspectionConfigurationResponse', 'updateTLSInspectionConfigurationResponse_tLSInspectionConfigurationResponse' - The high-level properties of a TLS inspection configuration. This, along
-- with the TLSInspectionConfiguration, define the TLS inspection
-- configuration. You can retrieve all objects for a TLS inspection
-- configuration by calling DescribeTLSInspectionConfiguration.
newUpdateTLSInspectionConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'updateToken'
  Prelude.Text ->
  -- | 'tLSInspectionConfigurationResponse'
  TLSInspectionConfigurationResponse ->
  UpdateTLSInspectionConfigurationResponse
newUpdateTLSInspectionConfigurationResponse
  pHttpStatus_
  pUpdateToken_
  pTLSInspectionConfigurationResponse_ =
    UpdateTLSInspectionConfigurationResponse'
      { httpStatus =
          pHttpStatus_,
        updateToken = pUpdateToken_,
        tLSInspectionConfigurationResponse =
          pTLSInspectionConfigurationResponse_
      }

-- | The response's http status code.
updateTLSInspectionConfigurationResponse_httpStatus :: Lens.Lens' UpdateTLSInspectionConfigurationResponse Prelude.Int
updateTLSInspectionConfigurationResponse_httpStatus = Lens.lens (\UpdateTLSInspectionConfigurationResponse' {httpStatus} -> httpStatus) (\s@UpdateTLSInspectionConfigurationResponse' {} a -> s {httpStatus = a} :: UpdateTLSInspectionConfigurationResponse)

-- | A token used for optimistic locking. Network Firewall returns a token to
-- your requests that access the TLS inspection configuration. The token
-- marks the state of the TLS inspection configuration resource at the time
-- of the request.
--
-- To make changes to the TLS inspection configuration, you provide the
-- token in your request. Network Firewall uses the token to ensure that
-- the TLS inspection configuration hasn\'t changed since you last
-- retrieved it. If it has changed, the operation fails with an
-- @InvalidTokenException@. If this happens, retrieve the TLS inspection
-- configuration again to get a current copy of it with a current token.
-- Reapply your changes as needed, then try the operation again using the
-- new token.
updateTLSInspectionConfigurationResponse_updateToken :: Lens.Lens' UpdateTLSInspectionConfigurationResponse Prelude.Text
updateTLSInspectionConfigurationResponse_updateToken = Lens.lens (\UpdateTLSInspectionConfigurationResponse' {updateToken} -> updateToken) (\s@UpdateTLSInspectionConfigurationResponse' {} a -> s {updateToken = a} :: UpdateTLSInspectionConfigurationResponse)

-- | The high-level properties of a TLS inspection configuration. This, along
-- with the TLSInspectionConfiguration, define the TLS inspection
-- configuration. You can retrieve all objects for a TLS inspection
-- configuration by calling DescribeTLSInspectionConfiguration.
updateTLSInspectionConfigurationResponse_tLSInspectionConfigurationResponse :: Lens.Lens' UpdateTLSInspectionConfigurationResponse TLSInspectionConfigurationResponse
updateTLSInspectionConfigurationResponse_tLSInspectionConfigurationResponse = Lens.lens (\UpdateTLSInspectionConfigurationResponse' {tLSInspectionConfigurationResponse} -> tLSInspectionConfigurationResponse) (\s@UpdateTLSInspectionConfigurationResponse' {} a -> s {tLSInspectionConfigurationResponse = a} :: UpdateTLSInspectionConfigurationResponse)

instance
  Prelude.NFData
    UpdateTLSInspectionConfigurationResponse
  where
  rnf UpdateTLSInspectionConfigurationResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf updateToken
      `Prelude.seq` Prelude.rnf tLSInspectionConfigurationResponse
