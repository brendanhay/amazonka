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
-- Module      : Amazonka.NetworkFirewall.DescribeTLSInspectionConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the data objects for the specified TLS inspection configuration.
module Amazonka.NetworkFirewall.DescribeTLSInspectionConfiguration
  ( -- * Creating a Request
    DescribeTLSInspectionConfiguration (..),
    newDescribeTLSInspectionConfiguration,

    -- * Request Lenses
    describeTLSInspectionConfiguration_tLSInspectionConfigurationArn,
    describeTLSInspectionConfiguration_tLSInspectionConfigurationName,

    -- * Destructuring the Response
    DescribeTLSInspectionConfigurationResponse (..),
    newDescribeTLSInspectionConfigurationResponse,

    -- * Response Lenses
    describeTLSInspectionConfigurationResponse_tLSInspectionConfiguration,
    describeTLSInspectionConfigurationResponse_httpStatus,
    describeTLSInspectionConfigurationResponse_updateToken,
    describeTLSInspectionConfigurationResponse_tLSInspectionConfigurationResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.NetworkFirewall.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeTLSInspectionConfiguration' smart constructor.
data DescribeTLSInspectionConfiguration = DescribeTLSInspectionConfiguration'
  { -- | The Amazon Resource Name (ARN) of the TLS inspection configuration.
    --
    -- You must specify the ARN or the name, and you can specify both.
    tLSInspectionConfigurationArn :: Prelude.Maybe Prelude.Text,
    -- | The descriptive name of the TLS inspection configuration. You can\'t
    -- change the name of a TLS inspection configuration after you create it.
    --
    -- You must specify the ARN or the name, and you can specify both.
    tLSInspectionConfigurationName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeTLSInspectionConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tLSInspectionConfigurationArn', 'describeTLSInspectionConfiguration_tLSInspectionConfigurationArn' - The Amazon Resource Name (ARN) of the TLS inspection configuration.
--
-- You must specify the ARN or the name, and you can specify both.
--
-- 'tLSInspectionConfigurationName', 'describeTLSInspectionConfiguration_tLSInspectionConfigurationName' - The descriptive name of the TLS inspection configuration. You can\'t
-- change the name of a TLS inspection configuration after you create it.
--
-- You must specify the ARN or the name, and you can specify both.
newDescribeTLSInspectionConfiguration ::
  DescribeTLSInspectionConfiguration
newDescribeTLSInspectionConfiguration =
  DescribeTLSInspectionConfiguration'
    { tLSInspectionConfigurationArn =
        Prelude.Nothing,
      tLSInspectionConfigurationName =
        Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the TLS inspection configuration.
--
-- You must specify the ARN or the name, and you can specify both.
describeTLSInspectionConfiguration_tLSInspectionConfigurationArn :: Lens.Lens' DescribeTLSInspectionConfiguration (Prelude.Maybe Prelude.Text)
describeTLSInspectionConfiguration_tLSInspectionConfigurationArn = Lens.lens (\DescribeTLSInspectionConfiguration' {tLSInspectionConfigurationArn} -> tLSInspectionConfigurationArn) (\s@DescribeTLSInspectionConfiguration' {} a -> s {tLSInspectionConfigurationArn = a} :: DescribeTLSInspectionConfiguration)

-- | The descriptive name of the TLS inspection configuration. You can\'t
-- change the name of a TLS inspection configuration after you create it.
--
-- You must specify the ARN or the name, and you can specify both.
describeTLSInspectionConfiguration_tLSInspectionConfigurationName :: Lens.Lens' DescribeTLSInspectionConfiguration (Prelude.Maybe Prelude.Text)
describeTLSInspectionConfiguration_tLSInspectionConfigurationName = Lens.lens (\DescribeTLSInspectionConfiguration' {tLSInspectionConfigurationName} -> tLSInspectionConfigurationName) (\s@DescribeTLSInspectionConfiguration' {} a -> s {tLSInspectionConfigurationName = a} :: DescribeTLSInspectionConfiguration)

instance
  Core.AWSRequest
    DescribeTLSInspectionConfiguration
  where
  type
    AWSResponse DescribeTLSInspectionConfiguration =
      DescribeTLSInspectionConfigurationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeTLSInspectionConfigurationResponse'
            Prelude.<$> (x Data..?> "TLSInspectionConfiguration")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "UpdateToken")
            Prelude.<*> (x Data..:> "TLSInspectionConfigurationResponse")
      )

instance
  Prelude.Hashable
    DescribeTLSInspectionConfiguration
  where
  hashWithSalt
    _salt
    DescribeTLSInspectionConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` tLSInspectionConfigurationArn
        `Prelude.hashWithSalt` tLSInspectionConfigurationName

instance
  Prelude.NFData
    DescribeTLSInspectionConfiguration
  where
  rnf DescribeTLSInspectionConfiguration' {..} =
    Prelude.rnf tLSInspectionConfigurationArn
      `Prelude.seq` Prelude.rnf tLSInspectionConfigurationName

instance
  Data.ToHeaders
    DescribeTLSInspectionConfiguration
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "NetworkFirewall_20201112.DescribeTLSInspectionConfiguration" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    DescribeTLSInspectionConfiguration
  where
  toJSON DescribeTLSInspectionConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("TLSInspectionConfigurationArn" Data..=)
              Prelude.<$> tLSInspectionConfigurationArn,
            ("TLSInspectionConfigurationName" Data..=)
              Prelude.<$> tLSInspectionConfigurationName
          ]
      )

instance
  Data.ToPath
    DescribeTLSInspectionConfiguration
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DescribeTLSInspectionConfiguration
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeTLSInspectionConfigurationResponse' smart constructor.
data DescribeTLSInspectionConfigurationResponse = DescribeTLSInspectionConfigurationResponse'
  { -- | The object that defines a TLS inspection configuration. This, along with
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
    tLSInspectionConfiguration :: Prelude.Maybe TLSInspectionConfiguration,
    -- | The response's http status code.
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
-- Create a value of 'DescribeTLSInspectionConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tLSInspectionConfiguration', 'describeTLSInspectionConfigurationResponse_tLSInspectionConfiguration' - The object that defines a TLS inspection configuration. This, along with
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
-- 'httpStatus', 'describeTLSInspectionConfigurationResponse_httpStatus' - The response's http status code.
--
-- 'updateToken', 'describeTLSInspectionConfigurationResponse_updateToken' - A token used for optimistic locking. Network Firewall returns a token to
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
-- 'tLSInspectionConfigurationResponse', 'describeTLSInspectionConfigurationResponse_tLSInspectionConfigurationResponse' - The high-level properties of a TLS inspection configuration. This, along
-- with the TLSInspectionConfiguration, define the TLS inspection
-- configuration. You can retrieve all objects for a TLS inspection
-- configuration by calling DescribeTLSInspectionConfiguration.
newDescribeTLSInspectionConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'updateToken'
  Prelude.Text ->
  -- | 'tLSInspectionConfigurationResponse'
  TLSInspectionConfigurationResponse ->
  DescribeTLSInspectionConfigurationResponse
newDescribeTLSInspectionConfigurationResponse
  pHttpStatus_
  pUpdateToken_
  pTLSInspectionConfigurationResponse_ =
    DescribeTLSInspectionConfigurationResponse'
      { tLSInspectionConfiguration =
          Prelude.Nothing,
        httpStatus = pHttpStatus_,
        updateToken = pUpdateToken_,
        tLSInspectionConfigurationResponse =
          pTLSInspectionConfigurationResponse_
      }

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
describeTLSInspectionConfigurationResponse_tLSInspectionConfiguration :: Lens.Lens' DescribeTLSInspectionConfigurationResponse (Prelude.Maybe TLSInspectionConfiguration)
describeTLSInspectionConfigurationResponse_tLSInspectionConfiguration = Lens.lens (\DescribeTLSInspectionConfigurationResponse' {tLSInspectionConfiguration} -> tLSInspectionConfiguration) (\s@DescribeTLSInspectionConfigurationResponse' {} a -> s {tLSInspectionConfiguration = a} :: DescribeTLSInspectionConfigurationResponse)

-- | The response's http status code.
describeTLSInspectionConfigurationResponse_httpStatus :: Lens.Lens' DescribeTLSInspectionConfigurationResponse Prelude.Int
describeTLSInspectionConfigurationResponse_httpStatus = Lens.lens (\DescribeTLSInspectionConfigurationResponse' {httpStatus} -> httpStatus) (\s@DescribeTLSInspectionConfigurationResponse' {} a -> s {httpStatus = a} :: DescribeTLSInspectionConfigurationResponse)

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
describeTLSInspectionConfigurationResponse_updateToken :: Lens.Lens' DescribeTLSInspectionConfigurationResponse Prelude.Text
describeTLSInspectionConfigurationResponse_updateToken = Lens.lens (\DescribeTLSInspectionConfigurationResponse' {updateToken} -> updateToken) (\s@DescribeTLSInspectionConfigurationResponse' {} a -> s {updateToken = a} :: DescribeTLSInspectionConfigurationResponse)

-- | The high-level properties of a TLS inspection configuration. This, along
-- with the TLSInspectionConfiguration, define the TLS inspection
-- configuration. You can retrieve all objects for a TLS inspection
-- configuration by calling DescribeTLSInspectionConfiguration.
describeTLSInspectionConfigurationResponse_tLSInspectionConfigurationResponse :: Lens.Lens' DescribeTLSInspectionConfigurationResponse TLSInspectionConfigurationResponse
describeTLSInspectionConfigurationResponse_tLSInspectionConfigurationResponse = Lens.lens (\DescribeTLSInspectionConfigurationResponse' {tLSInspectionConfigurationResponse} -> tLSInspectionConfigurationResponse) (\s@DescribeTLSInspectionConfigurationResponse' {} a -> s {tLSInspectionConfigurationResponse = a} :: DescribeTLSInspectionConfigurationResponse)

instance
  Prelude.NFData
    DescribeTLSInspectionConfigurationResponse
  where
  rnf DescribeTLSInspectionConfigurationResponse' {..} =
    Prelude.rnf tLSInspectionConfiguration
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf updateToken
      `Prelude.seq` Prelude.rnf tLSInspectionConfigurationResponse
