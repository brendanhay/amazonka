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
-- Module      : Amazonka.NetworkFirewall.CreateTLSInspectionConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an Network Firewall TLS inspection configuration. A TLS
-- inspection configuration contains the Certificate Manager certificate
-- references that Network Firewall uses to decrypt and re-encrypt inbound
-- traffic.
--
-- After you create a TLS inspection configuration, you associate it with a
-- firewall policy.
--
-- To update the settings for a TLS inspection configuration, use
-- UpdateTLSInspectionConfiguration.
--
-- To manage a TLS inspection configuration\'s tags, use the standard
-- Amazon Web Services resource tagging operations, ListTagsForResource,
-- TagResource, and UntagResource.
--
-- To retrieve information about TLS inspection configurations, use
-- ListTLSInspectionConfigurations and DescribeTLSInspectionConfiguration.
--
-- For more information about TLS inspection configurations, see
-- <https://docs.aws.amazon.com/network-firewall/latest/developerguide/tls-inspection.html Decrypting SSL\/TLS traffic with TLS inspection configurations>
-- in the /Network Firewall Developer Guide/.
module Amazonka.NetworkFirewall.CreateTLSInspectionConfiguration
  ( -- * Creating a Request
    CreateTLSInspectionConfiguration (..),
    newCreateTLSInspectionConfiguration,

    -- * Request Lenses
    createTLSInspectionConfiguration_description,
    createTLSInspectionConfiguration_encryptionConfiguration,
    createTLSInspectionConfiguration_tags,
    createTLSInspectionConfiguration_tLSInspectionConfigurationName,
    createTLSInspectionConfiguration_tLSInspectionConfiguration,

    -- * Destructuring the Response
    CreateTLSInspectionConfigurationResponse (..),
    newCreateTLSInspectionConfigurationResponse,

    -- * Response Lenses
    createTLSInspectionConfigurationResponse_httpStatus,
    createTLSInspectionConfigurationResponse_updateToken,
    createTLSInspectionConfigurationResponse_tLSInspectionConfigurationResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.NetworkFirewall.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateTLSInspectionConfiguration' smart constructor.
data CreateTLSInspectionConfiguration = CreateTLSInspectionConfiguration'
  { -- | A description of the TLS inspection configuration.
    description :: Prelude.Maybe Prelude.Text,
    encryptionConfiguration :: Prelude.Maybe EncryptionConfiguration,
    -- | The key:value pairs to associate with the resource.
    tags :: Prelude.Maybe (Prelude.NonEmpty Tag),
    -- | The descriptive name of the TLS inspection configuration. You can\'t
    -- change the name of a TLS inspection configuration after you create it.
    tLSInspectionConfigurationName :: Prelude.Text,
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
    tLSInspectionConfiguration :: TLSInspectionConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateTLSInspectionConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'createTLSInspectionConfiguration_description' - A description of the TLS inspection configuration.
--
-- 'encryptionConfiguration', 'createTLSInspectionConfiguration_encryptionConfiguration' - Undocumented member.
--
-- 'tags', 'createTLSInspectionConfiguration_tags' - The key:value pairs to associate with the resource.
--
-- 'tLSInspectionConfigurationName', 'createTLSInspectionConfiguration_tLSInspectionConfigurationName' - The descriptive name of the TLS inspection configuration. You can\'t
-- change the name of a TLS inspection configuration after you create it.
--
-- 'tLSInspectionConfiguration', 'createTLSInspectionConfiguration_tLSInspectionConfiguration' - The object that defines a TLS inspection configuration. This, along with
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
newCreateTLSInspectionConfiguration ::
  -- | 'tLSInspectionConfigurationName'
  Prelude.Text ->
  -- | 'tLSInspectionConfiguration'
  TLSInspectionConfiguration ->
  CreateTLSInspectionConfiguration
newCreateTLSInspectionConfiguration
  pTLSInspectionConfigurationName_
  pTLSInspectionConfiguration_ =
    CreateTLSInspectionConfiguration'
      { description =
          Prelude.Nothing,
        encryptionConfiguration = Prelude.Nothing,
        tags = Prelude.Nothing,
        tLSInspectionConfigurationName =
          pTLSInspectionConfigurationName_,
        tLSInspectionConfiguration =
          pTLSInspectionConfiguration_
      }

-- | A description of the TLS inspection configuration.
createTLSInspectionConfiguration_description :: Lens.Lens' CreateTLSInspectionConfiguration (Prelude.Maybe Prelude.Text)
createTLSInspectionConfiguration_description = Lens.lens (\CreateTLSInspectionConfiguration' {description} -> description) (\s@CreateTLSInspectionConfiguration' {} a -> s {description = a} :: CreateTLSInspectionConfiguration)

-- | Undocumented member.
createTLSInspectionConfiguration_encryptionConfiguration :: Lens.Lens' CreateTLSInspectionConfiguration (Prelude.Maybe EncryptionConfiguration)
createTLSInspectionConfiguration_encryptionConfiguration = Lens.lens (\CreateTLSInspectionConfiguration' {encryptionConfiguration} -> encryptionConfiguration) (\s@CreateTLSInspectionConfiguration' {} a -> s {encryptionConfiguration = a} :: CreateTLSInspectionConfiguration)

-- | The key:value pairs to associate with the resource.
createTLSInspectionConfiguration_tags :: Lens.Lens' CreateTLSInspectionConfiguration (Prelude.Maybe (Prelude.NonEmpty Tag))
createTLSInspectionConfiguration_tags = Lens.lens (\CreateTLSInspectionConfiguration' {tags} -> tags) (\s@CreateTLSInspectionConfiguration' {} a -> s {tags = a} :: CreateTLSInspectionConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The descriptive name of the TLS inspection configuration. You can\'t
-- change the name of a TLS inspection configuration after you create it.
createTLSInspectionConfiguration_tLSInspectionConfigurationName :: Lens.Lens' CreateTLSInspectionConfiguration Prelude.Text
createTLSInspectionConfiguration_tLSInspectionConfigurationName = Lens.lens (\CreateTLSInspectionConfiguration' {tLSInspectionConfigurationName} -> tLSInspectionConfigurationName) (\s@CreateTLSInspectionConfiguration' {} a -> s {tLSInspectionConfigurationName = a} :: CreateTLSInspectionConfiguration)

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
createTLSInspectionConfiguration_tLSInspectionConfiguration :: Lens.Lens' CreateTLSInspectionConfiguration TLSInspectionConfiguration
createTLSInspectionConfiguration_tLSInspectionConfiguration = Lens.lens (\CreateTLSInspectionConfiguration' {tLSInspectionConfiguration} -> tLSInspectionConfiguration) (\s@CreateTLSInspectionConfiguration' {} a -> s {tLSInspectionConfiguration = a} :: CreateTLSInspectionConfiguration)

instance
  Core.AWSRequest
    CreateTLSInspectionConfiguration
  where
  type
    AWSResponse CreateTLSInspectionConfiguration =
      CreateTLSInspectionConfigurationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateTLSInspectionConfigurationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "UpdateToken")
            Prelude.<*> (x Data..:> "TLSInspectionConfigurationResponse")
      )

instance
  Prelude.Hashable
    CreateTLSInspectionConfiguration
  where
  hashWithSalt
    _salt
    CreateTLSInspectionConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` description
        `Prelude.hashWithSalt` encryptionConfiguration
        `Prelude.hashWithSalt` tags
        `Prelude.hashWithSalt` tLSInspectionConfigurationName
        `Prelude.hashWithSalt` tLSInspectionConfiguration

instance
  Prelude.NFData
    CreateTLSInspectionConfiguration
  where
  rnf CreateTLSInspectionConfiguration' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf encryptionConfiguration
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf tLSInspectionConfigurationName
      `Prelude.seq` Prelude.rnf tLSInspectionConfiguration

instance
  Data.ToHeaders
    CreateTLSInspectionConfiguration
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "NetworkFirewall_20201112.CreateTLSInspectionConfiguration" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateTLSInspectionConfiguration where
  toJSON CreateTLSInspectionConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Description" Data..=) Prelude.<$> description,
            ("EncryptionConfiguration" Data..=)
              Prelude.<$> encryptionConfiguration,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just
              ( "TLSInspectionConfigurationName"
                  Data..= tLSInspectionConfigurationName
              ),
            Prelude.Just
              ( "TLSInspectionConfiguration"
                  Data..= tLSInspectionConfiguration
              )
          ]
      )

instance Data.ToPath CreateTLSInspectionConfiguration where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    CreateTLSInspectionConfiguration
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateTLSInspectionConfigurationResponse' smart constructor.
data CreateTLSInspectionConfigurationResponse = CreateTLSInspectionConfigurationResponse'
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
-- Create a value of 'CreateTLSInspectionConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createTLSInspectionConfigurationResponse_httpStatus' - The response's http status code.
--
-- 'updateToken', 'createTLSInspectionConfigurationResponse_updateToken' - A token used for optimistic locking. Network Firewall returns a token to
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
-- 'tLSInspectionConfigurationResponse', 'createTLSInspectionConfigurationResponse_tLSInspectionConfigurationResponse' - The high-level properties of a TLS inspection configuration. This, along
-- with the TLSInspectionConfiguration, define the TLS inspection
-- configuration. You can retrieve all objects for a TLS inspection
-- configuration by calling DescribeTLSInspectionConfiguration.
newCreateTLSInspectionConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'updateToken'
  Prelude.Text ->
  -- | 'tLSInspectionConfigurationResponse'
  TLSInspectionConfigurationResponse ->
  CreateTLSInspectionConfigurationResponse
newCreateTLSInspectionConfigurationResponse
  pHttpStatus_
  pUpdateToken_
  pTLSInspectionConfigurationResponse_ =
    CreateTLSInspectionConfigurationResponse'
      { httpStatus =
          pHttpStatus_,
        updateToken = pUpdateToken_,
        tLSInspectionConfigurationResponse =
          pTLSInspectionConfigurationResponse_
      }

-- | The response's http status code.
createTLSInspectionConfigurationResponse_httpStatus :: Lens.Lens' CreateTLSInspectionConfigurationResponse Prelude.Int
createTLSInspectionConfigurationResponse_httpStatus = Lens.lens (\CreateTLSInspectionConfigurationResponse' {httpStatus} -> httpStatus) (\s@CreateTLSInspectionConfigurationResponse' {} a -> s {httpStatus = a} :: CreateTLSInspectionConfigurationResponse)

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
createTLSInspectionConfigurationResponse_updateToken :: Lens.Lens' CreateTLSInspectionConfigurationResponse Prelude.Text
createTLSInspectionConfigurationResponse_updateToken = Lens.lens (\CreateTLSInspectionConfigurationResponse' {updateToken} -> updateToken) (\s@CreateTLSInspectionConfigurationResponse' {} a -> s {updateToken = a} :: CreateTLSInspectionConfigurationResponse)

-- | The high-level properties of a TLS inspection configuration. This, along
-- with the TLSInspectionConfiguration, define the TLS inspection
-- configuration. You can retrieve all objects for a TLS inspection
-- configuration by calling DescribeTLSInspectionConfiguration.
createTLSInspectionConfigurationResponse_tLSInspectionConfigurationResponse :: Lens.Lens' CreateTLSInspectionConfigurationResponse TLSInspectionConfigurationResponse
createTLSInspectionConfigurationResponse_tLSInspectionConfigurationResponse = Lens.lens (\CreateTLSInspectionConfigurationResponse' {tLSInspectionConfigurationResponse} -> tLSInspectionConfigurationResponse) (\s@CreateTLSInspectionConfigurationResponse' {} a -> s {tLSInspectionConfigurationResponse = a} :: CreateTLSInspectionConfigurationResponse)

instance
  Prelude.NFData
    CreateTLSInspectionConfigurationResponse
  where
  rnf CreateTLSInspectionConfigurationResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf updateToken
      `Prelude.seq` Prelude.rnf tLSInspectionConfigurationResponse
