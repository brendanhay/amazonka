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
-- Module      : Amazonka.Lightsail.UpdateInstanceMetadataOptions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the Amazon Lightsail instance metadata parameters on a running
-- or stopped instance. When you modify the parameters on a running
-- instance, the @GetInstance@ or @GetInstances@ API operation initially
-- responds with a state of @pending@. After the parameter modifications
-- are successfully applied, the state changes to @applied@ in subsequent
-- @GetInstance@ or @GetInstances@ API calls. For more information, see
-- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-configuring-instance-metadata-service Use IMDSv2 with an Amazon Lightsail instance>
-- in the /Amazon Lightsail Developer Guide/.
module Amazonka.Lightsail.UpdateInstanceMetadataOptions
  ( -- * Creating a Request
    UpdateInstanceMetadataOptions (..),
    newUpdateInstanceMetadataOptions,

    -- * Request Lenses
    updateInstanceMetadataOptions_httpPutResponseHopLimit,
    updateInstanceMetadataOptions_httpTokens,
    updateInstanceMetadataOptions_httpEndpoint,
    updateInstanceMetadataOptions_httpProtocolIpv6,
    updateInstanceMetadataOptions_instanceName,

    -- * Destructuring the Response
    UpdateInstanceMetadataOptionsResponse (..),
    newUpdateInstanceMetadataOptionsResponse,

    -- * Response Lenses
    updateInstanceMetadataOptionsResponse_operation,
    updateInstanceMetadataOptionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lightsail.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateInstanceMetadataOptions' smart constructor.
data UpdateInstanceMetadataOptions = UpdateInstanceMetadataOptions'
  { -- | The desired HTTP PUT response hop limit for instance metadata requests.
    -- A larger number means that the instance metadata requests can travel
    -- farther. If no parameter is specified, the existing state is maintained.
    httpPutResponseHopLimit :: Prelude.Maybe Prelude.Int,
    -- | The state of token usage for your instance metadata requests. If the
    -- parameter is not specified in the request, the default state is
    -- @optional@.
    --
    -- If the state is @optional@, you can choose whether to retrieve instance
    -- metadata with a signed token header on your request. If you retrieve the
    -- IAM role credentials without a token, the version 1.0 role credentials
    -- are returned. If you retrieve the IAM role credentials by using a valid
    -- signed token, the version 2.0 role credentials are returned.
    --
    -- If the state is @required@, you must send a signed token header with all
    -- instance metadata retrieval requests. In this state, retrieving the IAM
    -- role credential always returns the version 2.0 credentials. The version
    -- 1.0 credentials are not available.
    httpTokens :: Prelude.Maybe HttpTokens,
    -- | Enables or disables the HTTP metadata endpoint on your instances. If
    -- this parameter is not specified, the existing state is maintained.
    --
    -- If you specify a value of @disabled@, you cannot access your instance
    -- metadata.
    httpEndpoint :: Prelude.Maybe HttpEndpoint,
    -- | Enables or disables the IPv6 endpoint for the instance metadata service.
    -- This setting applies only when the HTTP metadata endpoint is enabled.
    --
    -- This parameter is available only for instances in the Europe (Stockholm)
    -- Amazon Web Services Region (@eu-north-1@).
    httpProtocolIpv6 :: Prelude.Maybe HttpProtocolIpv6,
    -- | The name of the instance for which to update metadata parameters.
    instanceName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateInstanceMetadataOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpPutResponseHopLimit', 'updateInstanceMetadataOptions_httpPutResponseHopLimit' - The desired HTTP PUT response hop limit for instance metadata requests.
-- A larger number means that the instance metadata requests can travel
-- farther. If no parameter is specified, the existing state is maintained.
--
-- 'httpTokens', 'updateInstanceMetadataOptions_httpTokens' - The state of token usage for your instance metadata requests. If the
-- parameter is not specified in the request, the default state is
-- @optional@.
--
-- If the state is @optional@, you can choose whether to retrieve instance
-- metadata with a signed token header on your request. If you retrieve the
-- IAM role credentials without a token, the version 1.0 role credentials
-- are returned. If you retrieve the IAM role credentials by using a valid
-- signed token, the version 2.0 role credentials are returned.
--
-- If the state is @required@, you must send a signed token header with all
-- instance metadata retrieval requests. In this state, retrieving the IAM
-- role credential always returns the version 2.0 credentials. The version
-- 1.0 credentials are not available.
--
-- 'httpEndpoint', 'updateInstanceMetadataOptions_httpEndpoint' - Enables or disables the HTTP metadata endpoint on your instances. If
-- this parameter is not specified, the existing state is maintained.
--
-- If you specify a value of @disabled@, you cannot access your instance
-- metadata.
--
-- 'httpProtocolIpv6', 'updateInstanceMetadataOptions_httpProtocolIpv6' - Enables or disables the IPv6 endpoint for the instance metadata service.
-- This setting applies only when the HTTP metadata endpoint is enabled.
--
-- This parameter is available only for instances in the Europe (Stockholm)
-- Amazon Web Services Region (@eu-north-1@).
--
-- 'instanceName', 'updateInstanceMetadataOptions_instanceName' - The name of the instance for which to update metadata parameters.
newUpdateInstanceMetadataOptions ::
  -- | 'instanceName'
  Prelude.Text ->
  UpdateInstanceMetadataOptions
newUpdateInstanceMetadataOptions pInstanceName_ =
  UpdateInstanceMetadataOptions'
    { httpPutResponseHopLimit =
        Prelude.Nothing,
      httpTokens = Prelude.Nothing,
      httpEndpoint = Prelude.Nothing,
      httpProtocolIpv6 = Prelude.Nothing,
      instanceName = pInstanceName_
    }

-- | The desired HTTP PUT response hop limit for instance metadata requests.
-- A larger number means that the instance metadata requests can travel
-- farther. If no parameter is specified, the existing state is maintained.
updateInstanceMetadataOptions_httpPutResponseHopLimit :: Lens.Lens' UpdateInstanceMetadataOptions (Prelude.Maybe Prelude.Int)
updateInstanceMetadataOptions_httpPutResponseHopLimit = Lens.lens (\UpdateInstanceMetadataOptions' {httpPutResponseHopLimit} -> httpPutResponseHopLimit) (\s@UpdateInstanceMetadataOptions' {} a -> s {httpPutResponseHopLimit = a} :: UpdateInstanceMetadataOptions)

-- | The state of token usage for your instance metadata requests. If the
-- parameter is not specified in the request, the default state is
-- @optional@.
--
-- If the state is @optional@, you can choose whether to retrieve instance
-- metadata with a signed token header on your request. If you retrieve the
-- IAM role credentials without a token, the version 1.0 role credentials
-- are returned. If you retrieve the IAM role credentials by using a valid
-- signed token, the version 2.0 role credentials are returned.
--
-- If the state is @required@, you must send a signed token header with all
-- instance metadata retrieval requests. In this state, retrieving the IAM
-- role credential always returns the version 2.0 credentials. The version
-- 1.0 credentials are not available.
updateInstanceMetadataOptions_httpTokens :: Lens.Lens' UpdateInstanceMetadataOptions (Prelude.Maybe HttpTokens)
updateInstanceMetadataOptions_httpTokens = Lens.lens (\UpdateInstanceMetadataOptions' {httpTokens} -> httpTokens) (\s@UpdateInstanceMetadataOptions' {} a -> s {httpTokens = a} :: UpdateInstanceMetadataOptions)

-- | Enables or disables the HTTP metadata endpoint on your instances. If
-- this parameter is not specified, the existing state is maintained.
--
-- If you specify a value of @disabled@, you cannot access your instance
-- metadata.
updateInstanceMetadataOptions_httpEndpoint :: Lens.Lens' UpdateInstanceMetadataOptions (Prelude.Maybe HttpEndpoint)
updateInstanceMetadataOptions_httpEndpoint = Lens.lens (\UpdateInstanceMetadataOptions' {httpEndpoint} -> httpEndpoint) (\s@UpdateInstanceMetadataOptions' {} a -> s {httpEndpoint = a} :: UpdateInstanceMetadataOptions)

-- | Enables or disables the IPv6 endpoint for the instance metadata service.
-- This setting applies only when the HTTP metadata endpoint is enabled.
--
-- This parameter is available only for instances in the Europe (Stockholm)
-- Amazon Web Services Region (@eu-north-1@).
updateInstanceMetadataOptions_httpProtocolIpv6 :: Lens.Lens' UpdateInstanceMetadataOptions (Prelude.Maybe HttpProtocolIpv6)
updateInstanceMetadataOptions_httpProtocolIpv6 = Lens.lens (\UpdateInstanceMetadataOptions' {httpProtocolIpv6} -> httpProtocolIpv6) (\s@UpdateInstanceMetadataOptions' {} a -> s {httpProtocolIpv6 = a} :: UpdateInstanceMetadataOptions)

-- | The name of the instance for which to update metadata parameters.
updateInstanceMetadataOptions_instanceName :: Lens.Lens' UpdateInstanceMetadataOptions Prelude.Text
updateInstanceMetadataOptions_instanceName = Lens.lens (\UpdateInstanceMetadataOptions' {instanceName} -> instanceName) (\s@UpdateInstanceMetadataOptions' {} a -> s {instanceName = a} :: UpdateInstanceMetadataOptions)

instance
  Core.AWSRequest
    UpdateInstanceMetadataOptions
  where
  type
    AWSResponse UpdateInstanceMetadataOptions =
      UpdateInstanceMetadataOptionsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateInstanceMetadataOptionsResponse'
            Prelude.<$> (x Data..?> "operation")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    UpdateInstanceMetadataOptions
  where
  hashWithSalt _salt UpdateInstanceMetadataOptions' {..} =
    _salt
      `Prelude.hashWithSalt` httpPutResponseHopLimit
      `Prelude.hashWithSalt` httpTokens
      `Prelude.hashWithSalt` httpEndpoint
      `Prelude.hashWithSalt` httpProtocolIpv6
      `Prelude.hashWithSalt` instanceName

instance Prelude.NFData UpdateInstanceMetadataOptions where
  rnf UpdateInstanceMetadataOptions' {..} =
    Prelude.rnf httpPutResponseHopLimit
      `Prelude.seq` Prelude.rnf httpTokens
      `Prelude.seq` Prelude.rnf httpEndpoint
      `Prelude.seq` Prelude.rnf httpProtocolIpv6
      `Prelude.seq` Prelude.rnf instanceName

instance Data.ToHeaders UpdateInstanceMetadataOptions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Lightsail_20161128.UpdateInstanceMetadataOptions" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateInstanceMetadataOptions where
  toJSON UpdateInstanceMetadataOptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("httpPutResponseHopLimit" Data..=)
              Prelude.<$> httpPutResponseHopLimit,
            ("httpTokens" Data..=) Prelude.<$> httpTokens,
            ("httpEndpoint" Data..=) Prelude.<$> httpEndpoint,
            ("httpProtocolIpv6" Data..=)
              Prelude.<$> httpProtocolIpv6,
            Prelude.Just ("instanceName" Data..= instanceName)
          ]
      )

instance Data.ToPath UpdateInstanceMetadataOptions where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateInstanceMetadataOptions where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateInstanceMetadataOptionsResponse' smart constructor.
data UpdateInstanceMetadataOptionsResponse = UpdateInstanceMetadataOptionsResponse'
  { operation :: Prelude.Maybe Operation,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateInstanceMetadataOptionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operation', 'updateInstanceMetadataOptionsResponse_operation' - Undocumented member.
--
-- 'httpStatus', 'updateInstanceMetadataOptionsResponse_httpStatus' - The response's http status code.
newUpdateInstanceMetadataOptionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateInstanceMetadataOptionsResponse
newUpdateInstanceMetadataOptionsResponse pHttpStatus_ =
  UpdateInstanceMetadataOptionsResponse'
    { operation =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
updateInstanceMetadataOptionsResponse_operation :: Lens.Lens' UpdateInstanceMetadataOptionsResponse (Prelude.Maybe Operation)
updateInstanceMetadataOptionsResponse_operation = Lens.lens (\UpdateInstanceMetadataOptionsResponse' {operation} -> operation) (\s@UpdateInstanceMetadataOptionsResponse' {} a -> s {operation = a} :: UpdateInstanceMetadataOptionsResponse)

-- | The response's http status code.
updateInstanceMetadataOptionsResponse_httpStatus :: Lens.Lens' UpdateInstanceMetadataOptionsResponse Prelude.Int
updateInstanceMetadataOptionsResponse_httpStatus = Lens.lens (\UpdateInstanceMetadataOptionsResponse' {httpStatus} -> httpStatus) (\s@UpdateInstanceMetadataOptionsResponse' {} a -> s {httpStatus = a} :: UpdateInstanceMetadataOptionsResponse)

instance
  Prelude.NFData
    UpdateInstanceMetadataOptionsResponse
  where
  rnf UpdateInstanceMetadataOptionsResponse' {..} =
    Prelude.rnf operation
      `Prelude.seq` Prelude.rnf httpStatus
