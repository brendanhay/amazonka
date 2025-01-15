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
-- Module      : Amazonka.KinesisVideo.GetSignalingChannelEndpoint
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides an endpoint for the specified signaling channel to send and
-- receive messages. This API uses the
-- @SingleMasterChannelEndpointConfiguration@ input parameter, which
-- consists of the @Protocols@ and @Role@ properties.
--
-- @Protocols@ is used to determine the communication mechanism. For
-- example, if you specify @WSS@ as the protocol, this API produces a
-- secure websocket endpoint. If you specify @HTTPS@ as the protocol, this
-- API generates an HTTPS endpoint.
--
-- @Role@ determines the messaging permissions. A @MASTER@ role results in
-- this API generating an endpoint that a client can use to communicate
-- with any of the viewers on the channel. A @VIEWER@ role results in this
-- API generating an endpoint that a client can use to communicate only
-- with a @MASTER@.
module Amazonka.KinesisVideo.GetSignalingChannelEndpoint
  ( -- * Creating a Request
    GetSignalingChannelEndpoint (..),
    newGetSignalingChannelEndpoint,

    -- * Request Lenses
    getSignalingChannelEndpoint_singleMasterChannelEndpointConfiguration,
    getSignalingChannelEndpoint_channelARN,

    -- * Destructuring the Response
    GetSignalingChannelEndpointResponse (..),
    newGetSignalingChannelEndpointResponse,

    -- * Response Lenses
    getSignalingChannelEndpointResponse_resourceEndpointList,
    getSignalingChannelEndpointResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KinesisVideo.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetSignalingChannelEndpoint' smart constructor.
data GetSignalingChannelEndpoint = GetSignalingChannelEndpoint'
  { -- | A structure containing the endpoint configuration for the
    -- @SINGLE_MASTER@ channel type.
    singleMasterChannelEndpointConfiguration :: Prelude.Maybe SingleMasterChannelEndpointConfiguration,
    -- | The Amazon Resource Name (ARN) of the signalling channel for which you
    -- want to get an endpoint.
    channelARN :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSignalingChannelEndpoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'singleMasterChannelEndpointConfiguration', 'getSignalingChannelEndpoint_singleMasterChannelEndpointConfiguration' - A structure containing the endpoint configuration for the
-- @SINGLE_MASTER@ channel type.
--
-- 'channelARN', 'getSignalingChannelEndpoint_channelARN' - The Amazon Resource Name (ARN) of the signalling channel for which you
-- want to get an endpoint.
newGetSignalingChannelEndpoint ::
  -- | 'channelARN'
  Prelude.Text ->
  GetSignalingChannelEndpoint
newGetSignalingChannelEndpoint pChannelARN_ =
  GetSignalingChannelEndpoint'
    { singleMasterChannelEndpointConfiguration =
        Prelude.Nothing,
      channelARN = pChannelARN_
    }

-- | A structure containing the endpoint configuration for the
-- @SINGLE_MASTER@ channel type.
getSignalingChannelEndpoint_singleMasterChannelEndpointConfiguration :: Lens.Lens' GetSignalingChannelEndpoint (Prelude.Maybe SingleMasterChannelEndpointConfiguration)
getSignalingChannelEndpoint_singleMasterChannelEndpointConfiguration = Lens.lens (\GetSignalingChannelEndpoint' {singleMasterChannelEndpointConfiguration} -> singleMasterChannelEndpointConfiguration) (\s@GetSignalingChannelEndpoint' {} a -> s {singleMasterChannelEndpointConfiguration = a} :: GetSignalingChannelEndpoint)

-- | The Amazon Resource Name (ARN) of the signalling channel for which you
-- want to get an endpoint.
getSignalingChannelEndpoint_channelARN :: Lens.Lens' GetSignalingChannelEndpoint Prelude.Text
getSignalingChannelEndpoint_channelARN = Lens.lens (\GetSignalingChannelEndpoint' {channelARN} -> channelARN) (\s@GetSignalingChannelEndpoint' {} a -> s {channelARN = a} :: GetSignalingChannelEndpoint)

instance Core.AWSRequest GetSignalingChannelEndpoint where
  type
    AWSResponse GetSignalingChannelEndpoint =
      GetSignalingChannelEndpointResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSignalingChannelEndpointResponse'
            Prelude.<$> ( x
                            Data..?> "ResourceEndpointList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetSignalingChannelEndpoint where
  hashWithSalt _salt GetSignalingChannelEndpoint' {..} =
    _salt
      `Prelude.hashWithSalt` singleMasterChannelEndpointConfiguration
      `Prelude.hashWithSalt` channelARN

instance Prelude.NFData GetSignalingChannelEndpoint where
  rnf GetSignalingChannelEndpoint' {..} =
    Prelude.rnf
      singleMasterChannelEndpointConfiguration
      `Prelude.seq` Prelude.rnf channelARN

instance Data.ToHeaders GetSignalingChannelEndpoint where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON GetSignalingChannelEndpoint where
  toJSON GetSignalingChannelEndpoint' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("SingleMasterChannelEndpointConfiguration" Data..=)
              Prelude.<$> singleMasterChannelEndpointConfiguration,
            Prelude.Just ("ChannelARN" Data..= channelARN)
          ]
      )

instance Data.ToPath GetSignalingChannelEndpoint where
  toPath = Prelude.const "/getSignalingChannelEndpoint"

instance Data.ToQuery GetSignalingChannelEndpoint where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetSignalingChannelEndpointResponse' smart constructor.
data GetSignalingChannelEndpointResponse = GetSignalingChannelEndpointResponse'
  { -- | A list of endpoints for the specified signaling channel.
    resourceEndpointList :: Prelude.Maybe [ResourceEndpointListItem],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSignalingChannelEndpointResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceEndpointList', 'getSignalingChannelEndpointResponse_resourceEndpointList' - A list of endpoints for the specified signaling channel.
--
-- 'httpStatus', 'getSignalingChannelEndpointResponse_httpStatus' - The response's http status code.
newGetSignalingChannelEndpointResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetSignalingChannelEndpointResponse
newGetSignalingChannelEndpointResponse pHttpStatus_ =
  GetSignalingChannelEndpointResponse'
    { resourceEndpointList =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of endpoints for the specified signaling channel.
getSignalingChannelEndpointResponse_resourceEndpointList :: Lens.Lens' GetSignalingChannelEndpointResponse (Prelude.Maybe [ResourceEndpointListItem])
getSignalingChannelEndpointResponse_resourceEndpointList = Lens.lens (\GetSignalingChannelEndpointResponse' {resourceEndpointList} -> resourceEndpointList) (\s@GetSignalingChannelEndpointResponse' {} a -> s {resourceEndpointList = a} :: GetSignalingChannelEndpointResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getSignalingChannelEndpointResponse_httpStatus :: Lens.Lens' GetSignalingChannelEndpointResponse Prelude.Int
getSignalingChannelEndpointResponse_httpStatus = Lens.lens (\GetSignalingChannelEndpointResponse' {httpStatus} -> httpStatus) (\s@GetSignalingChannelEndpointResponse' {} a -> s {httpStatus = a} :: GetSignalingChannelEndpointResponse)

instance
  Prelude.NFData
    GetSignalingChannelEndpointResponse
  where
  rnf GetSignalingChannelEndpointResponse' {..} =
    Prelude.rnf resourceEndpointList `Prelude.seq`
      Prelude.rnf httpStatus
