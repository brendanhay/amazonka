{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.KinesisVideo.GetDataEndpoint
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets an endpoint for a specified stream for either reading or writing.
-- Use this endpoint in your application to read from the specified stream
-- (using the @GetMedia@ or @GetMediaForFragmentList@ operations) or write
-- to it (using the @PutMedia@ operation).
--
-- The returned endpoint does not have the API name appended. The client
-- needs to add the API name to the returned endpoint.
--
-- In the request, specify the stream either by @StreamName@ or
-- @StreamARN@.
module Network.AWS.KinesisVideo.GetDataEndpoint
  ( -- * Creating a Request
    GetDataEndpoint (..),
    newGetDataEndpoint,

    -- * Request Lenses
    getDataEndpoint_streamARN,
    getDataEndpoint_streamName,
    getDataEndpoint_aPIName,

    -- * Destructuring the Response
    GetDataEndpointResponse (..),
    newGetDataEndpointResponse,

    -- * Response Lenses
    getDataEndpointResponse_dataEndpoint,
    getDataEndpointResponse_httpStatus,
  )
where

import Network.AWS.KinesisVideo.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetDataEndpoint' smart constructor.
data GetDataEndpoint = GetDataEndpoint'
  { -- | The Amazon Resource Name (ARN) of the stream that you want to get the
    -- endpoint for. You must specify either this parameter or a @StreamName@
    -- in the request.
    streamARN :: Prelude.Maybe Prelude.Text,
    -- | The name of the stream that you want to get the endpoint for. You must
    -- specify either this parameter or a @StreamARN@ in the request.
    streamName :: Prelude.Maybe Prelude.Text,
    -- | The name of the API action for which to get an endpoint.
    aPIName :: APIName
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetDataEndpoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'streamARN', 'getDataEndpoint_streamARN' - The Amazon Resource Name (ARN) of the stream that you want to get the
-- endpoint for. You must specify either this parameter or a @StreamName@
-- in the request.
--
-- 'streamName', 'getDataEndpoint_streamName' - The name of the stream that you want to get the endpoint for. You must
-- specify either this parameter or a @StreamARN@ in the request.
--
-- 'aPIName', 'getDataEndpoint_aPIName' - The name of the API action for which to get an endpoint.
newGetDataEndpoint ::
  -- | 'aPIName'
  APIName ->
  GetDataEndpoint
newGetDataEndpoint pAPIName_ =
  GetDataEndpoint'
    { streamARN = Prelude.Nothing,
      streamName = Prelude.Nothing,
      aPIName = pAPIName_
    }

-- | The Amazon Resource Name (ARN) of the stream that you want to get the
-- endpoint for. You must specify either this parameter or a @StreamName@
-- in the request.
getDataEndpoint_streamARN :: Lens.Lens' GetDataEndpoint (Prelude.Maybe Prelude.Text)
getDataEndpoint_streamARN = Lens.lens (\GetDataEndpoint' {streamARN} -> streamARN) (\s@GetDataEndpoint' {} a -> s {streamARN = a} :: GetDataEndpoint)

-- | The name of the stream that you want to get the endpoint for. You must
-- specify either this parameter or a @StreamARN@ in the request.
getDataEndpoint_streamName :: Lens.Lens' GetDataEndpoint (Prelude.Maybe Prelude.Text)
getDataEndpoint_streamName = Lens.lens (\GetDataEndpoint' {streamName} -> streamName) (\s@GetDataEndpoint' {} a -> s {streamName = a} :: GetDataEndpoint)

-- | The name of the API action for which to get an endpoint.
getDataEndpoint_aPIName :: Lens.Lens' GetDataEndpoint APIName
getDataEndpoint_aPIName = Lens.lens (\GetDataEndpoint' {aPIName} -> aPIName) (\s@GetDataEndpoint' {} a -> s {aPIName = a} :: GetDataEndpoint)

instance Prelude.AWSRequest GetDataEndpoint where
  type Rs GetDataEndpoint = GetDataEndpointResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDataEndpointResponse'
            Prelude.<$> (x Prelude..?> "DataEndpoint")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetDataEndpoint

instance Prelude.NFData GetDataEndpoint

instance Prelude.ToHeaders GetDataEndpoint where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToJSON GetDataEndpoint where
  toJSON GetDataEndpoint' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("StreamARN" Prelude..=) Prelude.<$> streamARN,
            ("StreamName" Prelude..=) Prelude.<$> streamName,
            Prelude.Just ("APIName" Prelude..= aPIName)
          ]
      )

instance Prelude.ToPath GetDataEndpoint where
  toPath = Prelude.const "/getDataEndpoint"

instance Prelude.ToQuery GetDataEndpoint where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetDataEndpointResponse' smart constructor.
data GetDataEndpointResponse = GetDataEndpointResponse'
  { -- | The endpoint value. To read data from the stream or to write data to it,
    -- specify this endpoint in your application.
    dataEndpoint :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetDataEndpointResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataEndpoint', 'getDataEndpointResponse_dataEndpoint' - The endpoint value. To read data from the stream or to write data to it,
-- specify this endpoint in your application.
--
-- 'httpStatus', 'getDataEndpointResponse_httpStatus' - The response's http status code.
newGetDataEndpointResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetDataEndpointResponse
newGetDataEndpointResponse pHttpStatus_ =
  GetDataEndpointResponse'
    { dataEndpoint =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The endpoint value. To read data from the stream or to write data to it,
-- specify this endpoint in your application.
getDataEndpointResponse_dataEndpoint :: Lens.Lens' GetDataEndpointResponse (Prelude.Maybe Prelude.Text)
getDataEndpointResponse_dataEndpoint = Lens.lens (\GetDataEndpointResponse' {dataEndpoint} -> dataEndpoint) (\s@GetDataEndpointResponse' {} a -> s {dataEndpoint = a} :: GetDataEndpointResponse)

-- | The response's http status code.
getDataEndpointResponse_httpStatus :: Lens.Lens' GetDataEndpointResponse Prelude.Int
getDataEndpointResponse_httpStatus = Lens.lens (\GetDataEndpointResponse' {httpStatus} -> httpStatus) (\s@GetDataEndpointResponse' {} a -> s {httpStatus = a} :: GetDataEndpointResponse)

instance Prelude.NFData GetDataEndpointResponse
