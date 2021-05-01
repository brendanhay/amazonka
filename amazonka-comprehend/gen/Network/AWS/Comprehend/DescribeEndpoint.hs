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
-- Module      : Network.AWS.Comprehend.DescribeEndpoint
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the properties associated with a specific endpoint. Use this
-- operation to get the status of an endpoint.
module Network.AWS.Comprehend.DescribeEndpoint
  ( -- * Creating a Request
    DescribeEndpoint (..),
    newDescribeEndpoint,

    -- * Request Lenses
    describeEndpoint_endpointArn,

    -- * Destructuring the Response
    DescribeEndpointResponse (..),
    newDescribeEndpointResponse,

    -- * Response Lenses
    describeEndpointResponse_endpointProperties,
    describeEndpointResponse_httpStatus,
  )
where

import Network.AWS.Comprehend.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeEndpoint' smart constructor.
data DescribeEndpoint = DescribeEndpoint'
  { -- | The Amazon Resource Number (ARN) of the endpoint being described.
    endpointArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DescribeEndpoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endpointArn', 'describeEndpoint_endpointArn' - The Amazon Resource Number (ARN) of the endpoint being described.
newDescribeEndpoint ::
  -- | 'endpointArn'
  Prelude.Text ->
  DescribeEndpoint
newDescribeEndpoint pEndpointArn_ =
  DescribeEndpoint' {endpointArn = pEndpointArn_}

-- | The Amazon Resource Number (ARN) of the endpoint being described.
describeEndpoint_endpointArn :: Lens.Lens' DescribeEndpoint Prelude.Text
describeEndpoint_endpointArn = Lens.lens (\DescribeEndpoint' {endpointArn} -> endpointArn) (\s@DescribeEndpoint' {} a -> s {endpointArn = a} :: DescribeEndpoint)

instance Prelude.AWSRequest DescribeEndpoint where
  type Rs DescribeEndpoint = DescribeEndpointResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeEndpointResponse'
            Prelude.<$> (x Prelude..?> "EndpointProperties")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeEndpoint

instance Prelude.NFData DescribeEndpoint

instance Prelude.ToHeaders DescribeEndpoint where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "Comprehend_20171127.DescribeEndpoint" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DescribeEndpoint where
  toJSON DescribeEndpoint' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("EndpointArn" Prelude..= endpointArn)
          ]
      )

instance Prelude.ToPath DescribeEndpoint where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DescribeEndpoint where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeEndpointResponse' smart constructor.
data DescribeEndpointResponse = DescribeEndpointResponse'
  { -- | Describes information associated with the specific endpoint.
    endpointProperties :: Prelude.Maybe EndpointProperties,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DescribeEndpointResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endpointProperties', 'describeEndpointResponse_endpointProperties' - Describes information associated with the specific endpoint.
--
-- 'httpStatus', 'describeEndpointResponse_httpStatus' - The response's http status code.
newDescribeEndpointResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeEndpointResponse
newDescribeEndpointResponse pHttpStatus_ =
  DescribeEndpointResponse'
    { endpointProperties =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Describes information associated with the specific endpoint.
describeEndpointResponse_endpointProperties :: Lens.Lens' DescribeEndpointResponse (Prelude.Maybe EndpointProperties)
describeEndpointResponse_endpointProperties = Lens.lens (\DescribeEndpointResponse' {endpointProperties} -> endpointProperties) (\s@DescribeEndpointResponse' {} a -> s {endpointProperties = a} :: DescribeEndpointResponse)

-- | The response's http status code.
describeEndpointResponse_httpStatus :: Lens.Lens' DescribeEndpointResponse Prelude.Int
describeEndpointResponse_httpStatus = Lens.lens (\DescribeEndpointResponse' {httpStatus} -> httpStatus) (\s@DescribeEndpointResponse' {} a -> s {httpStatus = a} :: DescribeEndpointResponse)

instance Prelude.NFData DescribeEndpointResponse
