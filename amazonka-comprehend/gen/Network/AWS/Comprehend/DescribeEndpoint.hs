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
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeEndpoint' smart constructor.
data DescribeEndpoint = DescribeEndpoint'
  { -- | The Amazon Resource Number (ARN) of the endpoint being described.
    endpointArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  DescribeEndpoint
newDescribeEndpoint pEndpointArn_ =
  DescribeEndpoint' {endpointArn = pEndpointArn_}

-- | The Amazon Resource Number (ARN) of the endpoint being described.
describeEndpoint_endpointArn :: Lens.Lens' DescribeEndpoint Core.Text
describeEndpoint_endpointArn = Lens.lens (\DescribeEndpoint' {endpointArn} -> endpointArn) (\s@DescribeEndpoint' {} a -> s {endpointArn = a} :: DescribeEndpoint)

instance Core.AWSRequest DescribeEndpoint where
  type
    AWSResponse DescribeEndpoint =
      DescribeEndpointResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeEndpointResponse'
            Core.<$> (x Core..?> "EndpointProperties")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeEndpoint

instance Core.NFData DescribeEndpoint

instance Core.ToHeaders DescribeEndpoint where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Comprehend_20171127.DescribeEndpoint" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeEndpoint where
  toJSON DescribeEndpoint' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("EndpointArn" Core..= endpointArn)]
      )

instance Core.ToPath DescribeEndpoint where
  toPath = Core.const "/"

instance Core.ToQuery DescribeEndpoint where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeEndpointResponse' smart constructor.
data DescribeEndpointResponse = DescribeEndpointResponse'
  { -- | Describes information associated with the specific endpoint.
    endpointProperties :: Core.Maybe EndpointProperties,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  DescribeEndpointResponse
newDescribeEndpointResponse pHttpStatus_ =
  DescribeEndpointResponse'
    { endpointProperties =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Describes information associated with the specific endpoint.
describeEndpointResponse_endpointProperties :: Lens.Lens' DescribeEndpointResponse (Core.Maybe EndpointProperties)
describeEndpointResponse_endpointProperties = Lens.lens (\DescribeEndpointResponse' {endpointProperties} -> endpointProperties) (\s@DescribeEndpointResponse' {} a -> s {endpointProperties = a} :: DescribeEndpointResponse)

-- | The response's http status code.
describeEndpointResponse_httpStatus :: Lens.Lens' DescribeEndpointResponse Core.Int
describeEndpointResponse_httpStatus = Lens.lens (\DescribeEndpointResponse' {httpStatus} -> httpStatus) (\s@DescribeEndpointResponse' {} a -> s {httpStatus = a} :: DescribeEndpointResponse)

instance Core.NFData DescribeEndpointResponse
