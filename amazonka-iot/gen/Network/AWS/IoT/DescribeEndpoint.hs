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
-- Module      : Network.AWS.IoT.DescribeEndpoint
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a unique endpoint specific to the AWS account making the call.
module Network.AWS.IoT.DescribeEndpoint
  ( -- * Creating a Request
    DescribeEndpoint (..),
    newDescribeEndpoint,

    -- * Request Lenses
    describeEndpoint_endpointType,

    -- * Destructuring the Response
    DescribeEndpointResponse (..),
    newDescribeEndpointResponse,

    -- * Response Lenses
    describeEndpointResponse_endpointAddress,
    describeEndpointResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the DescribeEndpoint operation.
--
-- /See:/ 'newDescribeEndpoint' smart constructor.
data DescribeEndpoint = DescribeEndpoint'
  { -- | The endpoint type. Valid endpoint types include:
    --
    -- -   @iot:Data@ - Returns a VeriSign signed data endpoint.
    --
    -- -   @iot:Data-ATS@ - Returns an ATS signed data endpoint.
    --
    -- -   @iot:CredentialProvider@ - Returns an AWS IoT credentials provider
    --     API endpoint.
    --
    -- -   @iot:Jobs@ - Returns an AWS IoT device management Jobs API endpoint.
    --
    -- We strongly recommend that customers use the newer @iot:Data-ATS@
    -- endpoint type to avoid issues related to the widespread distrust of
    -- Symantec certificate authorities.
    endpointType :: Core.Maybe Core.Text
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
-- 'endpointType', 'describeEndpoint_endpointType' - The endpoint type. Valid endpoint types include:
--
-- -   @iot:Data@ - Returns a VeriSign signed data endpoint.
--
-- -   @iot:Data-ATS@ - Returns an ATS signed data endpoint.
--
-- -   @iot:CredentialProvider@ - Returns an AWS IoT credentials provider
--     API endpoint.
--
-- -   @iot:Jobs@ - Returns an AWS IoT device management Jobs API endpoint.
--
-- We strongly recommend that customers use the newer @iot:Data-ATS@
-- endpoint type to avoid issues related to the widespread distrust of
-- Symantec certificate authorities.
newDescribeEndpoint ::
  DescribeEndpoint
newDescribeEndpoint =
  DescribeEndpoint' {endpointType = Core.Nothing}

-- | The endpoint type. Valid endpoint types include:
--
-- -   @iot:Data@ - Returns a VeriSign signed data endpoint.
--
-- -   @iot:Data-ATS@ - Returns an ATS signed data endpoint.
--
-- -   @iot:CredentialProvider@ - Returns an AWS IoT credentials provider
--     API endpoint.
--
-- -   @iot:Jobs@ - Returns an AWS IoT device management Jobs API endpoint.
--
-- We strongly recommend that customers use the newer @iot:Data-ATS@
-- endpoint type to avoid issues related to the widespread distrust of
-- Symantec certificate authorities.
describeEndpoint_endpointType :: Lens.Lens' DescribeEndpoint (Core.Maybe Core.Text)
describeEndpoint_endpointType = Lens.lens (\DescribeEndpoint' {endpointType} -> endpointType) (\s@DescribeEndpoint' {} a -> s {endpointType = a} :: DescribeEndpoint)

instance Core.AWSRequest DescribeEndpoint where
  type
    AWSResponse DescribeEndpoint =
      DescribeEndpointResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeEndpointResponse'
            Core.<$> (x Core..?> "endpointAddress")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeEndpoint

instance Core.NFData DescribeEndpoint

instance Core.ToHeaders DescribeEndpoint where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeEndpoint where
  toPath = Core.const "/endpoint"

instance Core.ToQuery DescribeEndpoint where
  toQuery DescribeEndpoint' {..} =
    Core.mconcat ["endpointType" Core.=: endpointType]

-- | The output from the DescribeEndpoint operation.
--
-- /See:/ 'newDescribeEndpointResponse' smart constructor.
data DescribeEndpointResponse = DescribeEndpointResponse'
  { -- | The endpoint. The format of the endpoint is as follows:
    -- /identifier/.iot./region/.amazonaws.com.
    endpointAddress :: Core.Maybe Core.Text,
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
-- 'endpointAddress', 'describeEndpointResponse_endpointAddress' - The endpoint. The format of the endpoint is as follows:
-- /identifier/.iot./region/.amazonaws.com.
--
-- 'httpStatus', 'describeEndpointResponse_httpStatus' - The response's http status code.
newDescribeEndpointResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeEndpointResponse
newDescribeEndpointResponse pHttpStatus_ =
  DescribeEndpointResponse'
    { endpointAddress =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The endpoint. The format of the endpoint is as follows:
-- /identifier/.iot./region/.amazonaws.com.
describeEndpointResponse_endpointAddress :: Lens.Lens' DescribeEndpointResponse (Core.Maybe Core.Text)
describeEndpointResponse_endpointAddress = Lens.lens (\DescribeEndpointResponse' {endpointAddress} -> endpointAddress) (\s@DescribeEndpointResponse' {} a -> s {endpointAddress = a} :: DescribeEndpointResponse)

-- | The response's http status code.
describeEndpointResponse_httpStatus :: Lens.Lens' DescribeEndpointResponse Core.Int
describeEndpointResponse_httpStatus = Lens.lens (\DescribeEndpointResponse' {httpStatus} -> httpStatus) (\s@DescribeEndpointResponse' {} a -> s {httpStatus = a} :: DescribeEndpointResponse)

instance Core.NFData DescribeEndpointResponse
