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
-- Module      : Network.AWS.GameLift.DescribeVpcPeeringAuthorizations
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves valid VPC peering authorizations that are pending for the AWS
-- account. This operation returns all VPC peering authorizations and
-- requests for peering. This includes those initiated and received by this
-- account.
--
-- -   CreateVpcPeeringAuthorization
--
-- -   DescribeVpcPeeringAuthorizations
--
-- -   DeleteVpcPeeringAuthorization
--
-- -   CreateVpcPeeringConnection
--
-- -   DescribeVpcPeeringConnections
--
-- -   DeleteVpcPeeringConnection
module Network.AWS.GameLift.DescribeVpcPeeringAuthorizations
  ( -- * Creating a Request
    DescribeVpcPeeringAuthorizations (..),
    newDescribeVpcPeeringAuthorizations,

    -- * Destructuring the Response
    DescribeVpcPeeringAuthorizationsResponse (..),
    newDescribeVpcPeeringAuthorizationsResponse,

    -- * Response Lenses
    describeVpcPeeringAuthorizationsResponse_vpcPeeringAuthorizations,
    describeVpcPeeringAuthorizationsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeVpcPeeringAuthorizations' smart constructor.
data DescribeVpcPeeringAuthorizations = DescribeVpcPeeringAuthorizations'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeVpcPeeringAuthorizations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDescribeVpcPeeringAuthorizations ::
  DescribeVpcPeeringAuthorizations
newDescribeVpcPeeringAuthorizations =
  DescribeVpcPeeringAuthorizations'

instance
  Core.AWSRequest
    DescribeVpcPeeringAuthorizations
  where
  type
    AWSResponse DescribeVpcPeeringAuthorizations =
      DescribeVpcPeeringAuthorizationsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeVpcPeeringAuthorizationsResponse'
            Core.<$> ( x Core..?> "VpcPeeringAuthorizations"
                         Core..!@ Core.mempty
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    DescribeVpcPeeringAuthorizations

instance Core.NFData DescribeVpcPeeringAuthorizations

instance
  Core.ToHeaders
    DescribeVpcPeeringAuthorizations
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "GameLift.DescribeVpcPeeringAuthorizations" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeVpcPeeringAuthorizations where
  toJSON = Core.const (Core.Object Core.mempty)

instance Core.ToPath DescribeVpcPeeringAuthorizations where
  toPath = Core.const "/"

instance
  Core.ToQuery
    DescribeVpcPeeringAuthorizations
  where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeVpcPeeringAuthorizationsResponse' smart constructor.
data DescribeVpcPeeringAuthorizationsResponse = DescribeVpcPeeringAuthorizationsResponse'
  { -- | A collection of objects that describe all valid VPC peering operations
    -- for the current AWS account.
    vpcPeeringAuthorizations :: Core.Maybe [VpcPeeringAuthorization],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeVpcPeeringAuthorizationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vpcPeeringAuthorizations', 'describeVpcPeeringAuthorizationsResponse_vpcPeeringAuthorizations' - A collection of objects that describe all valid VPC peering operations
-- for the current AWS account.
--
-- 'httpStatus', 'describeVpcPeeringAuthorizationsResponse_httpStatus' - The response's http status code.
newDescribeVpcPeeringAuthorizationsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeVpcPeeringAuthorizationsResponse
newDescribeVpcPeeringAuthorizationsResponse
  pHttpStatus_ =
    DescribeVpcPeeringAuthorizationsResponse'
      { vpcPeeringAuthorizations =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | A collection of objects that describe all valid VPC peering operations
-- for the current AWS account.
describeVpcPeeringAuthorizationsResponse_vpcPeeringAuthorizations :: Lens.Lens' DescribeVpcPeeringAuthorizationsResponse (Core.Maybe [VpcPeeringAuthorization])
describeVpcPeeringAuthorizationsResponse_vpcPeeringAuthorizations = Lens.lens (\DescribeVpcPeeringAuthorizationsResponse' {vpcPeeringAuthorizations} -> vpcPeeringAuthorizations) (\s@DescribeVpcPeeringAuthorizationsResponse' {} a -> s {vpcPeeringAuthorizations = a} :: DescribeVpcPeeringAuthorizationsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeVpcPeeringAuthorizationsResponse_httpStatus :: Lens.Lens' DescribeVpcPeeringAuthorizationsResponse Core.Int
describeVpcPeeringAuthorizationsResponse_httpStatus = Lens.lens (\DescribeVpcPeeringAuthorizationsResponse' {httpStatus} -> httpStatus) (\s@DescribeVpcPeeringAuthorizationsResponse' {} a -> s {httpStatus = a} :: DescribeVpcPeeringAuthorizationsResponse)

instance
  Core.NFData
    DescribeVpcPeeringAuthorizationsResponse
