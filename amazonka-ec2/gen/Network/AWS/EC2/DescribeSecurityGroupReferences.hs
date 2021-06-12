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
-- Module      : Network.AWS.EC2.DescribeSecurityGroupReferences
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- [VPC only] Describes the VPCs on the other side of a VPC peering
-- connection that are referencing the security groups you\'ve specified in
-- this request.
module Network.AWS.EC2.DescribeSecurityGroupReferences
  ( -- * Creating a Request
    DescribeSecurityGroupReferences (..),
    newDescribeSecurityGroupReferences,

    -- * Request Lenses
    describeSecurityGroupReferences_dryRun,
    describeSecurityGroupReferences_groupId,

    -- * Destructuring the Response
    DescribeSecurityGroupReferencesResponse (..),
    newDescribeSecurityGroupReferencesResponse,

    -- * Response Lenses
    describeSecurityGroupReferencesResponse_securityGroupReferenceSet,
    describeSecurityGroupReferencesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeSecurityGroupReferences' smart constructor.
data DescribeSecurityGroupReferences = DescribeSecurityGroupReferences'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | The IDs of the security groups in your account.
    groupId :: [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeSecurityGroupReferences' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'describeSecurityGroupReferences_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'groupId', 'describeSecurityGroupReferences_groupId' - The IDs of the security groups in your account.
newDescribeSecurityGroupReferences ::
  DescribeSecurityGroupReferences
newDescribeSecurityGroupReferences =
  DescribeSecurityGroupReferences'
    { dryRun =
        Core.Nothing,
      groupId = Core.mempty
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeSecurityGroupReferences_dryRun :: Lens.Lens' DescribeSecurityGroupReferences (Core.Maybe Core.Bool)
describeSecurityGroupReferences_dryRun = Lens.lens (\DescribeSecurityGroupReferences' {dryRun} -> dryRun) (\s@DescribeSecurityGroupReferences' {} a -> s {dryRun = a} :: DescribeSecurityGroupReferences)

-- | The IDs of the security groups in your account.
describeSecurityGroupReferences_groupId :: Lens.Lens' DescribeSecurityGroupReferences [Core.Text]
describeSecurityGroupReferences_groupId = Lens.lens (\DescribeSecurityGroupReferences' {groupId} -> groupId) (\s@DescribeSecurityGroupReferences' {} a -> s {groupId = a} :: DescribeSecurityGroupReferences) Core.. Lens._Coerce

instance
  Core.AWSRequest
    DescribeSecurityGroupReferences
  where
  type
    AWSResponse DescribeSecurityGroupReferences =
      DescribeSecurityGroupReferencesResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeSecurityGroupReferencesResponse'
            Core.<$> ( x Core..@? "securityGroupReferenceSet"
                         Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "item")
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    DescribeSecurityGroupReferences

instance Core.NFData DescribeSecurityGroupReferences

instance
  Core.ToHeaders
    DescribeSecurityGroupReferences
  where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeSecurityGroupReferences where
  toPath = Core.const "/"

instance Core.ToQuery DescribeSecurityGroupReferences where
  toQuery DescribeSecurityGroupReferences' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ( "DescribeSecurityGroupReferences" ::
                      Core.ByteString
                  ),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "DryRun" Core.=: dryRun,
        Core.toQueryList "GroupId" groupId
      ]

-- | /See:/ 'newDescribeSecurityGroupReferencesResponse' smart constructor.
data DescribeSecurityGroupReferencesResponse = DescribeSecurityGroupReferencesResponse'
  { -- | Information about the VPCs with the referencing security groups.
    securityGroupReferenceSet :: Core.Maybe [SecurityGroupReference],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeSecurityGroupReferencesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'securityGroupReferenceSet', 'describeSecurityGroupReferencesResponse_securityGroupReferenceSet' - Information about the VPCs with the referencing security groups.
--
-- 'httpStatus', 'describeSecurityGroupReferencesResponse_httpStatus' - The response's http status code.
newDescribeSecurityGroupReferencesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeSecurityGroupReferencesResponse
newDescribeSecurityGroupReferencesResponse
  pHttpStatus_ =
    DescribeSecurityGroupReferencesResponse'
      { securityGroupReferenceSet =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Information about the VPCs with the referencing security groups.
describeSecurityGroupReferencesResponse_securityGroupReferenceSet :: Lens.Lens' DescribeSecurityGroupReferencesResponse (Core.Maybe [SecurityGroupReference])
describeSecurityGroupReferencesResponse_securityGroupReferenceSet = Lens.lens (\DescribeSecurityGroupReferencesResponse' {securityGroupReferenceSet} -> securityGroupReferenceSet) (\s@DescribeSecurityGroupReferencesResponse' {} a -> s {securityGroupReferenceSet = a} :: DescribeSecurityGroupReferencesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeSecurityGroupReferencesResponse_httpStatus :: Lens.Lens' DescribeSecurityGroupReferencesResponse Core.Int
describeSecurityGroupReferencesResponse_httpStatus = Lens.lens (\DescribeSecurityGroupReferencesResponse' {httpStatus} -> httpStatus) (\s@DescribeSecurityGroupReferencesResponse' {} a -> s {httpStatus = a} :: DescribeSecurityGroupReferencesResponse)

instance
  Core.NFData
    DescribeSecurityGroupReferencesResponse
