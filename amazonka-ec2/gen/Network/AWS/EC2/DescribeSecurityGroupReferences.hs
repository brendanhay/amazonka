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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeSecurityGroupReferences' smart constructor.
data DescribeSecurityGroupReferences = DescribeSecurityGroupReferences'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The IDs of the security groups in your account.
    groupId :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
        Prelude.Nothing,
      groupId = Prelude.mempty
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeSecurityGroupReferences_dryRun :: Lens.Lens' DescribeSecurityGroupReferences (Prelude.Maybe Prelude.Bool)
describeSecurityGroupReferences_dryRun = Lens.lens (\DescribeSecurityGroupReferences' {dryRun} -> dryRun) (\s@DescribeSecurityGroupReferences' {} a -> s {dryRun = a} :: DescribeSecurityGroupReferences)

-- | The IDs of the security groups in your account.
describeSecurityGroupReferences_groupId :: Lens.Lens' DescribeSecurityGroupReferences [Prelude.Text]
describeSecurityGroupReferences_groupId = Lens.lens (\DescribeSecurityGroupReferences' {groupId} -> groupId) (\s@DescribeSecurityGroupReferences' {} a -> s {groupId = a} :: DescribeSecurityGroupReferences) Prelude.. Lens._Coerce

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
            Prelude.<$> ( x Core..@? "securityGroupReferenceSet"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeSecurityGroupReferences

instance
  Prelude.NFData
    DescribeSecurityGroupReferences

instance
  Core.ToHeaders
    DescribeSecurityGroupReferences
  where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DescribeSecurityGroupReferences where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeSecurityGroupReferences where
  toQuery DescribeSecurityGroupReferences' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ( "DescribeSecurityGroupReferences" ::
                      Prelude.ByteString
                  ),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Core.=: dryRun,
        Core.toQueryList "GroupId" groupId
      ]

-- | /See:/ 'newDescribeSecurityGroupReferencesResponse' smart constructor.
data DescribeSecurityGroupReferencesResponse = DescribeSecurityGroupReferencesResponse'
  { -- | Information about the VPCs with the referencing security groups.
    securityGroupReferenceSet :: Prelude.Maybe [SecurityGroupReference],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DescribeSecurityGroupReferencesResponse
newDescribeSecurityGroupReferencesResponse
  pHttpStatus_ =
    DescribeSecurityGroupReferencesResponse'
      { securityGroupReferenceSet =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Information about the VPCs with the referencing security groups.
describeSecurityGroupReferencesResponse_securityGroupReferenceSet :: Lens.Lens' DescribeSecurityGroupReferencesResponse (Prelude.Maybe [SecurityGroupReference])
describeSecurityGroupReferencesResponse_securityGroupReferenceSet = Lens.lens (\DescribeSecurityGroupReferencesResponse' {securityGroupReferenceSet} -> securityGroupReferenceSet) (\s@DescribeSecurityGroupReferencesResponse' {} a -> s {securityGroupReferenceSet = a} :: DescribeSecurityGroupReferencesResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeSecurityGroupReferencesResponse_httpStatus :: Lens.Lens' DescribeSecurityGroupReferencesResponse Prelude.Int
describeSecurityGroupReferencesResponse_httpStatus = Lens.lens (\DescribeSecurityGroupReferencesResponse' {httpStatus} -> httpStatus) (\s@DescribeSecurityGroupReferencesResponse' {} a -> s {httpStatus = a} :: DescribeSecurityGroupReferencesResponse)

instance
  Prelude.NFData
    DescribeSecurityGroupReferencesResponse
