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
-- Module      : Amazonka.EC2.DescribeSecurityGroupReferences
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- [VPC only] Describes the VPCs on the other side of a VPC peering
-- connection that are referencing the security groups you\'ve specified in
-- this request.
module Amazonka.EC2.DescribeSecurityGroupReferences
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
describeSecurityGroupReferences_groupId = Lens.lens (\DescribeSecurityGroupReferences' {groupId} -> groupId) (\s@DescribeSecurityGroupReferences' {} a -> s {groupId = a} :: DescribeSecurityGroupReferences) Prelude.. Lens.coerced

instance
  Core.AWSRequest
    DescribeSecurityGroupReferences
  where
  type
    AWSResponse DescribeSecurityGroupReferences =
      DescribeSecurityGroupReferencesResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeSecurityGroupReferencesResponse'
            Prelude.<$> ( x
                            Data..@? "securityGroupReferenceSet"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeSecurityGroupReferences
  where
  hashWithSalt
    _salt
    DescribeSecurityGroupReferences' {..} =
      _salt
        `Prelude.hashWithSalt` dryRun
        `Prelude.hashWithSalt` groupId

instance
  Prelude.NFData
    DescribeSecurityGroupReferences
  where
  rnf DescribeSecurityGroupReferences' {..} =
    Prelude.rnf dryRun `Prelude.seq`
      Prelude.rnf groupId

instance
  Data.ToHeaders
    DescribeSecurityGroupReferences
  where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeSecurityGroupReferences where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeSecurityGroupReferences where
  toQuery DescribeSecurityGroupReferences' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "DescribeSecurityGroupReferences" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        Data.toQueryList "GroupId" groupId
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
describeSecurityGroupReferencesResponse_securityGroupReferenceSet = Lens.lens (\DescribeSecurityGroupReferencesResponse' {securityGroupReferenceSet} -> securityGroupReferenceSet) (\s@DescribeSecurityGroupReferencesResponse' {} a -> s {securityGroupReferenceSet = a} :: DescribeSecurityGroupReferencesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeSecurityGroupReferencesResponse_httpStatus :: Lens.Lens' DescribeSecurityGroupReferencesResponse Prelude.Int
describeSecurityGroupReferencesResponse_httpStatus = Lens.lens (\DescribeSecurityGroupReferencesResponse' {httpStatus} -> httpStatus) (\s@DescribeSecurityGroupReferencesResponse' {} a -> s {httpStatus = a} :: DescribeSecurityGroupReferencesResponse)

instance
  Prelude.NFData
    DescribeSecurityGroupReferencesResponse
  where
  rnf DescribeSecurityGroupReferencesResponse' {..} =
    Prelude.rnf securityGroupReferenceSet `Prelude.seq`
      Prelude.rnf httpStatus
