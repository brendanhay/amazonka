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
-- Module      : Network.AWS.RDS.DescribeDBClusterSnapshotAttributes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of DB cluster snapshot attribute names and values for a
-- manual DB cluster snapshot.
--
-- When sharing snapshots with other AWS accounts,
-- @DescribeDBClusterSnapshotAttributes@ returns the @restore@ attribute
-- and a list of IDs for the AWS accounts that are authorized to copy or
-- restore the manual DB cluster snapshot. If @all@ is included in the list
-- of values for the @restore@ attribute, then the manual DB cluster
-- snapshot is public and can be copied or restored by all AWS accounts.
--
-- To add or remove access for an AWS account to copy or restore a manual
-- DB cluster snapshot, or to make the manual DB cluster snapshot public or
-- private, use the @ModifyDBClusterSnapshotAttribute@ API action.
--
-- This action only applies to Aurora DB clusters.
module Network.AWS.RDS.DescribeDBClusterSnapshotAttributes
  ( -- * Creating a Request
    DescribeDBClusterSnapshotAttributes (..),
    newDescribeDBClusterSnapshotAttributes,

    -- * Request Lenses
    describeDBClusterSnapshotAttributes_dbClusterSnapshotIdentifier,

    -- * Destructuring the Response
    DescribeDBClusterSnapshotAttributesResponse (..),
    newDescribeDBClusterSnapshotAttributesResponse,

    -- * Response Lenses
    describeDBClusterSnapshotAttributesResponse_dbClusterSnapshotAttributesResult,
    describeDBClusterSnapshotAttributesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newDescribeDBClusterSnapshotAttributes' smart constructor.
data DescribeDBClusterSnapshotAttributes = DescribeDBClusterSnapshotAttributes'
  { -- | The identifier for the DB cluster snapshot to describe the attributes
    -- for.
    dbClusterSnapshotIdentifier :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeDBClusterSnapshotAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbClusterSnapshotIdentifier', 'describeDBClusterSnapshotAttributes_dbClusterSnapshotIdentifier' - The identifier for the DB cluster snapshot to describe the attributes
-- for.
newDescribeDBClusterSnapshotAttributes ::
  -- | 'dbClusterSnapshotIdentifier'
  Core.Text ->
  DescribeDBClusterSnapshotAttributes
newDescribeDBClusterSnapshotAttributes
  pDBClusterSnapshotIdentifier_ =
    DescribeDBClusterSnapshotAttributes'
      { dbClusterSnapshotIdentifier =
          pDBClusterSnapshotIdentifier_
      }

-- | The identifier for the DB cluster snapshot to describe the attributes
-- for.
describeDBClusterSnapshotAttributes_dbClusterSnapshotIdentifier :: Lens.Lens' DescribeDBClusterSnapshotAttributes Core.Text
describeDBClusterSnapshotAttributes_dbClusterSnapshotIdentifier = Lens.lens (\DescribeDBClusterSnapshotAttributes' {dbClusterSnapshotIdentifier} -> dbClusterSnapshotIdentifier) (\s@DescribeDBClusterSnapshotAttributes' {} a -> s {dbClusterSnapshotIdentifier = a} :: DescribeDBClusterSnapshotAttributes)

instance
  Core.AWSRequest
    DescribeDBClusterSnapshotAttributes
  where
  type
    AWSResponse DescribeDBClusterSnapshotAttributes =
      DescribeDBClusterSnapshotAttributesResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeDBClusterSnapshotAttributesResult"
      ( \s h x ->
          DescribeDBClusterSnapshotAttributesResponse'
            Core.<$> (x Core..@? "DBClusterSnapshotAttributesResult")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    DescribeDBClusterSnapshotAttributes

instance
  Core.NFData
    DescribeDBClusterSnapshotAttributes

instance
  Core.ToHeaders
    DescribeDBClusterSnapshotAttributes
  where
  toHeaders = Core.const Core.mempty

instance
  Core.ToPath
    DescribeDBClusterSnapshotAttributes
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    DescribeDBClusterSnapshotAttributes
  where
  toQuery DescribeDBClusterSnapshotAttributes' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ( "DescribeDBClusterSnapshotAttributes" ::
                      Core.ByteString
                  ),
        "Version" Core.=: ("2014-10-31" :: Core.ByteString),
        "DBClusterSnapshotIdentifier"
          Core.=: dbClusterSnapshotIdentifier
      ]

-- | /See:/ 'newDescribeDBClusterSnapshotAttributesResponse' smart constructor.
data DescribeDBClusterSnapshotAttributesResponse = DescribeDBClusterSnapshotAttributesResponse'
  { dbClusterSnapshotAttributesResult :: Core.Maybe DBClusterSnapshotAttributesResult,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeDBClusterSnapshotAttributesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbClusterSnapshotAttributesResult', 'describeDBClusterSnapshotAttributesResponse_dbClusterSnapshotAttributesResult' - Undocumented member.
--
-- 'httpStatus', 'describeDBClusterSnapshotAttributesResponse_httpStatus' - The response's http status code.
newDescribeDBClusterSnapshotAttributesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeDBClusterSnapshotAttributesResponse
newDescribeDBClusterSnapshotAttributesResponse
  pHttpStatus_ =
    DescribeDBClusterSnapshotAttributesResponse'
      { dbClusterSnapshotAttributesResult =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Undocumented member.
describeDBClusterSnapshotAttributesResponse_dbClusterSnapshotAttributesResult :: Lens.Lens' DescribeDBClusterSnapshotAttributesResponse (Core.Maybe DBClusterSnapshotAttributesResult)
describeDBClusterSnapshotAttributesResponse_dbClusterSnapshotAttributesResult = Lens.lens (\DescribeDBClusterSnapshotAttributesResponse' {dbClusterSnapshotAttributesResult} -> dbClusterSnapshotAttributesResult) (\s@DescribeDBClusterSnapshotAttributesResponse' {} a -> s {dbClusterSnapshotAttributesResult = a} :: DescribeDBClusterSnapshotAttributesResponse)

-- | The response's http status code.
describeDBClusterSnapshotAttributesResponse_httpStatus :: Lens.Lens' DescribeDBClusterSnapshotAttributesResponse Core.Int
describeDBClusterSnapshotAttributesResponse_httpStatus = Lens.lens (\DescribeDBClusterSnapshotAttributesResponse' {httpStatus} -> httpStatus) (\s@DescribeDBClusterSnapshotAttributesResponse' {} a -> s {httpStatus = a} :: DescribeDBClusterSnapshotAttributesResponse)

instance
  Core.NFData
    DescribeDBClusterSnapshotAttributesResponse
