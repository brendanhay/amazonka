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
-- Module      : Network.AWS.RDS.DescribeDBSnapshotAttributes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of DB snapshot attribute names and values for a manual DB
-- snapshot.
--
-- When sharing snapshots with other AWS accounts,
-- @DescribeDBSnapshotAttributes@ returns the @restore@ attribute and a
-- list of IDs for the AWS accounts that are authorized to copy or restore
-- the manual DB snapshot. If @all@ is included in the list of values for
-- the @restore@ attribute, then the manual DB snapshot is public and can
-- be copied or restored by all AWS accounts.
--
-- To add or remove access for an AWS account to copy or restore a manual
-- DB snapshot, or to make the manual DB snapshot public or private, use
-- the @ModifyDBSnapshotAttribute@ API action.
module Network.AWS.RDS.DescribeDBSnapshotAttributes
  ( -- * Creating a Request
    DescribeDBSnapshotAttributes (..),
    newDescribeDBSnapshotAttributes,

    -- * Request Lenses
    describeDBSnapshotAttributes_dbSnapshotIdentifier,

    -- * Destructuring the Response
    DescribeDBSnapshotAttributesResponse (..),
    newDescribeDBSnapshotAttributesResponse,

    -- * Response Lenses
    describeDBSnapshotAttributesResponse_dbSnapshotAttributesResult,
    describeDBSnapshotAttributesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newDescribeDBSnapshotAttributes' smart constructor.
data DescribeDBSnapshotAttributes = DescribeDBSnapshotAttributes'
  { -- | The identifier for the DB snapshot to describe the attributes for.
    dbSnapshotIdentifier :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeDBSnapshotAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbSnapshotIdentifier', 'describeDBSnapshotAttributes_dbSnapshotIdentifier' - The identifier for the DB snapshot to describe the attributes for.
newDescribeDBSnapshotAttributes ::
  -- | 'dbSnapshotIdentifier'
  Core.Text ->
  DescribeDBSnapshotAttributes
newDescribeDBSnapshotAttributes
  pDBSnapshotIdentifier_ =
    DescribeDBSnapshotAttributes'
      { dbSnapshotIdentifier =
          pDBSnapshotIdentifier_
      }

-- | The identifier for the DB snapshot to describe the attributes for.
describeDBSnapshotAttributes_dbSnapshotIdentifier :: Lens.Lens' DescribeDBSnapshotAttributes Core.Text
describeDBSnapshotAttributes_dbSnapshotIdentifier = Lens.lens (\DescribeDBSnapshotAttributes' {dbSnapshotIdentifier} -> dbSnapshotIdentifier) (\s@DescribeDBSnapshotAttributes' {} a -> s {dbSnapshotIdentifier = a} :: DescribeDBSnapshotAttributes)

instance Core.AWSRequest DescribeDBSnapshotAttributes where
  type
    AWSResponse DescribeDBSnapshotAttributes =
      DescribeDBSnapshotAttributesResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeDBSnapshotAttributesResult"
      ( \s h x ->
          DescribeDBSnapshotAttributesResponse'
            Core.<$> (x Core..@? "DBSnapshotAttributesResult")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeDBSnapshotAttributes

instance Core.NFData DescribeDBSnapshotAttributes

instance Core.ToHeaders DescribeDBSnapshotAttributes where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeDBSnapshotAttributes where
  toPath = Core.const "/"

instance Core.ToQuery DescribeDBSnapshotAttributes where
  toQuery DescribeDBSnapshotAttributes' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DescribeDBSnapshotAttributes" :: Core.ByteString),
        "Version" Core.=: ("2014-10-31" :: Core.ByteString),
        "DBSnapshotIdentifier" Core.=: dbSnapshotIdentifier
      ]

-- | /See:/ 'newDescribeDBSnapshotAttributesResponse' smart constructor.
data DescribeDBSnapshotAttributesResponse = DescribeDBSnapshotAttributesResponse'
  { dbSnapshotAttributesResult :: Core.Maybe DBSnapshotAttributesResult,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeDBSnapshotAttributesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbSnapshotAttributesResult', 'describeDBSnapshotAttributesResponse_dbSnapshotAttributesResult' - Undocumented member.
--
-- 'httpStatus', 'describeDBSnapshotAttributesResponse_httpStatus' - The response's http status code.
newDescribeDBSnapshotAttributesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeDBSnapshotAttributesResponse
newDescribeDBSnapshotAttributesResponse pHttpStatus_ =
  DescribeDBSnapshotAttributesResponse'
    { dbSnapshotAttributesResult =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
describeDBSnapshotAttributesResponse_dbSnapshotAttributesResult :: Lens.Lens' DescribeDBSnapshotAttributesResponse (Core.Maybe DBSnapshotAttributesResult)
describeDBSnapshotAttributesResponse_dbSnapshotAttributesResult = Lens.lens (\DescribeDBSnapshotAttributesResponse' {dbSnapshotAttributesResult} -> dbSnapshotAttributesResult) (\s@DescribeDBSnapshotAttributesResponse' {} a -> s {dbSnapshotAttributesResult = a} :: DescribeDBSnapshotAttributesResponse)

-- | The response's http status code.
describeDBSnapshotAttributesResponse_httpStatus :: Lens.Lens' DescribeDBSnapshotAttributesResponse Core.Int
describeDBSnapshotAttributesResponse_httpStatus = Lens.lens (\DescribeDBSnapshotAttributesResponse' {httpStatus} -> httpStatus) (\s@DescribeDBSnapshotAttributesResponse' {} a -> s {httpStatus = a} :: DescribeDBSnapshotAttributesResponse)

instance
  Core.NFData
    DescribeDBSnapshotAttributesResponse
