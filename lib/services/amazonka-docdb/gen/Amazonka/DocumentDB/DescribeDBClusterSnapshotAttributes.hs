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
-- Module      : Amazonka.DocumentDB.DescribeDBClusterSnapshotAttributes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of cluster snapshot attribute names and values for a
-- manual DB cluster snapshot.
--
-- When you share snapshots with other Amazon Web Services accounts,
-- @DescribeDBClusterSnapshotAttributes@ returns the @restore@ attribute
-- and a list of IDs for the Amazon Web Services accounts that are
-- authorized to copy or restore the manual cluster snapshot. If @all@ is
-- included in the list of values for the @restore@ attribute, then the
-- manual cluster snapshot is public and can be copied or restored by all
-- Amazon Web Services accounts.
module Amazonka.DocumentDB.DescribeDBClusterSnapshotAttributes
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DocumentDB.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the input to DescribeDBClusterSnapshotAttributes.
--
-- /See:/ 'newDescribeDBClusterSnapshotAttributes' smart constructor.
data DescribeDBClusterSnapshotAttributes = DescribeDBClusterSnapshotAttributes'
  { -- | The identifier for the cluster snapshot to describe the attributes for.
    dbClusterSnapshotIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDBClusterSnapshotAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbClusterSnapshotIdentifier', 'describeDBClusterSnapshotAttributes_dbClusterSnapshotIdentifier' - The identifier for the cluster snapshot to describe the attributes for.
newDescribeDBClusterSnapshotAttributes ::
  -- | 'dbClusterSnapshotIdentifier'
  Prelude.Text ->
  DescribeDBClusterSnapshotAttributes
newDescribeDBClusterSnapshotAttributes
  pDBClusterSnapshotIdentifier_ =
    DescribeDBClusterSnapshotAttributes'
      { dbClusterSnapshotIdentifier =
          pDBClusterSnapshotIdentifier_
      }

-- | The identifier for the cluster snapshot to describe the attributes for.
describeDBClusterSnapshotAttributes_dbClusterSnapshotIdentifier :: Lens.Lens' DescribeDBClusterSnapshotAttributes Prelude.Text
describeDBClusterSnapshotAttributes_dbClusterSnapshotIdentifier = Lens.lens (\DescribeDBClusterSnapshotAttributes' {dbClusterSnapshotIdentifier} -> dbClusterSnapshotIdentifier) (\s@DescribeDBClusterSnapshotAttributes' {} a -> s {dbClusterSnapshotIdentifier = a} :: DescribeDBClusterSnapshotAttributes)

instance
  Core.AWSRequest
    DescribeDBClusterSnapshotAttributes
  where
  type
    AWSResponse DescribeDBClusterSnapshotAttributes =
      DescribeDBClusterSnapshotAttributesResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DescribeDBClusterSnapshotAttributesResult"
      ( \s h x ->
          DescribeDBClusterSnapshotAttributesResponse'
            Prelude.<$> (x Data..@? "DBClusterSnapshotAttributesResult")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeDBClusterSnapshotAttributes
  where
  hashWithSalt
    _salt
    DescribeDBClusterSnapshotAttributes' {..} =
      _salt
        `Prelude.hashWithSalt` dbClusterSnapshotIdentifier

instance
  Prelude.NFData
    DescribeDBClusterSnapshotAttributes
  where
  rnf DescribeDBClusterSnapshotAttributes' {..} =
    Prelude.rnf dbClusterSnapshotIdentifier

instance
  Data.ToHeaders
    DescribeDBClusterSnapshotAttributes
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    DescribeDBClusterSnapshotAttributes
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DescribeDBClusterSnapshotAttributes
  where
  toQuery DescribeDBClusterSnapshotAttributes' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "DescribeDBClusterSnapshotAttributes" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2014-10-31" :: Prelude.ByteString),
        "DBClusterSnapshotIdentifier"
          Data.=: dbClusterSnapshotIdentifier
      ]

-- | /See:/ 'newDescribeDBClusterSnapshotAttributesResponse' smart constructor.
data DescribeDBClusterSnapshotAttributesResponse = DescribeDBClusterSnapshotAttributesResponse'
  { dbClusterSnapshotAttributesResult :: Prelude.Maybe DBClusterSnapshotAttributesResult,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DescribeDBClusterSnapshotAttributesResponse
newDescribeDBClusterSnapshotAttributesResponse
  pHttpStatus_ =
    DescribeDBClusterSnapshotAttributesResponse'
      { dbClusterSnapshotAttributesResult =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Undocumented member.
describeDBClusterSnapshotAttributesResponse_dbClusterSnapshotAttributesResult :: Lens.Lens' DescribeDBClusterSnapshotAttributesResponse (Prelude.Maybe DBClusterSnapshotAttributesResult)
describeDBClusterSnapshotAttributesResponse_dbClusterSnapshotAttributesResult = Lens.lens (\DescribeDBClusterSnapshotAttributesResponse' {dbClusterSnapshotAttributesResult} -> dbClusterSnapshotAttributesResult) (\s@DescribeDBClusterSnapshotAttributesResponse' {} a -> s {dbClusterSnapshotAttributesResult = a} :: DescribeDBClusterSnapshotAttributesResponse)

-- | The response's http status code.
describeDBClusterSnapshotAttributesResponse_httpStatus :: Lens.Lens' DescribeDBClusterSnapshotAttributesResponse Prelude.Int
describeDBClusterSnapshotAttributesResponse_httpStatus = Lens.lens (\DescribeDBClusterSnapshotAttributesResponse' {httpStatus} -> httpStatus) (\s@DescribeDBClusterSnapshotAttributesResponse' {} a -> s {httpStatus = a} :: DescribeDBClusterSnapshotAttributesResponse)

instance
  Prelude.NFData
    DescribeDBClusterSnapshotAttributesResponse
  where
  rnf DescribeDBClusterSnapshotAttributesResponse' {..} =
    Prelude.rnf dbClusterSnapshotAttributesResult `Prelude.seq`
      Prelude.rnf httpStatus
