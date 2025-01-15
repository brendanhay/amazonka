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
-- Module      : Amazonka.RDS.DescribeDBSnapshotAttributes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of DB snapshot attribute names and values for a manual DB
-- snapshot.
--
-- When sharing snapshots with other Amazon Web Services accounts,
-- @DescribeDBSnapshotAttributes@ returns the @restore@ attribute and a
-- list of IDs for the Amazon Web Services accounts that are authorized to
-- copy or restore the manual DB snapshot. If @all@ is included in the list
-- of values for the @restore@ attribute, then the manual DB snapshot is
-- public and can be copied or restored by all Amazon Web Services
-- accounts.
--
-- To add or remove access for an Amazon Web Services account to copy or
-- restore a manual DB snapshot, or to make the manual DB snapshot public
-- or private, use the @ModifyDBSnapshotAttribute@ API action.
module Amazonka.RDS.DescribeDBSnapshotAttributes
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RDS.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- |
--
-- /See:/ 'newDescribeDBSnapshotAttributes' smart constructor.
data DescribeDBSnapshotAttributes = DescribeDBSnapshotAttributes'
  { -- | The identifier for the DB snapshot to describe the attributes for.
    dbSnapshotIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  DescribeDBSnapshotAttributes
newDescribeDBSnapshotAttributes
  pDBSnapshotIdentifier_ =
    DescribeDBSnapshotAttributes'
      { dbSnapshotIdentifier =
          pDBSnapshotIdentifier_
      }

-- | The identifier for the DB snapshot to describe the attributes for.
describeDBSnapshotAttributes_dbSnapshotIdentifier :: Lens.Lens' DescribeDBSnapshotAttributes Prelude.Text
describeDBSnapshotAttributes_dbSnapshotIdentifier = Lens.lens (\DescribeDBSnapshotAttributes' {dbSnapshotIdentifier} -> dbSnapshotIdentifier) (\s@DescribeDBSnapshotAttributes' {} a -> s {dbSnapshotIdentifier = a} :: DescribeDBSnapshotAttributes)

instance Core.AWSRequest DescribeDBSnapshotAttributes where
  type
    AWSResponse DescribeDBSnapshotAttributes =
      DescribeDBSnapshotAttributesResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DescribeDBSnapshotAttributesResult"
      ( \s h x ->
          DescribeDBSnapshotAttributesResponse'
            Prelude.<$> (x Data..@? "DBSnapshotAttributesResult")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeDBSnapshotAttributes
  where
  hashWithSalt _salt DescribeDBSnapshotAttributes' {..} =
    _salt `Prelude.hashWithSalt` dbSnapshotIdentifier

instance Prelude.NFData DescribeDBSnapshotAttributes where
  rnf DescribeDBSnapshotAttributes' {..} =
    Prelude.rnf dbSnapshotIdentifier

instance Data.ToHeaders DescribeDBSnapshotAttributes where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeDBSnapshotAttributes where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeDBSnapshotAttributes where
  toQuery DescribeDBSnapshotAttributes' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "DescribeDBSnapshotAttributes" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2014-10-31" :: Prelude.ByteString),
        "DBSnapshotIdentifier" Data.=: dbSnapshotIdentifier
      ]

-- | /See:/ 'newDescribeDBSnapshotAttributesResponse' smart constructor.
data DescribeDBSnapshotAttributesResponse = DescribeDBSnapshotAttributesResponse'
  { dbSnapshotAttributesResult :: Prelude.Maybe DBSnapshotAttributesResult,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DescribeDBSnapshotAttributesResponse
newDescribeDBSnapshotAttributesResponse pHttpStatus_ =
  DescribeDBSnapshotAttributesResponse'
    { dbSnapshotAttributesResult =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
describeDBSnapshotAttributesResponse_dbSnapshotAttributesResult :: Lens.Lens' DescribeDBSnapshotAttributesResponse (Prelude.Maybe DBSnapshotAttributesResult)
describeDBSnapshotAttributesResponse_dbSnapshotAttributesResult = Lens.lens (\DescribeDBSnapshotAttributesResponse' {dbSnapshotAttributesResult} -> dbSnapshotAttributesResult) (\s@DescribeDBSnapshotAttributesResponse' {} a -> s {dbSnapshotAttributesResult = a} :: DescribeDBSnapshotAttributesResponse)

-- | The response's http status code.
describeDBSnapshotAttributesResponse_httpStatus :: Lens.Lens' DescribeDBSnapshotAttributesResponse Prelude.Int
describeDBSnapshotAttributesResponse_httpStatus = Lens.lens (\DescribeDBSnapshotAttributesResponse' {httpStatus} -> httpStatus) (\s@DescribeDBSnapshotAttributesResponse' {} a -> s {httpStatus = a} :: DescribeDBSnapshotAttributesResponse)

instance
  Prelude.NFData
    DescribeDBSnapshotAttributesResponse
  where
  rnf DescribeDBSnapshotAttributesResponse' {..} =
    Prelude.rnf dbSnapshotAttributesResult `Prelude.seq`
      Prelude.rnf httpStatus
