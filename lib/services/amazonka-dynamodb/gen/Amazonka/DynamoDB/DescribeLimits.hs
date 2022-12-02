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
-- Module      : Amazonka.DynamoDB.DescribeLimits
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the current provisioned-capacity quotas for your Amazon Web
-- Services account in a Region, both for the Region as a whole and for any
-- one DynamoDB table that you create there.
--
-- When you establish an Amazon Web Services account, the account has
-- initial quotas on the maximum read capacity units and write capacity
-- units that you can provision across all of your DynamoDB tables in a
-- given Region. Also, there are per-table quotas that apply when you
-- create a table there. For more information, see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Limits.html Service, Account, and Table Quotas>
-- page in the /Amazon DynamoDB Developer Guide/.
--
-- Although you can increase these quotas by filing a case at
-- <https://console.aws.amazon.com/support/home#/ Amazon Web Services Support Center>,
-- obtaining the increase is not instantaneous. The @DescribeLimits@ action
-- lets you write code to compare the capacity you are currently using to
-- those quotas imposed by your account so that you have enough time to
-- apply for an increase before you hit a quota.
--
-- For example, you could use one of the Amazon Web Services SDKs to do the
-- following:
--
-- 1.  Call @DescribeLimits@ for a particular Region to obtain your current
--     account quotas on provisioned capacity there.
--
-- 2.  Create a variable to hold the aggregate read capacity units
--     provisioned for all your tables in that Region, and one to hold the
--     aggregate write capacity units. Zero them both.
--
-- 3.  Call @ListTables@ to obtain a list of all your DynamoDB tables.
--
-- 4.  For each table name listed by @ListTables@, do the following:
--
--     -   Call @DescribeTable@ with the table name.
--
--     -   Use the data returned by @DescribeTable@ to add the read
--         capacity units and write capacity units provisioned for the
--         table itself to your variables.
--
--     -   If the table has one or more global secondary indexes (GSIs),
--         loop over these GSIs and add their provisioned capacity values
--         to your variables as well.
--
-- 5.  Report the account quotas for that Region returned by
--     @DescribeLimits@, along with the total current provisioned capacity
--     levels you have calculated.
--
-- This will let you see whether you are getting close to your
-- account-level quotas.
--
-- The per-table quotas apply only when you are creating a new table. They
-- restrict the sum of the provisioned capacity of the new table itself and
-- all its global secondary indexes.
--
-- For existing tables and their GSIs, DynamoDB doesn\'t let you increase
-- provisioned capacity extremely rapidly, but the only quota that applies
-- is that the aggregate provisioned capacity over all your tables and GSIs
-- cannot exceed either of the per-account quotas.
--
-- @DescribeLimits@ should only be called periodically. You can expect
-- throttling errors if you call it more than once in a minute.
--
-- The @DescribeLimits@ Request element has no content.
module Amazonka.DynamoDB.DescribeLimits
  ( -- * Creating a Request
    DescribeLimits (..),
    newDescribeLimits,

    -- * Destructuring the Response
    DescribeLimitsResponse (..),
    newDescribeLimitsResponse,

    -- * Response Lenses
    describeLimitsResponse_tableMaxReadCapacityUnits,
    describeLimitsResponse_accountMaxWriteCapacityUnits,
    describeLimitsResponse_tableMaxWriteCapacityUnits,
    describeLimitsResponse_accountMaxReadCapacityUnits,
    describeLimitsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DynamoDB.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the input of a @DescribeLimits@ operation. Has no content.
--
-- /See:/ 'newDescribeLimits' smart constructor.
data DescribeLimits = DescribeLimits'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeLimits' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDescribeLimits ::
  DescribeLimits
newDescribeLimits = DescribeLimits'

instance Core.AWSRequest DescribeLimits where
  type
    AWSResponse DescribeLimits =
      DescribeLimitsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeLimitsResponse'
            Prelude.<$> (x Data..?> "TableMaxReadCapacityUnits")
            Prelude.<*> (x Data..?> "AccountMaxWriteCapacityUnits")
            Prelude.<*> (x Data..?> "TableMaxWriteCapacityUnits")
            Prelude.<*> (x Data..?> "AccountMaxReadCapacityUnits")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeLimits where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance Prelude.NFData DescribeLimits where
  rnf _ = ()

instance Data.ToHeaders DescribeLimits where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "DynamoDB_20120810.DescribeLimits" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeLimits where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath DescribeLimits where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeLimits where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the output of a @DescribeLimits@ operation.
--
-- /See:/ 'newDescribeLimitsResponse' smart constructor.
data DescribeLimitsResponse = DescribeLimitsResponse'
  { -- | The maximum read capacity units that your account allows you to
    -- provision for a new table that you are creating in this Region,
    -- including the read capacity units provisioned for its global secondary
    -- indexes (GSIs).
    tableMaxReadCapacityUnits :: Prelude.Maybe Prelude.Natural,
    -- | The maximum total write capacity units that your account allows you to
    -- provision across all of your tables in this Region.
    accountMaxWriteCapacityUnits :: Prelude.Maybe Prelude.Natural,
    -- | The maximum write capacity units that your account allows you to
    -- provision for a new table that you are creating in this Region,
    -- including the write capacity units provisioned for its global secondary
    -- indexes (GSIs).
    tableMaxWriteCapacityUnits :: Prelude.Maybe Prelude.Natural,
    -- | The maximum total read capacity units that your account allows you to
    -- provision across all of your tables in this Region.
    accountMaxReadCapacityUnits :: Prelude.Maybe Prelude.Natural,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeLimitsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tableMaxReadCapacityUnits', 'describeLimitsResponse_tableMaxReadCapacityUnits' - The maximum read capacity units that your account allows you to
-- provision for a new table that you are creating in this Region,
-- including the read capacity units provisioned for its global secondary
-- indexes (GSIs).
--
-- 'accountMaxWriteCapacityUnits', 'describeLimitsResponse_accountMaxWriteCapacityUnits' - The maximum total write capacity units that your account allows you to
-- provision across all of your tables in this Region.
--
-- 'tableMaxWriteCapacityUnits', 'describeLimitsResponse_tableMaxWriteCapacityUnits' - The maximum write capacity units that your account allows you to
-- provision for a new table that you are creating in this Region,
-- including the write capacity units provisioned for its global secondary
-- indexes (GSIs).
--
-- 'accountMaxReadCapacityUnits', 'describeLimitsResponse_accountMaxReadCapacityUnits' - The maximum total read capacity units that your account allows you to
-- provision across all of your tables in this Region.
--
-- 'httpStatus', 'describeLimitsResponse_httpStatus' - The response's http status code.
newDescribeLimitsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeLimitsResponse
newDescribeLimitsResponse pHttpStatus_ =
  DescribeLimitsResponse'
    { tableMaxReadCapacityUnits =
        Prelude.Nothing,
      accountMaxWriteCapacityUnits = Prelude.Nothing,
      tableMaxWriteCapacityUnits = Prelude.Nothing,
      accountMaxReadCapacityUnits = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The maximum read capacity units that your account allows you to
-- provision for a new table that you are creating in this Region,
-- including the read capacity units provisioned for its global secondary
-- indexes (GSIs).
describeLimitsResponse_tableMaxReadCapacityUnits :: Lens.Lens' DescribeLimitsResponse (Prelude.Maybe Prelude.Natural)
describeLimitsResponse_tableMaxReadCapacityUnits = Lens.lens (\DescribeLimitsResponse' {tableMaxReadCapacityUnits} -> tableMaxReadCapacityUnits) (\s@DescribeLimitsResponse' {} a -> s {tableMaxReadCapacityUnits = a} :: DescribeLimitsResponse)

-- | The maximum total write capacity units that your account allows you to
-- provision across all of your tables in this Region.
describeLimitsResponse_accountMaxWriteCapacityUnits :: Lens.Lens' DescribeLimitsResponse (Prelude.Maybe Prelude.Natural)
describeLimitsResponse_accountMaxWriteCapacityUnits = Lens.lens (\DescribeLimitsResponse' {accountMaxWriteCapacityUnits} -> accountMaxWriteCapacityUnits) (\s@DescribeLimitsResponse' {} a -> s {accountMaxWriteCapacityUnits = a} :: DescribeLimitsResponse)

-- | The maximum write capacity units that your account allows you to
-- provision for a new table that you are creating in this Region,
-- including the write capacity units provisioned for its global secondary
-- indexes (GSIs).
describeLimitsResponse_tableMaxWriteCapacityUnits :: Lens.Lens' DescribeLimitsResponse (Prelude.Maybe Prelude.Natural)
describeLimitsResponse_tableMaxWriteCapacityUnits = Lens.lens (\DescribeLimitsResponse' {tableMaxWriteCapacityUnits} -> tableMaxWriteCapacityUnits) (\s@DescribeLimitsResponse' {} a -> s {tableMaxWriteCapacityUnits = a} :: DescribeLimitsResponse)

-- | The maximum total read capacity units that your account allows you to
-- provision across all of your tables in this Region.
describeLimitsResponse_accountMaxReadCapacityUnits :: Lens.Lens' DescribeLimitsResponse (Prelude.Maybe Prelude.Natural)
describeLimitsResponse_accountMaxReadCapacityUnits = Lens.lens (\DescribeLimitsResponse' {accountMaxReadCapacityUnits} -> accountMaxReadCapacityUnits) (\s@DescribeLimitsResponse' {} a -> s {accountMaxReadCapacityUnits = a} :: DescribeLimitsResponse)

-- | The response's http status code.
describeLimitsResponse_httpStatus :: Lens.Lens' DescribeLimitsResponse Prelude.Int
describeLimitsResponse_httpStatus = Lens.lens (\DescribeLimitsResponse' {httpStatus} -> httpStatus) (\s@DescribeLimitsResponse' {} a -> s {httpStatus = a} :: DescribeLimitsResponse)

instance Prelude.NFData DescribeLimitsResponse where
  rnf DescribeLimitsResponse' {..} =
    Prelude.rnf tableMaxReadCapacityUnits
      `Prelude.seq` Prelude.rnf accountMaxWriteCapacityUnits
      `Prelude.seq` Prelude.rnf tableMaxWriteCapacityUnits
      `Prelude.seq` Prelude.rnf accountMaxReadCapacityUnits
      `Prelude.seq` Prelude.rnf httpStatus
