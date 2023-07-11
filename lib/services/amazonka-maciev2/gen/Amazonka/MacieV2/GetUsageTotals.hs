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
-- Module      : Amazonka.MacieV2.GetUsageTotals
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves (queries) aggregated usage data for an account.
module Amazonka.MacieV2.GetUsageTotals
  ( -- * Creating a Request
    GetUsageTotals (..),
    newGetUsageTotals,

    -- * Request Lenses
    getUsageTotals_timeRange,

    -- * Destructuring the Response
    GetUsageTotalsResponse (..),
    newGetUsageTotalsResponse,

    -- * Response Lenses
    getUsageTotalsResponse_timeRange,
    getUsageTotalsResponse_usageTotals,
    getUsageTotalsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MacieV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetUsageTotals' smart constructor.
data GetUsageTotals = GetUsageTotals'
  { -- | The inclusive time period to retrieve the data for. Valid values are:
    -- MONTH_TO_DATE, for the current calendar month to date; and,
    -- PAST_30_DAYS, for the preceding 30 days. If you don\'t specify a value
    -- for this parameter, Amazon Macie provides aggregated usage data for the
    -- preceding 30 days.
    timeRange :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetUsageTotals' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'timeRange', 'getUsageTotals_timeRange' - The inclusive time period to retrieve the data for. Valid values are:
-- MONTH_TO_DATE, for the current calendar month to date; and,
-- PAST_30_DAYS, for the preceding 30 days. If you don\'t specify a value
-- for this parameter, Amazon Macie provides aggregated usage data for the
-- preceding 30 days.
newGetUsageTotals ::
  GetUsageTotals
newGetUsageTotals =
  GetUsageTotals' {timeRange = Prelude.Nothing}

-- | The inclusive time period to retrieve the data for. Valid values are:
-- MONTH_TO_DATE, for the current calendar month to date; and,
-- PAST_30_DAYS, for the preceding 30 days. If you don\'t specify a value
-- for this parameter, Amazon Macie provides aggregated usage data for the
-- preceding 30 days.
getUsageTotals_timeRange :: Lens.Lens' GetUsageTotals (Prelude.Maybe Prelude.Text)
getUsageTotals_timeRange = Lens.lens (\GetUsageTotals' {timeRange} -> timeRange) (\s@GetUsageTotals' {} a -> s {timeRange = a} :: GetUsageTotals)

instance Core.AWSRequest GetUsageTotals where
  type
    AWSResponse GetUsageTotals =
      GetUsageTotalsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetUsageTotalsResponse'
            Prelude.<$> (x Data..?> "timeRange")
            Prelude.<*> (x Data..?> "usageTotals" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetUsageTotals where
  hashWithSalt _salt GetUsageTotals' {..} =
    _salt `Prelude.hashWithSalt` timeRange

instance Prelude.NFData GetUsageTotals where
  rnf GetUsageTotals' {..} = Prelude.rnf timeRange

instance Data.ToHeaders GetUsageTotals where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetUsageTotals where
  toPath = Prelude.const "/usage"

instance Data.ToQuery GetUsageTotals where
  toQuery GetUsageTotals' {..} =
    Prelude.mconcat ["timeRange" Data.=: timeRange]

-- | /See:/ 'newGetUsageTotalsResponse' smart constructor.
data GetUsageTotalsResponse = GetUsageTotalsResponse'
  { -- | The inclusive time period that the usage data applies to. Possible
    -- values are: MONTH_TO_DATE, for the current calendar month to date; and,
    -- PAST_30_DAYS, for the preceding 30 days.
    timeRange :: Prelude.Maybe TimeRange,
    -- | An array of objects that contains the results of the query. Each object
    -- contains the data for a specific usage metric.
    usageTotals :: Prelude.Maybe [UsageTotal],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetUsageTotalsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'timeRange', 'getUsageTotalsResponse_timeRange' - The inclusive time period that the usage data applies to. Possible
-- values are: MONTH_TO_DATE, for the current calendar month to date; and,
-- PAST_30_DAYS, for the preceding 30 days.
--
-- 'usageTotals', 'getUsageTotalsResponse_usageTotals' - An array of objects that contains the results of the query. Each object
-- contains the data for a specific usage metric.
--
-- 'httpStatus', 'getUsageTotalsResponse_httpStatus' - The response's http status code.
newGetUsageTotalsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetUsageTotalsResponse
newGetUsageTotalsResponse pHttpStatus_ =
  GetUsageTotalsResponse'
    { timeRange =
        Prelude.Nothing,
      usageTotals = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The inclusive time period that the usage data applies to. Possible
-- values are: MONTH_TO_DATE, for the current calendar month to date; and,
-- PAST_30_DAYS, for the preceding 30 days.
getUsageTotalsResponse_timeRange :: Lens.Lens' GetUsageTotalsResponse (Prelude.Maybe TimeRange)
getUsageTotalsResponse_timeRange = Lens.lens (\GetUsageTotalsResponse' {timeRange} -> timeRange) (\s@GetUsageTotalsResponse' {} a -> s {timeRange = a} :: GetUsageTotalsResponse)

-- | An array of objects that contains the results of the query. Each object
-- contains the data for a specific usage metric.
getUsageTotalsResponse_usageTotals :: Lens.Lens' GetUsageTotalsResponse (Prelude.Maybe [UsageTotal])
getUsageTotalsResponse_usageTotals = Lens.lens (\GetUsageTotalsResponse' {usageTotals} -> usageTotals) (\s@GetUsageTotalsResponse' {} a -> s {usageTotals = a} :: GetUsageTotalsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getUsageTotalsResponse_httpStatus :: Lens.Lens' GetUsageTotalsResponse Prelude.Int
getUsageTotalsResponse_httpStatus = Lens.lens (\GetUsageTotalsResponse' {httpStatus} -> httpStatus) (\s@GetUsageTotalsResponse' {} a -> s {httpStatus = a} :: GetUsageTotalsResponse)

instance Prelude.NFData GetUsageTotalsResponse where
  rnf GetUsageTotalsResponse' {..} =
    Prelude.rnf timeRange
      `Prelude.seq` Prelude.rnf usageTotals
      `Prelude.seq` Prelude.rnf httpStatus
