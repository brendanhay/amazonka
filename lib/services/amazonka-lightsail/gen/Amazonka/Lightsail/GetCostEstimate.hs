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
-- Module      : Amazonka.Lightsail.GetCostEstimate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the cost estimate for a specified resource.
-- A cost estimate will not generate for a resource that has been deleted.
module Amazonka.Lightsail.GetCostEstimate
  ( -- * Creating a Request
    GetCostEstimate (..),
    newGetCostEstimate,

    -- * Request Lenses
    getCostEstimate_resourceName,
    getCostEstimate_startTime,
    getCostEstimate_endTime,

    -- * Destructuring the Response
    GetCostEstimateResponse (..),
    newGetCostEstimateResponse,

    -- * Response Lenses
    getCostEstimateResponse_resourcesBudgetEstimate,
    getCostEstimateResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lightsail.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetCostEstimate' smart constructor.
data GetCostEstimate = GetCostEstimate'
  { -- | The resource name.
    resourceName :: Prelude.Text,
    -- | The cost estimate start time.
    --
    -- Constraints:
    --
    -- -   Specified in Coordinated Universal Time (UTC).
    --
    -- -   Specified in the Unix time format.
    --
    --     For example, if you want to use a start time of October 1, 2018, at
    --     8 PM UTC, specify @1538424000@ as the start time.
    --
    -- You can convert a human-friendly time to Unix time format using a
    -- converter like <https://www.epochconverter.com/ Epoch converter>.
    startTime :: Data.POSIX,
    -- | The cost estimate end time.
    --
    -- Constraints:
    --
    -- -   Specified in Coordinated Universal Time (UTC).
    --
    -- -   Specified in the Unix time format.
    --
    --     For example, if you want to use an end time of October 1, 2018, at 9
    --     PM UTC, specify @1538427600@ as the end time.
    --
    -- You can convert a human-friendly time to Unix time format using a
    -- converter like <https://www.epochconverter.com/ Epoch converter>.
    endTime :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetCostEstimate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceName', 'getCostEstimate_resourceName' - The resource name.
--
-- 'startTime', 'getCostEstimate_startTime' - The cost estimate start time.
--
-- Constraints:
--
-- -   Specified in Coordinated Universal Time (UTC).
--
-- -   Specified in the Unix time format.
--
--     For example, if you want to use a start time of October 1, 2018, at
--     8 PM UTC, specify @1538424000@ as the start time.
--
-- You can convert a human-friendly time to Unix time format using a
-- converter like <https://www.epochconverter.com/ Epoch converter>.
--
-- 'endTime', 'getCostEstimate_endTime' - The cost estimate end time.
--
-- Constraints:
--
-- -   Specified in Coordinated Universal Time (UTC).
--
-- -   Specified in the Unix time format.
--
--     For example, if you want to use an end time of October 1, 2018, at 9
--     PM UTC, specify @1538427600@ as the end time.
--
-- You can convert a human-friendly time to Unix time format using a
-- converter like <https://www.epochconverter.com/ Epoch converter>.
newGetCostEstimate ::
  -- | 'resourceName'
  Prelude.Text ->
  -- | 'startTime'
  Prelude.UTCTime ->
  -- | 'endTime'
  Prelude.UTCTime ->
  GetCostEstimate
newGetCostEstimate
  pResourceName_
  pStartTime_
  pEndTime_ =
    GetCostEstimate'
      { resourceName = pResourceName_,
        startTime = Data._Time Lens.# pStartTime_,
        endTime = Data._Time Lens.# pEndTime_
      }

-- | The resource name.
getCostEstimate_resourceName :: Lens.Lens' GetCostEstimate Prelude.Text
getCostEstimate_resourceName = Lens.lens (\GetCostEstimate' {resourceName} -> resourceName) (\s@GetCostEstimate' {} a -> s {resourceName = a} :: GetCostEstimate)

-- | The cost estimate start time.
--
-- Constraints:
--
-- -   Specified in Coordinated Universal Time (UTC).
--
-- -   Specified in the Unix time format.
--
--     For example, if you want to use a start time of October 1, 2018, at
--     8 PM UTC, specify @1538424000@ as the start time.
--
-- You can convert a human-friendly time to Unix time format using a
-- converter like <https://www.epochconverter.com/ Epoch converter>.
getCostEstimate_startTime :: Lens.Lens' GetCostEstimate Prelude.UTCTime
getCostEstimate_startTime = Lens.lens (\GetCostEstimate' {startTime} -> startTime) (\s@GetCostEstimate' {} a -> s {startTime = a} :: GetCostEstimate) Prelude.. Data._Time

-- | The cost estimate end time.
--
-- Constraints:
--
-- -   Specified in Coordinated Universal Time (UTC).
--
-- -   Specified in the Unix time format.
--
--     For example, if you want to use an end time of October 1, 2018, at 9
--     PM UTC, specify @1538427600@ as the end time.
--
-- You can convert a human-friendly time to Unix time format using a
-- converter like <https://www.epochconverter.com/ Epoch converter>.
getCostEstimate_endTime :: Lens.Lens' GetCostEstimate Prelude.UTCTime
getCostEstimate_endTime = Lens.lens (\GetCostEstimate' {endTime} -> endTime) (\s@GetCostEstimate' {} a -> s {endTime = a} :: GetCostEstimate) Prelude.. Data._Time

instance Core.AWSRequest GetCostEstimate where
  type
    AWSResponse GetCostEstimate =
      GetCostEstimateResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetCostEstimateResponse'
            Prelude.<$> ( x
                            Data..?> "resourcesBudgetEstimate"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetCostEstimate where
  hashWithSalt _salt GetCostEstimate' {..} =
    _salt
      `Prelude.hashWithSalt` resourceName
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` endTime

instance Prelude.NFData GetCostEstimate where
  rnf GetCostEstimate' {..} =
    Prelude.rnf resourceName
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf endTime

instance Data.ToHeaders GetCostEstimate where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Lightsail_20161128.GetCostEstimate" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetCostEstimate where
  toJSON GetCostEstimate' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("resourceName" Data..= resourceName),
            Prelude.Just ("startTime" Data..= startTime),
            Prelude.Just ("endTime" Data..= endTime)
          ]
      )

instance Data.ToPath GetCostEstimate where
  toPath = Prelude.const "/"

instance Data.ToQuery GetCostEstimate where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetCostEstimateResponse' smart constructor.
data GetCostEstimateResponse = GetCostEstimateResponse'
  { -- | Returns the estimate\'s forecasted cost or usage.
    resourcesBudgetEstimate :: Prelude.Maybe [ResourceBudgetEstimate],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetCostEstimateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourcesBudgetEstimate', 'getCostEstimateResponse_resourcesBudgetEstimate' - Returns the estimate\'s forecasted cost or usage.
--
-- 'httpStatus', 'getCostEstimateResponse_httpStatus' - The response's http status code.
newGetCostEstimateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetCostEstimateResponse
newGetCostEstimateResponse pHttpStatus_ =
  GetCostEstimateResponse'
    { resourcesBudgetEstimate =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Returns the estimate\'s forecasted cost or usage.
getCostEstimateResponse_resourcesBudgetEstimate :: Lens.Lens' GetCostEstimateResponse (Prelude.Maybe [ResourceBudgetEstimate])
getCostEstimateResponse_resourcesBudgetEstimate = Lens.lens (\GetCostEstimateResponse' {resourcesBudgetEstimate} -> resourcesBudgetEstimate) (\s@GetCostEstimateResponse' {} a -> s {resourcesBudgetEstimate = a} :: GetCostEstimateResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getCostEstimateResponse_httpStatus :: Lens.Lens' GetCostEstimateResponse Prelude.Int
getCostEstimateResponse_httpStatus = Lens.lens (\GetCostEstimateResponse' {httpStatus} -> httpStatus) (\s@GetCostEstimateResponse' {} a -> s {httpStatus = a} :: GetCostEstimateResponse)

instance Prelude.NFData GetCostEstimateResponse where
  rnf GetCostEstimateResponse' {..} =
    Prelude.rnf resourcesBudgetEstimate
      `Prelude.seq` Prelude.rnf httpStatus
