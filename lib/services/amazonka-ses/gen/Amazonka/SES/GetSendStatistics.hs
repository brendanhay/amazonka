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
-- Module      : Amazonka.SES.GetSendStatistics
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides sending statistics for the current AWS Region. The result is a
-- list of data points, representing the last two weeks of sending
-- activity. Each data point in the list contains statistics for a
-- 15-minute period of time.
--
-- You can execute this operation no more than once per second.
module Amazonka.SES.GetSendStatistics
  ( -- * Creating a Request
    GetSendStatistics (..),
    newGetSendStatistics,

    -- * Destructuring the Response
    GetSendStatisticsResponse (..),
    newGetSendStatisticsResponse,

    -- * Response Lenses
    getSendStatisticsResponse_sendDataPoints,
    getSendStatisticsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SES.Types

-- | /See:/ 'newGetSendStatistics' smart constructor.
data GetSendStatistics = GetSendStatistics'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSendStatistics' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newGetSendStatistics ::
  GetSendStatistics
newGetSendStatistics = GetSendStatistics'

instance Core.AWSRequest GetSendStatistics where
  type
    AWSResponse GetSendStatistics =
      GetSendStatisticsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "GetSendStatisticsResult"
      ( \s h x ->
          GetSendStatisticsResponse'
            Prelude.<$> ( x Data..@? "SendDataPoints" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "member")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetSendStatistics where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance Prelude.NFData GetSendStatistics where
  rnf _ = ()

instance Data.ToHeaders GetSendStatistics where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetSendStatistics where
  toPath = Prelude.const "/"

instance Data.ToQuery GetSendStatistics where
  toQuery =
    Prelude.const
      ( Prelude.mconcat
          [ "Action"
              Data.=: ("GetSendStatistics" :: Prelude.ByteString),
            "Version"
              Data.=: ("2010-12-01" :: Prelude.ByteString)
          ]
      )

-- | Represents a list of data points. This list contains aggregated data
-- from the previous two weeks of your sending activity with Amazon SES.
--
-- /See:/ 'newGetSendStatisticsResponse' smart constructor.
data GetSendStatisticsResponse = GetSendStatisticsResponse'
  { -- | A list of data points, each of which represents 15 minutes of activity.
    sendDataPoints :: Prelude.Maybe [SendDataPoint],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSendStatisticsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sendDataPoints', 'getSendStatisticsResponse_sendDataPoints' - A list of data points, each of which represents 15 minutes of activity.
--
-- 'httpStatus', 'getSendStatisticsResponse_httpStatus' - The response's http status code.
newGetSendStatisticsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetSendStatisticsResponse
newGetSendStatisticsResponse pHttpStatus_ =
  GetSendStatisticsResponse'
    { sendDataPoints =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of data points, each of which represents 15 minutes of activity.
getSendStatisticsResponse_sendDataPoints :: Lens.Lens' GetSendStatisticsResponse (Prelude.Maybe [SendDataPoint])
getSendStatisticsResponse_sendDataPoints = Lens.lens (\GetSendStatisticsResponse' {sendDataPoints} -> sendDataPoints) (\s@GetSendStatisticsResponse' {} a -> s {sendDataPoints = a} :: GetSendStatisticsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getSendStatisticsResponse_httpStatus :: Lens.Lens' GetSendStatisticsResponse Prelude.Int
getSendStatisticsResponse_httpStatus = Lens.lens (\GetSendStatisticsResponse' {httpStatus} -> httpStatus) (\s@GetSendStatisticsResponse' {} a -> s {httpStatus = a} :: GetSendStatisticsResponse)

instance Prelude.NFData GetSendStatisticsResponse where
  rnf GetSendStatisticsResponse' {..} =
    Prelude.rnf sendDataPoints `Prelude.seq`
      Prelude.rnf httpStatus
