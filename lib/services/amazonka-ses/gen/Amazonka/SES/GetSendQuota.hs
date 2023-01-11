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
-- Module      : Amazonka.SES.GetSendQuota
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides the sending limits for the Amazon SES account.
--
-- You can execute this operation no more than once per second.
module Amazonka.SES.GetSendQuota
  ( -- * Creating a Request
    GetSendQuota (..),
    newGetSendQuota,

    -- * Destructuring the Response
    GetSendQuotaResponse (..),
    newGetSendQuotaResponse,

    -- * Response Lenses
    getSendQuotaResponse_max24HourSend,
    getSendQuotaResponse_maxSendRate,
    getSendQuotaResponse_sentLast24Hours,
    getSendQuotaResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SES.Types

-- | /See:/ 'newGetSendQuota' smart constructor.
data GetSendQuota = GetSendQuota'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSendQuota' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newGetSendQuota ::
  GetSendQuota
newGetSendQuota = GetSendQuota'

instance Core.AWSRequest GetSendQuota where
  type AWSResponse GetSendQuota = GetSendQuotaResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "GetSendQuotaResult"
      ( \s h x ->
          GetSendQuotaResponse'
            Prelude.<$> (x Data..@? "Max24HourSend")
            Prelude.<*> (x Data..@? "MaxSendRate")
            Prelude.<*> (x Data..@? "SentLast24Hours")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetSendQuota where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance Prelude.NFData GetSendQuota where
  rnf _ = ()

instance Data.ToHeaders GetSendQuota where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetSendQuota where
  toPath = Prelude.const "/"

instance Data.ToQuery GetSendQuota where
  toQuery =
    Prelude.const
      ( Prelude.mconcat
          [ "Action"
              Data.=: ("GetSendQuota" :: Prelude.ByteString),
            "Version"
              Data.=: ("2010-12-01" :: Prelude.ByteString)
          ]
      )

-- | Represents your Amazon SES daily sending quota, maximum send rate, and
-- the number of emails you have sent in the last 24 hours.
--
-- /See:/ 'newGetSendQuotaResponse' smart constructor.
data GetSendQuotaResponse = GetSendQuotaResponse'
  { -- | The maximum number of emails the user is allowed to send in a 24-hour
    -- interval. A value of -1 signifies an unlimited quota.
    max24HourSend :: Prelude.Maybe Prelude.Double,
    -- | The maximum number of emails that Amazon SES can accept from the user\'s
    -- account per second.
    --
    -- The rate at which Amazon SES accepts the user\'s messages might be less
    -- than the maximum send rate.
    maxSendRate :: Prelude.Maybe Prelude.Double,
    -- | The number of emails sent during the previous 24 hours.
    sentLast24Hours :: Prelude.Maybe Prelude.Double,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSendQuotaResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'max24HourSend', 'getSendQuotaResponse_max24HourSend' - The maximum number of emails the user is allowed to send in a 24-hour
-- interval. A value of -1 signifies an unlimited quota.
--
-- 'maxSendRate', 'getSendQuotaResponse_maxSendRate' - The maximum number of emails that Amazon SES can accept from the user\'s
-- account per second.
--
-- The rate at which Amazon SES accepts the user\'s messages might be less
-- than the maximum send rate.
--
-- 'sentLast24Hours', 'getSendQuotaResponse_sentLast24Hours' - The number of emails sent during the previous 24 hours.
--
-- 'httpStatus', 'getSendQuotaResponse_httpStatus' - The response's http status code.
newGetSendQuotaResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetSendQuotaResponse
newGetSendQuotaResponse pHttpStatus_ =
  GetSendQuotaResponse'
    { max24HourSend =
        Prelude.Nothing,
      maxSendRate = Prelude.Nothing,
      sentLast24Hours = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The maximum number of emails the user is allowed to send in a 24-hour
-- interval. A value of -1 signifies an unlimited quota.
getSendQuotaResponse_max24HourSend :: Lens.Lens' GetSendQuotaResponse (Prelude.Maybe Prelude.Double)
getSendQuotaResponse_max24HourSend = Lens.lens (\GetSendQuotaResponse' {max24HourSend} -> max24HourSend) (\s@GetSendQuotaResponse' {} a -> s {max24HourSend = a} :: GetSendQuotaResponse)

-- | The maximum number of emails that Amazon SES can accept from the user\'s
-- account per second.
--
-- The rate at which Amazon SES accepts the user\'s messages might be less
-- than the maximum send rate.
getSendQuotaResponse_maxSendRate :: Lens.Lens' GetSendQuotaResponse (Prelude.Maybe Prelude.Double)
getSendQuotaResponse_maxSendRate = Lens.lens (\GetSendQuotaResponse' {maxSendRate} -> maxSendRate) (\s@GetSendQuotaResponse' {} a -> s {maxSendRate = a} :: GetSendQuotaResponse)

-- | The number of emails sent during the previous 24 hours.
getSendQuotaResponse_sentLast24Hours :: Lens.Lens' GetSendQuotaResponse (Prelude.Maybe Prelude.Double)
getSendQuotaResponse_sentLast24Hours = Lens.lens (\GetSendQuotaResponse' {sentLast24Hours} -> sentLast24Hours) (\s@GetSendQuotaResponse' {} a -> s {sentLast24Hours = a} :: GetSendQuotaResponse)

-- | The response's http status code.
getSendQuotaResponse_httpStatus :: Lens.Lens' GetSendQuotaResponse Prelude.Int
getSendQuotaResponse_httpStatus = Lens.lens (\GetSendQuotaResponse' {httpStatus} -> httpStatus) (\s@GetSendQuotaResponse' {} a -> s {httpStatus = a} :: GetSendQuotaResponse)

instance Prelude.NFData GetSendQuotaResponse where
  rnf GetSendQuotaResponse' {..} =
    Prelude.rnf max24HourSend
      `Prelude.seq` Prelude.rnf maxSendRate
      `Prelude.seq` Prelude.rnf sentLast24Hours
      `Prelude.seq` Prelude.rnf httpStatus
