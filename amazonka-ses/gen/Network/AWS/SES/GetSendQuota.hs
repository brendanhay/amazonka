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
-- Module      : Network.AWS.SES.GetSendQuota
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides the sending limits for the Amazon SES account.
--
-- You can execute this operation no more than once per second.
module Network.AWS.SES.GetSendQuota
  ( -- * Creating a Request
    GetSendQuota (..),
    newGetSendQuota,

    -- * Destructuring the Response
    GetSendQuotaResponse (..),
    newGetSendQuotaResponse,

    -- * Response Lenses
    getSendQuotaResponse_max24HourSend,
    getSendQuotaResponse_sentLast24Hours,
    getSendQuotaResponse_maxSendRate,
    getSendQuotaResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SES.Types

-- | /See:/ 'newGetSendQuota' smart constructor.
data GetSendQuota = GetSendQuota'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetSendQuota' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newGetSendQuota ::
  GetSendQuota
newGetSendQuota = GetSendQuota'

instance Core.AWSRequest GetSendQuota where
  type AWSResponse GetSendQuota = GetSendQuotaResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "GetSendQuotaResult"
      ( \s h x ->
          GetSendQuotaResponse'
            Core.<$> (x Core..@? "Max24HourSend")
            Core.<*> (x Core..@? "SentLast24Hours")
            Core.<*> (x Core..@? "MaxSendRate")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetSendQuota

instance Core.NFData GetSendQuota

instance Core.ToHeaders GetSendQuota where
  toHeaders = Core.const Core.mempty

instance Core.ToPath GetSendQuota where
  toPath = Core.const "/"

instance Core.ToQuery GetSendQuota where
  toQuery =
    Core.const
      ( Core.mconcat
          [ "Action"
              Core.=: ("GetSendQuota" :: Core.ByteString),
            "Version" Core.=: ("2010-12-01" :: Core.ByteString)
          ]
      )

-- | Represents your Amazon SES daily sending quota, maximum send rate, and
-- the number of emails you have sent in the last 24 hours.
--
-- /See:/ 'newGetSendQuotaResponse' smart constructor.
data GetSendQuotaResponse = GetSendQuotaResponse'
  { -- | The maximum number of emails the user is allowed to send in a 24-hour
    -- interval. A value of -1 signifies an unlimited quota.
    max24HourSend :: Core.Maybe Core.Double,
    -- | The number of emails sent during the previous 24 hours.
    sentLast24Hours :: Core.Maybe Core.Double,
    -- | The maximum number of emails that Amazon SES can accept from the user\'s
    -- account per second.
    --
    -- The rate at which Amazon SES accepts the user\'s messages might be less
    -- than the maximum send rate.
    maxSendRate :: Core.Maybe Core.Double,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- 'sentLast24Hours', 'getSendQuotaResponse_sentLast24Hours' - The number of emails sent during the previous 24 hours.
--
-- 'maxSendRate', 'getSendQuotaResponse_maxSendRate' - The maximum number of emails that Amazon SES can accept from the user\'s
-- account per second.
--
-- The rate at which Amazon SES accepts the user\'s messages might be less
-- than the maximum send rate.
--
-- 'httpStatus', 'getSendQuotaResponse_httpStatus' - The response's http status code.
newGetSendQuotaResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetSendQuotaResponse
newGetSendQuotaResponse pHttpStatus_ =
  GetSendQuotaResponse'
    { max24HourSend = Core.Nothing,
      sentLast24Hours = Core.Nothing,
      maxSendRate = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The maximum number of emails the user is allowed to send in a 24-hour
-- interval. A value of -1 signifies an unlimited quota.
getSendQuotaResponse_max24HourSend :: Lens.Lens' GetSendQuotaResponse (Core.Maybe Core.Double)
getSendQuotaResponse_max24HourSend = Lens.lens (\GetSendQuotaResponse' {max24HourSend} -> max24HourSend) (\s@GetSendQuotaResponse' {} a -> s {max24HourSend = a} :: GetSendQuotaResponse)

-- | The number of emails sent during the previous 24 hours.
getSendQuotaResponse_sentLast24Hours :: Lens.Lens' GetSendQuotaResponse (Core.Maybe Core.Double)
getSendQuotaResponse_sentLast24Hours = Lens.lens (\GetSendQuotaResponse' {sentLast24Hours} -> sentLast24Hours) (\s@GetSendQuotaResponse' {} a -> s {sentLast24Hours = a} :: GetSendQuotaResponse)

-- | The maximum number of emails that Amazon SES can accept from the user\'s
-- account per second.
--
-- The rate at which Amazon SES accepts the user\'s messages might be less
-- than the maximum send rate.
getSendQuotaResponse_maxSendRate :: Lens.Lens' GetSendQuotaResponse (Core.Maybe Core.Double)
getSendQuotaResponse_maxSendRate = Lens.lens (\GetSendQuotaResponse' {maxSendRate} -> maxSendRate) (\s@GetSendQuotaResponse' {} a -> s {maxSendRate = a} :: GetSendQuotaResponse)

-- | The response's http status code.
getSendQuotaResponse_httpStatus :: Lens.Lens' GetSendQuotaResponse Core.Int
getSendQuotaResponse_httpStatus = Lens.lens (\GetSendQuotaResponse' {httpStatus} -> httpStatus) (\s@GetSendQuotaResponse' {} a -> s {httpStatus = a} :: GetSendQuotaResponse)

instance Core.NFData GetSendQuotaResponse
