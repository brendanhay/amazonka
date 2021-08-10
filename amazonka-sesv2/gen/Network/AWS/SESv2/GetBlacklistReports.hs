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
-- Module      : Network.AWS.SESv2.GetBlacklistReports
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieve a list of the blacklists that your dedicated IP addresses
-- appear on.
module Network.AWS.SESv2.GetBlacklistReports
  ( -- * Creating a Request
    GetBlacklistReports (..),
    newGetBlacklistReports,

    -- * Request Lenses
    getBlacklistReports_blacklistItemNames,

    -- * Destructuring the Response
    GetBlacklistReportsResponse (..),
    newGetBlacklistReportsResponse,

    -- * Response Lenses
    getBlacklistReportsResponse_httpStatus,
    getBlacklistReportsResponse_blacklistReport,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SESv2.Types

-- | A request to retrieve a list of the blacklists that your dedicated IP
-- addresses appear on.
--
-- /See:/ 'newGetBlacklistReports' smart constructor.
data GetBlacklistReports = GetBlacklistReports'
  { -- | A list of IP addresses that you want to retrieve blacklist information
    -- about. You can only specify the dedicated IP addresses that you use to
    -- send email using Amazon SES or Amazon Pinpoint.
    blacklistItemNames :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetBlacklistReports' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'blacklistItemNames', 'getBlacklistReports_blacklistItemNames' - A list of IP addresses that you want to retrieve blacklist information
-- about. You can only specify the dedicated IP addresses that you use to
-- send email using Amazon SES or Amazon Pinpoint.
newGetBlacklistReports ::
  GetBlacklistReports
newGetBlacklistReports =
  GetBlacklistReports'
    { blacklistItemNames =
        Prelude.mempty
    }

-- | A list of IP addresses that you want to retrieve blacklist information
-- about. You can only specify the dedicated IP addresses that you use to
-- send email using Amazon SES or Amazon Pinpoint.
getBlacklistReports_blacklistItemNames :: Lens.Lens' GetBlacklistReports [Prelude.Text]
getBlacklistReports_blacklistItemNames = Lens.lens (\GetBlacklistReports' {blacklistItemNames} -> blacklistItemNames) (\s@GetBlacklistReports' {} a -> s {blacklistItemNames = a} :: GetBlacklistReports) Prelude.. Lens._Coerce

instance Core.AWSRequest GetBlacklistReports where
  type
    AWSResponse GetBlacklistReports =
      GetBlacklistReportsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetBlacklistReportsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Core..?> "BlacklistReport"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable GetBlacklistReports

instance Prelude.NFData GetBlacklistReports

instance Core.ToHeaders GetBlacklistReports where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetBlacklistReports where
  toPath =
    Prelude.const
      "/v2/email/deliverability-dashboard/blacklist-report"

instance Core.ToQuery GetBlacklistReports where
  toQuery GetBlacklistReports' {..} =
    Prelude.mconcat
      [ "BlacklistItemNames"
          Core.=: Core.toQueryList "member" blacklistItemNames
      ]

-- | An object that contains information about blacklist events.
--
-- /See:/ 'newGetBlacklistReportsResponse' smart constructor.
data GetBlacklistReportsResponse = GetBlacklistReportsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | An object that contains information about a blacklist that one of your
    -- dedicated IP addresses appears on.
    blacklistReport :: Prelude.HashMap Prelude.Text [BlacklistEntry]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetBlacklistReportsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getBlacklistReportsResponse_httpStatus' - The response's http status code.
--
-- 'blacklistReport', 'getBlacklistReportsResponse_blacklistReport' - An object that contains information about a blacklist that one of your
-- dedicated IP addresses appears on.
newGetBlacklistReportsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetBlacklistReportsResponse
newGetBlacklistReportsResponse pHttpStatus_ =
  GetBlacklistReportsResponse'
    { httpStatus =
        pHttpStatus_,
      blacklistReport = Prelude.mempty
    }

-- | The response's http status code.
getBlacklistReportsResponse_httpStatus :: Lens.Lens' GetBlacklistReportsResponse Prelude.Int
getBlacklistReportsResponse_httpStatus = Lens.lens (\GetBlacklistReportsResponse' {httpStatus} -> httpStatus) (\s@GetBlacklistReportsResponse' {} a -> s {httpStatus = a} :: GetBlacklistReportsResponse)

-- | An object that contains information about a blacklist that one of your
-- dedicated IP addresses appears on.
getBlacklistReportsResponse_blacklistReport :: Lens.Lens' GetBlacklistReportsResponse (Prelude.HashMap Prelude.Text [BlacklistEntry])
getBlacklistReportsResponse_blacklistReport = Lens.lens (\GetBlacklistReportsResponse' {blacklistReport} -> blacklistReport) (\s@GetBlacklistReportsResponse' {} a -> s {blacklistReport = a} :: GetBlacklistReportsResponse) Prelude.. Lens._Coerce

instance Prelude.NFData GetBlacklistReportsResponse
