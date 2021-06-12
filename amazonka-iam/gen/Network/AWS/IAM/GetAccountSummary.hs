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
-- Module      : Network.AWS.IAM.GetAccountSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about IAM entity usage and IAM quotas in the AWS
-- account.
--
-- For information about IAM quotas, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_iam-quotas.html IAM and STS quotas>
-- in the /IAM User Guide/.
module Network.AWS.IAM.GetAccountSummary
  ( -- * Creating a Request
    GetAccountSummary (..),
    newGetAccountSummary,

    -- * Destructuring the Response
    GetAccountSummaryResponse (..),
    newGetAccountSummaryResponse,

    -- * Response Lenses
    getAccountSummaryResponse_summaryMap,
    getAccountSummaryResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetAccountSummary' smart constructor.
data GetAccountSummary = GetAccountSummary'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetAccountSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newGetAccountSummary ::
  GetAccountSummary
newGetAccountSummary = GetAccountSummary'

instance Core.AWSRequest GetAccountSummary where
  type
    AWSResponse GetAccountSummary =
      GetAccountSummaryResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "GetAccountSummaryResult"
      ( \s h x ->
          GetAccountSummaryResponse'
            Core.<$> ( x Core..@? "SummaryMap" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLMap "entry" "key" "value")
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetAccountSummary

instance Core.NFData GetAccountSummary

instance Core.ToHeaders GetAccountSummary where
  toHeaders = Core.const Core.mempty

instance Core.ToPath GetAccountSummary where
  toPath = Core.const "/"

instance Core.ToQuery GetAccountSummary where
  toQuery =
    Core.const
      ( Core.mconcat
          [ "Action"
              Core.=: ("GetAccountSummary" :: Core.ByteString),
            "Version" Core.=: ("2010-05-08" :: Core.ByteString)
          ]
      )

-- | Contains the response to a successful GetAccountSummary request.
--
-- /See:/ 'newGetAccountSummaryResponse' smart constructor.
data GetAccountSummaryResponse = GetAccountSummaryResponse'
  { -- | A set of key–value pairs containing information about IAM entity usage
    -- and IAM quotas.
    summaryMap :: Core.Maybe (Core.HashMap SummaryKeyType Core.Int),
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetAccountSummaryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'summaryMap', 'getAccountSummaryResponse_summaryMap' - A set of key–value pairs containing information about IAM entity usage
-- and IAM quotas.
--
-- 'httpStatus', 'getAccountSummaryResponse_httpStatus' - The response's http status code.
newGetAccountSummaryResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetAccountSummaryResponse
newGetAccountSummaryResponse pHttpStatus_ =
  GetAccountSummaryResponse'
    { summaryMap =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A set of key–value pairs containing information about IAM entity usage
-- and IAM quotas.
getAccountSummaryResponse_summaryMap :: Lens.Lens' GetAccountSummaryResponse (Core.Maybe (Core.HashMap SummaryKeyType Core.Int))
getAccountSummaryResponse_summaryMap = Lens.lens (\GetAccountSummaryResponse' {summaryMap} -> summaryMap) (\s@GetAccountSummaryResponse' {} a -> s {summaryMap = a} :: GetAccountSummaryResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getAccountSummaryResponse_httpStatus :: Lens.Lens' GetAccountSummaryResponse Core.Int
getAccountSummaryResponse_httpStatus = Lens.lens (\GetAccountSummaryResponse' {httpStatus} -> httpStatus) (\s@GetAccountSummaryResponse' {} a -> s {httpStatus = a} :: GetAccountSummaryResponse)

instance Core.NFData GetAccountSummaryResponse
