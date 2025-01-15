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
-- Module      : Amazonka.IAM.GetAccountSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about IAM entity usage and IAM quotas in the
-- Amazon Web Services account.
--
-- For information about IAM quotas, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_iam-quotas.html IAM and STS quotas>
-- in the /IAM User Guide/.
module Amazonka.IAM.GetAccountSummary
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IAM.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetAccountSummary' smart constructor.
data GetAccountSummary = GetAccountSummary'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "GetAccountSummaryResult"
      ( \s h x ->
          GetAccountSummaryResponse'
            Prelude.<$> ( x Data..@? "SummaryMap" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLMap "entry" "key" "value")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetAccountSummary where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance Prelude.NFData GetAccountSummary where
  rnf _ = ()

instance Data.ToHeaders GetAccountSummary where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetAccountSummary where
  toPath = Prelude.const "/"

instance Data.ToQuery GetAccountSummary where
  toQuery =
    Prelude.const
      ( Prelude.mconcat
          [ "Action"
              Data.=: ("GetAccountSummary" :: Prelude.ByteString),
            "Version"
              Data.=: ("2010-05-08" :: Prelude.ByteString)
          ]
      )

-- | Contains the response to a successful GetAccountSummary request.
--
-- /See:/ 'newGetAccountSummaryResponse' smart constructor.
data GetAccountSummaryResponse = GetAccountSummaryResponse'
  { -- | A set of key–value pairs containing information about IAM entity usage
    -- and IAM quotas.
    summaryMap :: Prelude.Maybe (Prelude.HashMap SummaryKeyType Prelude.Int),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  GetAccountSummaryResponse
newGetAccountSummaryResponse pHttpStatus_ =
  GetAccountSummaryResponse'
    { summaryMap =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A set of key–value pairs containing information about IAM entity usage
-- and IAM quotas.
getAccountSummaryResponse_summaryMap :: Lens.Lens' GetAccountSummaryResponse (Prelude.Maybe (Prelude.HashMap SummaryKeyType Prelude.Int))
getAccountSummaryResponse_summaryMap = Lens.lens (\GetAccountSummaryResponse' {summaryMap} -> summaryMap) (\s@GetAccountSummaryResponse' {} a -> s {summaryMap = a} :: GetAccountSummaryResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getAccountSummaryResponse_httpStatus :: Lens.Lens' GetAccountSummaryResponse Prelude.Int
getAccountSummaryResponse_httpStatus = Lens.lens (\GetAccountSummaryResponse' {httpStatus} -> httpStatus) (\s@GetAccountSummaryResponse' {} a -> s {httpStatus = a} :: GetAccountSummaryResponse)

instance Prelude.NFData GetAccountSummaryResponse where
  rnf GetAccountSummaryResponse' {..} =
    Prelude.rnf summaryMap `Prelude.seq`
      Prelude.rnf httpStatus
