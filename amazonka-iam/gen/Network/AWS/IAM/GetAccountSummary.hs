{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetAccountSummary' smart constructor.
data GetAccountSummary = GetAccountSummary'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetAccountSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newGetAccountSummary ::
  GetAccountSummary
newGetAccountSummary = GetAccountSummary'

instance Prelude.AWSRequest GetAccountSummary where
  type Rs GetAccountSummary = GetAccountSummaryResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "GetAccountSummaryResult"
      ( \s h x ->
          GetAccountSummaryResponse'
            Prelude.<$> ( x Prelude..@? "SummaryMap"
                            Prelude..!@ Prelude.mempty
                            Prelude.>>= Prelude.may
                              (Prelude.parseXMLMap "entry" "key" "value")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetAccountSummary

instance Prelude.NFData GetAccountSummary

instance Prelude.ToHeaders GetAccountSummary where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath GetAccountSummary where
  toPath = Prelude.const "/"

instance Prelude.ToQuery GetAccountSummary where
  toQuery =
    Prelude.const
      ( Prelude.mconcat
          [ "Action"
              Prelude.=: ("GetAccountSummary" :: Prelude.ByteString),
            "Version"
              Prelude.=: ("2010-05-08" :: Prelude.ByteString)
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
getAccountSummaryResponse_summaryMap = Lens.lens (\GetAccountSummaryResponse' {summaryMap} -> summaryMap) (\s@GetAccountSummaryResponse' {} a -> s {summaryMap = a} :: GetAccountSummaryResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
getAccountSummaryResponse_httpStatus :: Lens.Lens' GetAccountSummaryResponse Prelude.Int
getAccountSummaryResponse_httpStatus = Lens.lens (\GetAccountSummaryResponse' {httpStatus} -> httpStatus) (\s@GetAccountSummaryResponse' {} a -> s {httpStatus = a} :: GetAccountSummaryResponse)

instance Prelude.NFData GetAccountSummaryResponse
