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
-- Module      : Amazonka.ConnectCampaigns.GetCampaignStateBatch
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get state of campaigns for the specified Amazon Connect account.
module Amazonka.ConnectCampaigns.GetCampaignStateBatch
  ( -- * Creating a Request
    GetCampaignStateBatch (..),
    newGetCampaignStateBatch,

    -- * Request Lenses
    getCampaignStateBatch_campaignIds,

    -- * Destructuring the Response
    GetCampaignStateBatchResponse (..),
    newGetCampaignStateBatchResponse,

    -- * Response Lenses
    getCampaignStateBatchResponse_failedRequests,
    getCampaignStateBatchResponse_successfulRequests,
    getCampaignStateBatchResponse_httpStatus,
  )
where

import Amazonka.ConnectCampaigns.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | GetCampaignStateBatchRequest
--
-- /See:/ 'newGetCampaignStateBatch' smart constructor.
data GetCampaignStateBatch = GetCampaignStateBatch'
  { campaignIds :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetCampaignStateBatch' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'campaignIds', 'getCampaignStateBatch_campaignIds' - Undocumented member.
newGetCampaignStateBatch ::
  -- | 'campaignIds'
  Prelude.NonEmpty Prelude.Text ->
  GetCampaignStateBatch
newGetCampaignStateBatch pCampaignIds_ =
  GetCampaignStateBatch'
    { campaignIds =
        Lens.coerced Lens.# pCampaignIds_
    }

-- | Undocumented member.
getCampaignStateBatch_campaignIds :: Lens.Lens' GetCampaignStateBatch (Prelude.NonEmpty Prelude.Text)
getCampaignStateBatch_campaignIds = Lens.lens (\GetCampaignStateBatch' {campaignIds} -> campaignIds) (\s@GetCampaignStateBatch' {} a -> s {campaignIds = a} :: GetCampaignStateBatch) Prelude.. Lens.coerced

instance Core.AWSRequest GetCampaignStateBatch where
  type
    AWSResponse GetCampaignStateBatch =
      GetCampaignStateBatchResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetCampaignStateBatchResponse'
            Prelude.<$> (x Data..?> "failedRequests" Core..!@ Prelude.mempty)
            Prelude.<*> ( x Data..?> "successfulRequests"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetCampaignStateBatch where
  hashWithSalt _salt GetCampaignStateBatch' {..} =
    _salt `Prelude.hashWithSalt` campaignIds

instance Prelude.NFData GetCampaignStateBatch where
  rnf GetCampaignStateBatch' {..} =
    Prelude.rnf campaignIds

instance Data.ToHeaders GetCampaignStateBatch where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetCampaignStateBatch where
  toJSON GetCampaignStateBatch' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("campaignIds" Data..= campaignIds)]
      )

instance Data.ToPath GetCampaignStateBatch where
  toPath = Prelude.const "/campaigns-state"

instance Data.ToQuery GetCampaignStateBatch where
  toQuery = Prelude.const Prelude.mempty

-- | GetCampaignStateBatchResponse
--
-- /See:/ 'newGetCampaignStateBatchResponse' smart constructor.
data GetCampaignStateBatchResponse = GetCampaignStateBatchResponse'
  { failedRequests :: Prelude.Maybe [FailedCampaignStateResponse],
    successfulRequests :: Prelude.Maybe [SuccessfulCampaignStateResponse],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetCampaignStateBatchResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failedRequests', 'getCampaignStateBatchResponse_failedRequests' - Undocumented member.
--
-- 'successfulRequests', 'getCampaignStateBatchResponse_successfulRequests' - Undocumented member.
--
-- 'httpStatus', 'getCampaignStateBatchResponse_httpStatus' - The response's http status code.
newGetCampaignStateBatchResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetCampaignStateBatchResponse
newGetCampaignStateBatchResponse pHttpStatus_ =
  GetCampaignStateBatchResponse'
    { failedRequests =
        Prelude.Nothing,
      successfulRequests = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
getCampaignStateBatchResponse_failedRequests :: Lens.Lens' GetCampaignStateBatchResponse (Prelude.Maybe [FailedCampaignStateResponse])
getCampaignStateBatchResponse_failedRequests = Lens.lens (\GetCampaignStateBatchResponse' {failedRequests} -> failedRequests) (\s@GetCampaignStateBatchResponse' {} a -> s {failedRequests = a} :: GetCampaignStateBatchResponse) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
getCampaignStateBatchResponse_successfulRequests :: Lens.Lens' GetCampaignStateBatchResponse (Prelude.Maybe [SuccessfulCampaignStateResponse])
getCampaignStateBatchResponse_successfulRequests = Lens.lens (\GetCampaignStateBatchResponse' {successfulRequests} -> successfulRequests) (\s@GetCampaignStateBatchResponse' {} a -> s {successfulRequests = a} :: GetCampaignStateBatchResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getCampaignStateBatchResponse_httpStatus :: Lens.Lens' GetCampaignStateBatchResponse Prelude.Int
getCampaignStateBatchResponse_httpStatus = Lens.lens (\GetCampaignStateBatchResponse' {httpStatus} -> httpStatus) (\s@GetCampaignStateBatchResponse' {} a -> s {httpStatus = a} :: GetCampaignStateBatchResponse)

instance Prelude.NFData GetCampaignStateBatchResponse where
  rnf GetCampaignStateBatchResponse' {..} =
    Prelude.rnf failedRequests
      `Prelude.seq` Prelude.rnf successfulRequests
      `Prelude.seq` Prelude.rnf httpStatus
