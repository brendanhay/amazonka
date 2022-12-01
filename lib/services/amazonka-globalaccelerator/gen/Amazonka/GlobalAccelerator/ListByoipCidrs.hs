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
-- Module      : Amazonka.GlobalAccelerator.ListByoipCidrs
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the IP address ranges that were specified in calls to
-- <https://docs.aws.amazon.com/global-accelerator/latest/api/ProvisionByoipCidr.html ProvisionByoipCidr>,
-- including the current state and a history of state changes.
--
-- This operation returns paginated results.
module Amazonka.GlobalAccelerator.ListByoipCidrs
  ( -- * Creating a Request
    ListByoipCidrs (..),
    newListByoipCidrs,

    -- * Request Lenses
    listByoipCidrs_nextToken,
    listByoipCidrs_maxResults,

    -- * Destructuring the Response
    ListByoipCidrsResponse (..),
    newListByoipCidrsResponse,

    -- * Response Lenses
    listByoipCidrsResponse_nextToken,
    listByoipCidrsResponse_byoipCidrs,
    listByoipCidrsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.GlobalAccelerator.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListByoipCidrs' smart constructor.
data ListByoipCidrs = ListByoipCidrs'
  { -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return with a single call. To retrieve
    -- the remaining results, make another call with the returned @nextToken@
    -- value.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListByoipCidrs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listByoipCidrs_nextToken' - The token for the next page of results.
--
-- 'maxResults', 'listByoipCidrs_maxResults' - The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
newListByoipCidrs ::
  ListByoipCidrs
newListByoipCidrs =
  ListByoipCidrs'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The token for the next page of results.
listByoipCidrs_nextToken :: Lens.Lens' ListByoipCidrs (Prelude.Maybe Prelude.Text)
listByoipCidrs_nextToken = Lens.lens (\ListByoipCidrs' {nextToken} -> nextToken) (\s@ListByoipCidrs' {} a -> s {nextToken = a} :: ListByoipCidrs)

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
listByoipCidrs_maxResults :: Lens.Lens' ListByoipCidrs (Prelude.Maybe Prelude.Natural)
listByoipCidrs_maxResults = Lens.lens (\ListByoipCidrs' {maxResults} -> maxResults) (\s@ListByoipCidrs' {} a -> s {maxResults = a} :: ListByoipCidrs)

instance Core.AWSPager ListByoipCidrs where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listByoipCidrsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listByoipCidrsResponse_byoipCidrs
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listByoipCidrs_nextToken
          Lens..~ rs
          Lens.^? listByoipCidrsResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListByoipCidrs where
  type
    AWSResponse ListByoipCidrs =
      ListByoipCidrsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListByoipCidrsResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "ByoipCidrs" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListByoipCidrs where
  hashWithSalt _salt ListByoipCidrs' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListByoipCidrs where
  rnf ListByoipCidrs' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults

instance Core.ToHeaders ListByoipCidrs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "GlobalAccelerator_V20180706.ListByoipCidrs" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListByoipCidrs where
  toJSON ListByoipCidrs' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath ListByoipCidrs where
  toPath = Prelude.const "/"

instance Core.ToQuery ListByoipCidrs where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListByoipCidrsResponse' smart constructor.
data ListByoipCidrsResponse = ListByoipCidrsResponse'
  { -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Information about your address ranges.
    byoipCidrs :: Prelude.Maybe [ByoipCidr],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListByoipCidrsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listByoipCidrsResponse_nextToken' - The token for the next page of results.
--
-- 'byoipCidrs', 'listByoipCidrsResponse_byoipCidrs' - Information about your address ranges.
--
-- 'httpStatus', 'listByoipCidrsResponse_httpStatus' - The response's http status code.
newListByoipCidrsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListByoipCidrsResponse
newListByoipCidrsResponse pHttpStatus_ =
  ListByoipCidrsResponse'
    { nextToken =
        Prelude.Nothing,
      byoipCidrs = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token for the next page of results.
listByoipCidrsResponse_nextToken :: Lens.Lens' ListByoipCidrsResponse (Prelude.Maybe Prelude.Text)
listByoipCidrsResponse_nextToken = Lens.lens (\ListByoipCidrsResponse' {nextToken} -> nextToken) (\s@ListByoipCidrsResponse' {} a -> s {nextToken = a} :: ListByoipCidrsResponse)

-- | Information about your address ranges.
listByoipCidrsResponse_byoipCidrs :: Lens.Lens' ListByoipCidrsResponse (Prelude.Maybe [ByoipCidr])
listByoipCidrsResponse_byoipCidrs = Lens.lens (\ListByoipCidrsResponse' {byoipCidrs} -> byoipCidrs) (\s@ListByoipCidrsResponse' {} a -> s {byoipCidrs = a} :: ListByoipCidrsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listByoipCidrsResponse_httpStatus :: Lens.Lens' ListByoipCidrsResponse Prelude.Int
listByoipCidrsResponse_httpStatus = Lens.lens (\ListByoipCidrsResponse' {httpStatus} -> httpStatus) (\s@ListByoipCidrsResponse' {} a -> s {httpStatus = a} :: ListByoipCidrsResponse)

instance Prelude.NFData ListByoipCidrsResponse where
  rnf ListByoipCidrsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf byoipCidrs
      `Prelude.seq` Prelude.rnf httpStatus
