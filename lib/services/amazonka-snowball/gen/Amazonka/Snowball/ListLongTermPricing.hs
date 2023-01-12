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
-- Module      : Amazonka.Snowball.ListLongTermPricing
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all long-term pricing types.
--
-- This operation returns paginated results.
module Amazonka.Snowball.ListLongTermPricing
  ( -- * Creating a Request
    ListLongTermPricing (..),
    newListLongTermPricing,

    -- * Request Lenses
    listLongTermPricing_maxResults,
    listLongTermPricing_nextToken,

    -- * Destructuring the Response
    ListLongTermPricingResponse (..),
    newListLongTermPricingResponse,

    -- * Response Lenses
    listLongTermPricingResponse_longTermPricingEntries,
    listLongTermPricingResponse_nextToken,
    listLongTermPricingResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Snowball.Types

-- | /See:/ 'newListLongTermPricing' smart constructor.
data ListLongTermPricing = ListLongTermPricing'
  { -- | The maximum number of @ListLongTermPricing@ objects to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Because HTTP requests are stateless, this is the starting point for your
    -- next list of @ListLongTermPricing@ to return.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListLongTermPricing' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listLongTermPricing_maxResults' - The maximum number of @ListLongTermPricing@ objects to return.
--
-- 'nextToken', 'listLongTermPricing_nextToken' - Because HTTP requests are stateless, this is the starting point for your
-- next list of @ListLongTermPricing@ to return.
newListLongTermPricing ::
  ListLongTermPricing
newListLongTermPricing =
  ListLongTermPricing'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The maximum number of @ListLongTermPricing@ objects to return.
listLongTermPricing_maxResults :: Lens.Lens' ListLongTermPricing (Prelude.Maybe Prelude.Natural)
listLongTermPricing_maxResults = Lens.lens (\ListLongTermPricing' {maxResults} -> maxResults) (\s@ListLongTermPricing' {} a -> s {maxResults = a} :: ListLongTermPricing)

-- | Because HTTP requests are stateless, this is the starting point for your
-- next list of @ListLongTermPricing@ to return.
listLongTermPricing_nextToken :: Lens.Lens' ListLongTermPricing (Prelude.Maybe Prelude.Text)
listLongTermPricing_nextToken = Lens.lens (\ListLongTermPricing' {nextToken} -> nextToken) (\s@ListLongTermPricing' {} a -> s {nextToken = a} :: ListLongTermPricing)

instance Core.AWSPager ListLongTermPricing where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listLongTermPricingResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listLongTermPricingResponse_longTermPricingEntries
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listLongTermPricing_nextToken
          Lens..~ rs
          Lens.^? listLongTermPricingResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListLongTermPricing where
  type
    AWSResponse ListLongTermPricing =
      ListLongTermPricingResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListLongTermPricingResponse'
            Prelude.<$> ( x Data..?> "LongTermPricingEntries"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListLongTermPricing where
  hashWithSalt _salt ListLongTermPricing' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListLongTermPricing where
  rnf ListLongTermPricing' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListLongTermPricing where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSIESnowballJobManagementService.ListLongTermPricing" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListLongTermPricing where
  toJSON ListLongTermPricing' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListLongTermPricing where
  toPath = Prelude.const "/"

instance Data.ToQuery ListLongTermPricing where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListLongTermPricingResponse' smart constructor.
data ListLongTermPricingResponse = ListLongTermPricingResponse'
  { -- | Each @LongTermPricingEntry@ object contains a status, ID, and other
    -- information about the @LongTermPricing@ type.
    longTermPricingEntries :: Prelude.Maybe [LongTermPricingListEntry],
    -- | Because HTTP requests are stateless, this is the starting point for your
    -- next list of returned @ListLongTermPricing@ list.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListLongTermPricingResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'longTermPricingEntries', 'listLongTermPricingResponse_longTermPricingEntries' - Each @LongTermPricingEntry@ object contains a status, ID, and other
-- information about the @LongTermPricing@ type.
--
-- 'nextToken', 'listLongTermPricingResponse_nextToken' - Because HTTP requests are stateless, this is the starting point for your
-- next list of returned @ListLongTermPricing@ list.
--
-- 'httpStatus', 'listLongTermPricingResponse_httpStatus' - The response's http status code.
newListLongTermPricingResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListLongTermPricingResponse
newListLongTermPricingResponse pHttpStatus_ =
  ListLongTermPricingResponse'
    { longTermPricingEntries =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Each @LongTermPricingEntry@ object contains a status, ID, and other
-- information about the @LongTermPricing@ type.
listLongTermPricingResponse_longTermPricingEntries :: Lens.Lens' ListLongTermPricingResponse (Prelude.Maybe [LongTermPricingListEntry])
listLongTermPricingResponse_longTermPricingEntries = Lens.lens (\ListLongTermPricingResponse' {longTermPricingEntries} -> longTermPricingEntries) (\s@ListLongTermPricingResponse' {} a -> s {longTermPricingEntries = a} :: ListLongTermPricingResponse) Prelude.. Lens.mapping Lens.coerced

-- | Because HTTP requests are stateless, this is the starting point for your
-- next list of returned @ListLongTermPricing@ list.
listLongTermPricingResponse_nextToken :: Lens.Lens' ListLongTermPricingResponse (Prelude.Maybe Prelude.Text)
listLongTermPricingResponse_nextToken = Lens.lens (\ListLongTermPricingResponse' {nextToken} -> nextToken) (\s@ListLongTermPricingResponse' {} a -> s {nextToken = a} :: ListLongTermPricingResponse)

-- | The response's http status code.
listLongTermPricingResponse_httpStatus :: Lens.Lens' ListLongTermPricingResponse Prelude.Int
listLongTermPricingResponse_httpStatus = Lens.lens (\ListLongTermPricingResponse' {httpStatus} -> httpStatus) (\s@ListLongTermPricingResponse' {} a -> s {httpStatus = a} :: ListLongTermPricingResponse)

instance Prelude.NFData ListLongTermPricingResponse where
  rnf ListLongTermPricingResponse' {..} =
    Prelude.rnf longTermPricingEntries
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
