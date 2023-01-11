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
-- Module      : Amazonka.GlobalAccelerator.ListCustomRoutingAccelerators
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List the custom routing accelerators for an Amazon Web Services account.
--
-- This operation returns paginated results.
module Amazonka.GlobalAccelerator.ListCustomRoutingAccelerators
  ( -- * Creating a Request
    ListCustomRoutingAccelerators (..),
    newListCustomRoutingAccelerators,

    -- * Request Lenses
    listCustomRoutingAccelerators_maxResults,
    listCustomRoutingAccelerators_nextToken,

    -- * Destructuring the Response
    ListCustomRoutingAcceleratorsResponse (..),
    newListCustomRoutingAcceleratorsResponse,

    -- * Response Lenses
    listCustomRoutingAcceleratorsResponse_accelerators,
    listCustomRoutingAcceleratorsResponse_nextToken,
    listCustomRoutingAcceleratorsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GlobalAccelerator.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListCustomRoutingAccelerators' smart constructor.
data ListCustomRoutingAccelerators = ListCustomRoutingAccelerators'
  { -- | The number of custom routing Global Accelerator objects that you want to
    -- return with this call. The default value is 10.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next set of results. You receive this token from a
    -- previous call.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListCustomRoutingAccelerators' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listCustomRoutingAccelerators_maxResults' - The number of custom routing Global Accelerator objects that you want to
-- return with this call. The default value is 10.
--
-- 'nextToken', 'listCustomRoutingAccelerators_nextToken' - The token for the next set of results. You receive this token from a
-- previous call.
newListCustomRoutingAccelerators ::
  ListCustomRoutingAccelerators
newListCustomRoutingAccelerators =
  ListCustomRoutingAccelerators'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The number of custom routing Global Accelerator objects that you want to
-- return with this call. The default value is 10.
listCustomRoutingAccelerators_maxResults :: Lens.Lens' ListCustomRoutingAccelerators (Prelude.Maybe Prelude.Natural)
listCustomRoutingAccelerators_maxResults = Lens.lens (\ListCustomRoutingAccelerators' {maxResults} -> maxResults) (\s@ListCustomRoutingAccelerators' {} a -> s {maxResults = a} :: ListCustomRoutingAccelerators)

-- | The token for the next set of results. You receive this token from a
-- previous call.
listCustomRoutingAccelerators_nextToken :: Lens.Lens' ListCustomRoutingAccelerators (Prelude.Maybe Prelude.Text)
listCustomRoutingAccelerators_nextToken = Lens.lens (\ListCustomRoutingAccelerators' {nextToken} -> nextToken) (\s@ListCustomRoutingAccelerators' {} a -> s {nextToken = a} :: ListCustomRoutingAccelerators)

instance Core.AWSPager ListCustomRoutingAccelerators where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listCustomRoutingAcceleratorsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listCustomRoutingAcceleratorsResponse_accelerators
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listCustomRoutingAccelerators_nextToken
          Lens..~ rs
          Lens.^? listCustomRoutingAcceleratorsResponse_nextToken
            Prelude.. Lens._Just

instance
  Core.AWSRequest
    ListCustomRoutingAccelerators
  where
  type
    AWSResponse ListCustomRoutingAccelerators =
      ListCustomRoutingAcceleratorsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListCustomRoutingAcceleratorsResponse'
            Prelude.<$> (x Data..?> "Accelerators" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListCustomRoutingAccelerators
  where
  hashWithSalt _salt ListCustomRoutingAccelerators' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListCustomRoutingAccelerators where
  rnf ListCustomRoutingAccelerators' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListCustomRoutingAccelerators where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "GlobalAccelerator_V20180706.ListCustomRoutingAccelerators" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListCustomRoutingAccelerators where
  toJSON ListCustomRoutingAccelerators' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListCustomRoutingAccelerators where
  toPath = Prelude.const "/"

instance Data.ToQuery ListCustomRoutingAccelerators where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListCustomRoutingAcceleratorsResponse' smart constructor.
data ListCustomRoutingAcceleratorsResponse = ListCustomRoutingAcceleratorsResponse'
  { -- | The list of custom routing accelerators for a customer account.
    accelerators :: Prelude.Maybe [CustomRoutingAccelerator],
    -- | The token for the next set of results. You receive this token from a
    -- previous call.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListCustomRoutingAcceleratorsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accelerators', 'listCustomRoutingAcceleratorsResponse_accelerators' - The list of custom routing accelerators for a customer account.
--
-- 'nextToken', 'listCustomRoutingAcceleratorsResponse_nextToken' - The token for the next set of results. You receive this token from a
-- previous call.
--
-- 'httpStatus', 'listCustomRoutingAcceleratorsResponse_httpStatus' - The response's http status code.
newListCustomRoutingAcceleratorsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListCustomRoutingAcceleratorsResponse
newListCustomRoutingAcceleratorsResponse pHttpStatus_ =
  ListCustomRoutingAcceleratorsResponse'
    { accelerators =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The list of custom routing accelerators for a customer account.
listCustomRoutingAcceleratorsResponse_accelerators :: Lens.Lens' ListCustomRoutingAcceleratorsResponse (Prelude.Maybe [CustomRoutingAccelerator])
listCustomRoutingAcceleratorsResponse_accelerators = Lens.lens (\ListCustomRoutingAcceleratorsResponse' {accelerators} -> accelerators) (\s@ListCustomRoutingAcceleratorsResponse' {} a -> s {accelerators = a} :: ListCustomRoutingAcceleratorsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token for the next set of results. You receive this token from a
-- previous call.
listCustomRoutingAcceleratorsResponse_nextToken :: Lens.Lens' ListCustomRoutingAcceleratorsResponse (Prelude.Maybe Prelude.Text)
listCustomRoutingAcceleratorsResponse_nextToken = Lens.lens (\ListCustomRoutingAcceleratorsResponse' {nextToken} -> nextToken) (\s@ListCustomRoutingAcceleratorsResponse' {} a -> s {nextToken = a} :: ListCustomRoutingAcceleratorsResponse)

-- | The response's http status code.
listCustomRoutingAcceleratorsResponse_httpStatus :: Lens.Lens' ListCustomRoutingAcceleratorsResponse Prelude.Int
listCustomRoutingAcceleratorsResponse_httpStatus = Lens.lens (\ListCustomRoutingAcceleratorsResponse' {httpStatus} -> httpStatus) (\s@ListCustomRoutingAcceleratorsResponse' {} a -> s {httpStatus = a} :: ListCustomRoutingAcceleratorsResponse)

instance
  Prelude.NFData
    ListCustomRoutingAcceleratorsResponse
  where
  rnf ListCustomRoutingAcceleratorsResponse' {..} =
    Prelude.rnf accelerators
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
