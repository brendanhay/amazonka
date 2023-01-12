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
-- Module      : Amazonka.GlobalAccelerator.ListAccelerators
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List the accelerators for an Amazon Web Services account.
--
-- This operation returns paginated results.
module Amazonka.GlobalAccelerator.ListAccelerators
  ( -- * Creating a Request
    ListAccelerators (..),
    newListAccelerators,

    -- * Request Lenses
    listAccelerators_maxResults,
    listAccelerators_nextToken,

    -- * Destructuring the Response
    ListAcceleratorsResponse (..),
    newListAcceleratorsResponse,

    -- * Response Lenses
    listAcceleratorsResponse_accelerators,
    listAcceleratorsResponse_nextToken,
    listAcceleratorsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GlobalAccelerator.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListAccelerators' smart constructor.
data ListAccelerators = ListAccelerators'
  { -- | The number of Global Accelerator objects that you want to return with
    -- this call. The default value is 10.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next set of results. You receive this token from a
    -- previous call.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAccelerators' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listAccelerators_maxResults' - The number of Global Accelerator objects that you want to return with
-- this call. The default value is 10.
--
-- 'nextToken', 'listAccelerators_nextToken' - The token for the next set of results. You receive this token from a
-- previous call.
newListAccelerators ::
  ListAccelerators
newListAccelerators =
  ListAccelerators'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The number of Global Accelerator objects that you want to return with
-- this call. The default value is 10.
listAccelerators_maxResults :: Lens.Lens' ListAccelerators (Prelude.Maybe Prelude.Natural)
listAccelerators_maxResults = Lens.lens (\ListAccelerators' {maxResults} -> maxResults) (\s@ListAccelerators' {} a -> s {maxResults = a} :: ListAccelerators)

-- | The token for the next set of results. You receive this token from a
-- previous call.
listAccelerators_nextToken :: Lens.Lens' ListAccelerators (Prelude.Maybe Prelude.Text)
listAccelerators_nextToken = Lens.lens (\ListAccelerators' {nextToken} -> nextToken) (\s@ListAccelerators' {} a -> s {nextToken = a} :: ListAccelerators)

instance Core.AWSPager ListAccelerators where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listAcceleratorsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listAcceleratorsResponse_accelerators
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listAccelerators_nextToken
          Lens..~ rs
          Lens.^? listAcceleratorsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListAccelerators where
  type
    AWSResponse ListAccelerators =
      ListAcceleratorsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAcceleratorsResponse'
            Prelude.<$> (x Data..?> "Accelerators" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListAccelerators where
  hashWithSalt _salt ListAccelerators' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListAccelerators where
  rnf ListAccelerators' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListAccelerators where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "GlobalAccelerator_V20180706.ListAccelerators" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListAccelerators where
  toJSON ListAccelerators' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListAccelerators where
  toPath = Prelude.const "/"

instance Data.ToQuery ListAccelerators where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListAcceleratorsResponse' smart constructor.
data ListAcceleratorsResponse = ListAcceleratorsResponse'
  { -- | The list of accelerators for a customer account.
    accelerators :: Prelude.Maybe [Accelerator],
    -- | The token for the next set of results. You receive this token from a
    -- previous call.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAcceleratorsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accelerators', 'listAcceleratorsResponse_accelerators' - The list of accelerators for a customer account.
--
-- 'nextToken', 'listAcceleratorsResponse_nextToken' - The token for the next set of results. You receive this token from a
-- previous call.
--
-- 'httpStatus', 'listAcceleratorsResponse_httpStatus' - The response's http status code.
newListAcceleratorsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListAcceleratorsResponse
newListAcceleratorsResponse pHttpStatus_ =
  ListAcceleratorsResponse'
    { accelerators =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The list of accelerators for a customer account.
listAcceleratorsResponse_accelerators :: Lens.Lens' ListAcceleratorsResponse (Prelude.Maybe [Accelerator])
listAcceleratorsResponse_accelerators = Lens.lens (\ListAcceleratorsResponse' {accelerators} -> accelerators) (\s@ListAcceleratorsResponse' {} a -> s {accelerators = a} :: ListAcceleratorsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token for the next set of results. You receive this token from a
-- previous call.
listAcceleratorsResponse_nextToken :: Lens.Lens' ListAcceleratorsResponse (Prelude.Maybe Prelude.Text)
listAcceleratorsResponse_nextToken = Lens.lens (\ListAcceleratorsResponse' {nextToken} -> nextToken) (\s@ListAcceleratorsResponse' {} a -> s {nextToken = a} :: ListAcceleratorsResponse)

-- | The response's http status code.
listAcceleratorsResponse_httpStatus :: Lens.Lens' ListAcceleratorsResponse Prelude.Int
listAcceleratorsResponse_httpStatus = Lens.lens (\ListAcceleratorsResponse' {httpStatus} -> httpStatus) (\s@ListAcceleratorsResponse' {} a -> s {httpStatus = a} :: ListAcceleratorsResponse)

instance Prelude.NFData ListAcceleratorsResponse where
  rnf ListAcceleratorsResponse' {..} =
    Prelude.rnf accelerators
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
