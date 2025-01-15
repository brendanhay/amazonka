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
-- Module      : Amazonka.MediaLive.ListInputs
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Produces list of inputs that have been created
--
-- This operation returns paginated results.
module Amazonka.MediaLive.ListInputs
  ( -- * Creating a Request
    ListInputs (..),
    newListInputs,

    -- * Request Lenses
    listInputs_maxResults,
    listInputs_nextToken,

    -- * Destructuring the Response
    ListInputsResponse (..),
    newListInputsResponse,

    -- * Response Lenses
    listInputsResponse_inputs,
    listInputsResponse_nextToken,
    listInputsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaLive.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Placeholder documentation for ListInputsRequest
--
-- /See:/ 'newListInputs' smart constructor.
data ListInputs = ListInputs'
  { maxResults :: Prelude.Maybe Prelude.Natural,
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListInputs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listInputs_maxResults' - Undocumented member.
--
-- 'nextToken', 'listInputs_nextToken' - Undocumented member.
newListInputs ::
  ListInputs
newListInputs =
  ListInputs'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | Undocumented member.
listInputs_maxResults :: Lens.Lens' ListInputs (Prelude.Maybe Prelude.Natural)
listInputs_maxResults = Lens.lens (\ListInputs' {maxResults} -> maxResults) (\s@ListInputs' {} a -> s {maxResults = a} :: ListInputs)

-- | Undocumented member.
listInputs_nextToken :: Lens.Lens' ListInputs (Prelude.Maybe Prelude.Text)
listInputs_nextToken = Lens.lens (\ListInputs' {nextToken} -> nextToken) (\s@ListInputs' {} a -> s {nextToken = a} :: ListInputs)

instance Core.AWSPager ListInputs where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listInputsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listInputsResponse_inputs
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& listInputs_nextToken
              Lens..~ rs
              Lens.^? listInputsResponse_nextToken
              Prelude.. Lens._Just

instance Core.AWSRequest ListInputs where
  type AWSResponse ListInputs = ListInputsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListInputsResponse'
            Prelude.<$> (x Data..?> "inputs" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListInputs where
  hashWithSalt _salt ListInputs' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListInputs where
  rnf ListInputs' {..} =
    Prelude.rnf maxResults `Prelude.seq`
      Prelude.rnf nextToken

instance Data.ToHeaders ListInputs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListInputs where
  toPath = Prelude.const "/prod/inputs"

instance Data.ToQuery ListInputs where
  toQuery ListInputs' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | Placeholder documentation for ListInputsResponse
--
-- /See:/ 'newListInputsResponse' smart constructor.
data ListInputsResponse = ListInputsResponse'
  { inputs :: Prelude.Maybe [Input],
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListInputsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'inputs', 'listInputsResponse_inputs' - Undocumented member.
--
-- 'nextToken', 'listInputsResponse_nextToken' - Undocumented member.
--
-- 'httpStatus', 'listInputsResponse_httpStatus' - The response's http status code.
newListInputsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListInputsResponse
newListInputsResponse pHttpStatus_ =
  ListInputsResponse'
    { inputs = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
listInputsResponse_inputs :: Lens.Lens' ListInputsResponse (Prelude.Maybe [Input])
listInputsResponse_inputs = Lens.lens (\ListInputsResponse' {inputs} -> inputs) (\s@ListInputsResponse' {} a -> s {inputs = a} :: ListInputsResponse) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
listInputsResponse_nextToken :: Lens.Lens' ListInputsResponse (Prelude.Maybe Prelude.Text)
listInputsResponse_nextToken = Lens.lens (\ListInputsResponse' {nextToken} -> nextToken) (\s@ListInputsResponse' {} a -> s {nextToken = a} :: ListInputsResponse)

-- | The response's http status code.
listInputsResponse_httpStatus :: Lens.Lens' ListInputsResponse Prelude.Int
listInputsResponse_httpStatus = Lens.lens (\ListInputsResponse' {httpStatus} -> httpStatus) (\s@ListInputsResponse' {} a -> s {httpStatus = a} :: ListInputsResponse)

instance Prelude.NFData ListInputsResponse where
  rnf ListInputsResponse' {..} =
    Prelude.rnf inputs `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf httpStatus
