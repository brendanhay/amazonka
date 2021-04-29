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
-- Module      : Network.AWS.MediaLive.ListInputs
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Produces list of inputs that have been created
--
-- This operation returns paginated results.
module Network.AWS.MediaLive.ListInputs
  ( -- * Creating a Request
    ListInputs (..),
    newListInputs,

    -- * Request Lenses
    listInputs_nextToken,
    listInputs_maxResults,

    -- * Destructuring the Response
    ListInputsResponse (..),
    newListInputsResponse,

    -- * Response Lenses
    listInputsResponse_nextToken,
    listInputsResponse_inputs,
    listInputsResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Placeholder documentation for ListInputsRequest
--
-- /See:/ 'newListInputs' smart constructor.
data ListInputs = ListInputs'
  { nextToken :: Prelude.Maybe Prelude.Text,
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ListInputs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listInputs_nextToken' - Undocumented member.
--
-- 'maxResults', 'listInputs_maxResults' - Undocumented member.
newListInputs ::
  ListInputs
newListInputs =
  ListInputs'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | Undocumented member.
listInputs_nextToken :: Lens.Lens' ListInputs (Prelude.Maybe Prelude.Text)
listInputs_nextToken = Lens.lens (\ListInputs' {nextToken} -> nextToken) (\s@ListInputs' {} a -> s {nextToken = a} :: ListInputs)

-- | Undocumented member.
listInputs_maxResults :: Lens.Lens' ListInputs (Prelude.Maybe Prelude.Natural)
listInputs_maxResults = Lens.lens (\ListInputs' {maxResults} -> maxResults) (\s@ListInputs' {} a -> s {maxResults = a} :: ListInputs)

instance Pager.AWSPager ListInputs where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? listInputsResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? listInputsResponse_inputs Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& listInputs_nextToken
          Lens..~ rs
          Lens.^? listInputsResponse_nextToken Prelude.. Lens._Just

instance Prelude.AWSRequest ListInputs where
  type Rs ListInputs = ListInputsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListInputsResponse'
            Prelude.<$> (x Prelude..?> "nextToken")
            Prelude.<*> (x Prelude..?> "inputs" Prelude..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListInputs

instance Prelude.NFData ListInputs

instance Prelude.ToHeaders ListInputs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToPath ListInputs where
  toPath = Prelude.const "/prod/inputs"

instance Prelude.ToQuery ListInputs where
  toQuery ListInputs' {..} =
    Prelude.mconcat
      [ "nextToken" Prelude.=: nextToken,
        "maxResults" Prelude.=: maxResults
      ]

-- | Placeholder documentation for ListInputsResponse
--
-- /See:/ 'newListInputsResponse' smart constructor.
data ListInputsResponse = ListInputsResponse'
  { nextToken :: Prelude.Maybe Prelude.Text,
    inputs :: Prelude.Maybe [Input],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ListInputsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listInputsResponse_nextToken' - Undocumented member.
--
-- 'inputs', 'listInputsResponse_inputs' - Undocumented member.
--
-- 'httpStatus', 'listInputsResponse_httpStatus' - The response's http status code.
newListInputsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListInputsResponse
newListInputsResponse pHttpStatus_ =
  ListInputsResponse'
    { nextToken = Prelude.Nothing,
      inputs = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
listInputsResponse_nextToken :: Lens.Lens' ListInputsResponse (Prelude.Maybe Prelude.Text)
listInputsResponse_nextToken = Lens.lens (\ListInputsResponse' {nextToken} -> nextToken) (\s@ListInputsResponse' {} a -> s {nextToken = a} :: ListInputsResponse)

-- | Undocumented member.
listInputsResponse_inputs :: Lens.Lens' ListInputsResponse (Prelude.Maybe [Input])
listInputsResponse_inputs = Lens.lens (\ListInputsResponse' {inputs} -> inputs) (\s@ListInputsResponse' {} a -> s {inputs = a} :: ListInputsResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
listInputsResponse_httpStatus :: Lens.Lens' ListInputsResponse Prelude.Int
listInputsResponse_httpStatus = Lens.lens (\ListInputsResponse' {httpStatus} -> httpStatus) (\s@ListInputsResponse' {} a -> s {httpStatus = a} :: ListInputsResponse)

instance Prelude.NFData ListInputsResponse
