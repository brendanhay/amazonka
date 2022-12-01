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
-- Module      : Amazonka.CodeDeploy.ListGitHubAccountTokenNames
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the names of stored connections to GitHub accounts.
--
-- This operation returns paginated results.
module Amazonka.CodeDeploy.ListGitHubAccountTokenNames
  ( -- * Creating a Request
    ListGitHubAccountTokenNames (..),
    newListGitHubAccountTokenNames,

    -- * Request Lenses
    listGitHubAccountTokenNames_nextToken,

    -- * Destructuring the Response
    ListGitHubAccountTokenNamesResponse (..),
    newListGitHubAccountTokenNamesResponse,

    -- * Response Lenses
    listGitHubAccountTokenNamesResponse_nextToken,
    listGitHubAccountTokenNamesResponse_tokenNameList,
    listGitHubAccountTokenNamesResponse_httpStatus,
  )
where

import Amazonka.CodeDeploy.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the input of a @ListGitHubAccountTokenNames@ operation.
--
-- /See:/ 'newListGitHubAccountTokenNames' smart constructor.
data ListGitHubAccountTokenNames = ListGitHubAccountTokenNames'
  { -- | An identifier returned from the previous @ListGitHubAccountTokenNames@
    -- call. It can be used to return the next set of names in the list.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListGitHubAccountTokenNames' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listGitHubAccountTokenNames_nextToken' - An identifier returned from the previous @ListGitHubAccountTokenNames@
-- call. It can be used to return the next set of names in the list.
newListGitHubAccountTokenNames ::
  ListGitHubAccountTokenNames
newListGitHubAccountTokenNames =
  ListGitHubAccountTokenNames'
    { nextToken =
        Prelude.Nothing
    }

-- | An identifier returned from the previous @ListGitHubAccountTokenNames@
-- call. It can be used to return the next set of names in the list.
listGitHubAccountTokenNames_nextToken :: Lens.Lens' ListGitHubAccountTokenNames (Prelude.Maybe Prelude.Text)
listGitHubAccountTokenNames_nextToken = Lens.lens (\ListGitHubAccountTokenNames' {nextToken} -> nextToken) (\s@ListGitHubAccountTokenNames' {} a -> s {nextToken = a} :: ListGitHubAccountTokenNames)

instance Core.AWSPager ListGitHubAccountTokenNames where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listGitHubAccountTokenNamesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listGitHubAccountTokenNamesResponse_tokenNameList
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listGitHubAccountTokenNames_nextToken
          Lens..~ rs
          Lens.^? listGitHubAccountTokenNamesResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListGitHubAccountTokenNames where
  type
    AWSResponse ListGitHubAccountTokenNames =
      ListGitHubAccountTokenNamesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListGitHubAccountTokenNamesResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> (x Core..?> "tokenNameList" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListGitHubAccountTokenNames where
  hashWithSalt _salt ListGitHubAccountTokenNames' {..} =
    _salt `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListGitHubAccountTokenNames where
  rnf ListGitHubAccountTokenNames' {..} =
    Prelude.rnf nextToken

instance Core.ToHeaders ListGitHubAccountTokenNames where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeDeploy_20141006.ListGitHubAccountTokenNames" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListGitHubAccountTokenNames where
  toJSON ListGitHubAccountTokenNames' {..} =
    Core.object
      ( Prelude.catMaybes
          [("nextToken" Core..=) Prelude.<$> nextToken]
      )

instance Core.ToPath ListGitHubAccountTokenNames where
  toPath = Prelude.const "/"

instance Core.ToQuery ListGitHubAccountTokenNames where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the output of a @ListGitHubAccountTokenNames@ operation.
--
-- /See:/ 'newListGitHubAccountTokenNamesResponse' smart constructor.
data ListGitHubAccountTokenNamesResponse = ListGitHubAccountTokenNamesResponse'
  { -- | If a large amount of information is returned, an identifier is also
    -- returned. It can be used in a subsequent @ListGitHubAccountTokenNames@
    -- call to return the next set of names in the list.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of names of connections to GitHub accounts.
    tokenNameList :: Prelude.Maybe [Prelude.Text],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListGitHubAccountTokenNamesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listGitHubAccountTokenNamesResponse_nextToken' - If a large amount of information is returned, an identifier is also
-- returned. It can be used in a subsequent @ListGitHubAccountTokenNames@
-- call to return the next set of names in the list.
--
-- 'tokenNameList', 'listGitHubAccountTokenNamesResponse_tokenNameList' - A list of names of connections to GitHub accounts.
--
-- 'httpStatus', 'listGitHubAccountTokenNamesResponse_httpStatus' - The response's http status code.
newListGitHubAccountTokenNamesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListGitHubAccountTokenNamesResponse
newListGitHubAccountTokenNamesResponse pHttpStatus_ =
  ListGitHubAccountTokenNamesResponse'
    { nextToken =
        Prelude.Nothing,
      tokenNameList = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If a large amount of information is returned, an identifier is also
-- returned. It can be used in a subsequent @ListGitHubAccountTokenNames@
-- call to return the next set of names in the list.
listGitHubAccountTokenNamesResponse_nextToken :: Lens.Lens' ListGitHubAccountTokenNamesResponse (Prelude.Maybe Prelude.Text)
listGitHubAccountTokenNamesResponse_nextToken = Lens.lens (\ListGitHubAccountTokenNamesResponse' {nextToken} -> nextToken) (\s@ListGitHubAccountTokenNamesResponse' {} a -> s {nextToken = a} :: ListGitHubAccountTokenNamesResponse)

-- | A list of names of connections to GitHub accounts.
listGitHubAccountTokenNamesResponse_tokenNameList :: Lens.Lens' ListGitHubAccountTokenNamesResponse (Prelude.Maybe [Prelude.Text])
listGitHubAccountTokenNamesResponse_tokenNameList = Lens.lens (\ListGitHubAccountTokenNamesResponse' {tokenNameList} -> tokenNameList) (\s@ListGitHubAccountTokenNamesResponse' {} a -> s {tokenNameList = a} :: ListGitHubAccountTokenNamesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listGitHubAccountTokenNamesResponse_httpStatus :: Lens.Lens' ListGitHubAccountTokenNamesResponse Prelude.Int
listGitHubAccountTokenNamesResponse_httpStatus = Lens.lens (\ListGitHubAccountTokenNamesResponse' {httpStatus} -> httpStatus) (\s@ListGitHubAccountTokenNamesResponse' {} a -> s {httpStatus = a} :: ListGitHubAccountTokenNamesResponse)

instance
  Prelude.NFData
    ListGitHubAccountTokenNamesResponse
  where
  rnf ListGitHubAccountTokenNamesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf tokenNameList
      `Prelude.seq` Prelude.rnf httpStatus
