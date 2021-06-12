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
-- Module      : Network.AWS.CodeDeploy.ListGitHubAccountTokenNames
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the names of stored connections to GitHub accounts.
--
-- This operation returns paginated results.
module Network.AWS.CodeDeploy.ListGitHubAccountTokenNames
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

import Network.AWS.CodeDeploy.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @ListGitHubAccountTokenNames@ operation.
--
-- /See:/ 'newListGitHubAccountTokenNames' smart constructor.
data ListGitHubAccountTokenNames = ListGitHubAccountTokenNames'
  { -- | An identifier returned from the previous @ListGitHubAccountTokenNames@
    -- call. It can be used to return the next set of names in the list.
    nextToken :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
        Core.Nothing
    }

-- | An identifier returned from the previous @ListGitHubAccountTokenNames@
-- call. It can be used to return the next set of names in the list.
listGitHubAccountTokenNames_nextToken :: Lens.Lens' ListGitHubAccountTokenNames (Core.Maybe Core.Text)
listGitHubAccountTokenNames_nextToken = Lens.lens (\ListGitHubAccountTokenNames' {nextToken} -> nextToken) (\s@ListGitHubAccountTokenNames' {} a -> s {nextToken = a} :: ListGitHubAccountTokenNames)

instance Core.AWSPager ListGitHubAccountTokenNames where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listGitHubAccountTokenNamesResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listGitHubAccountTokenNamesResponse_tokenNameList
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listGitHubAccountTokenNames_nextToken
          Lens..~ rs
          Lens.^? listGitHubAccountTokenNamesResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest ListGitHubAccountTokenNames where
  type
    AWSResponse ListGitHubAccountTokenNames =
      ListGitHubAccountTokenNamesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListGitHubAccountTokenNamesResponse'
            Core.<$> (x Core..?> "nextToken")
            Core.<*> (x Core..?> "tokenNameList" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListGitHubAccountTokenNames

instance Core.NFData ListGitHubAccountTokenNames

instance Core.ToHeaders ListGitHubAccountTokenNames where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeDeploy_20141006.ListGitHubAccountTokenNames" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListGitHubAccountTokenNames where
  toJSON ListGitHubAccountTokenNames' {..} =
    Core.object
      ( Core.catMaybes
          [("nextToken" Core..=) Core.<$> nextToken]
      )

instance Core.ToPath ListGitHubAccountTokenNames where
  toPath = Core.const "/"

instance Core.ToQuery ListGitHubAccountTokenNames where
  toQuery = Core.const Core.mempty

-- | Represents the output of a @ListGitHubAccountTokenNames@ operation.
--
-- /See:/ 'newListGitHubAccountTokenNamesResponse' smart constructor.
data ListGitHubAccountTokenNamesResponse = ListGitHubAccountTokenNamesResponse'
  { -- | If a large amount of information is returned, an identifier is also
    -- returned. It can be used in a subsequent @ListGitHubAccountTokenNames@
    -- call to return the next set of names in the list.
    nextToken :: Core.Maybe Core.Text,
    -- | A list of names of connections to GitHub accounts.
    tokenNameList :: Core.Maybe [Core.Text],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  ListGitHubAccountTokenNamesResponse
newListGitHubAccountTokenNamesResponse pHttpStatus_ =
  ListGitHubAccountTokenNamesResponse'
    { nextToken =
        Core.Nothing,
      tokenNameList = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If a large amount of information is returned, an identifier is also
-- returned. It can be used in a subsequent @ListGitHubAccountTokenNames@
-- call to return the next set of names in the list.
listGitHubAccountTokenNamesResponse_nextToken :: Lens.Lens' ListGitHubAccountTokenNamesResponse (Core.Maybe Core.Text)
listGitHubAccountTokenNamesResponse_nextToken = Lens.lens (\ListGitHubAccountTokenNamesResponse' {nextToken} -> nextToken) (\s@ListGitHubAccountTokenNamesResponse' {} a -> s {nextToken = a} :: ListGitHubAccountTokenNamesResponse)

-- | A list of names of connections to GitHub accounts.
listGitHubAccountTokenNamesResponse_tokenNameList :: Lens.Lens' ListGitHubAccountTokenNamesResponse (Core.Maybe [Core.Text])
listGitHubAccountTokenNamesResponse_tokenNameList = Lens.lens (\ListGitHubAccountTokenNamesResponse' {tokenNameList} -> tokenNameList) (\s@ListGitHubAccountTokenNamesResponse' {} a -> s {tokenNameList = a} :: ListGitHubAccountTokenNamesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listGitHubAccountTokenNamesResponse_httpStatus :: Lens.Lens' ListGitHubAccountTokenNamesResponse Core.Int
listGitHubAccountTokenNamesResponse_httpStatus = Lens.lens (\ListGitHubAccountTokenNamesResponse' {httpStatus} -> httpStatus) (\s@ListGitHubAccountTokenNamesResponse' {} a -> s {httpStatus = a} :: ListGitHubAccountTokenNamesResponse)

instance
  Core.NFData
    ListGitHubAccountTokenNamesResponse
