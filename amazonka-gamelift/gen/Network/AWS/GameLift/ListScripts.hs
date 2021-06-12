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
-- Module      : Network.AWS.GameLift.ListScripts
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves script records for all Realtime scripts that are associated
-- with the AWS account in use.
--
-- __Learn more__
--
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/realtime-intro.html Amazon GameLift Realtime Servers>
--
-- __Related operations__
--
-- -   CreateScript
--
-- -   ListScripts
--
-- -   DescribeScript
--
-- -   UpdateScript
--
-- -   DeleteScript
--
-- This operation returns paginated results.
module Network.AWS.GameLift.ListScripts
  ( -- * Creating a Request
    ListScripts (..),
    newListScripts,

    -- * Request Lenses
    listScripts_nextToken,
    listScripts_limit,

    -- * Destructuring the Response
    ListScriptsResponse (..),
    newListScriptsResponse,

    -- * Response Lenses
    listScriptsResponse_nextToken,
    listScriptsResponse_scripts,
    listScriptsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListScripts' smart constructor.
data ListScripts = ListScripts'
  { -- | A token that indicates the start of the next sequential page of results.
    -- Use the token that is returned with a previous call to this operation.
    -- To start at the beginning of the result set, do not specify a value.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of results to return. Use this parameter with
    -- @NextToken@ to get results as a set of sequential pages.
    limit :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListScripts' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listScripts_nextToken' - A token that indicates the start of the next sequential page of results.
-- Use the token that is returned with a previous call to this operation.
-- To start at the beginning of the result set, do not specify a value.
--
-- 'limit', 'listScripts_limit' - The maximum number of results to return. Use this parameter with
-- @NextToken@ to get results as a set of sequential pages.
newListScripts ::
  ListScripts
newListScripts =
  ListScripts'
    { nextToken = Core.Nothing,
      limit = Core.Nothing
    }

-- | A token that indicates the start of the next sequential page of results.
-- Use the token that is returned with a previous call to this operation.
-- To start at the beginning of the result set, do not specify a value.
listScripts_nextToken :: Lens.Lens' ListScripts (Core.Maybe Core.Text)
listScripts_nextToken = Lens.lens (\ListScripts' {nextToken} -> nextToken) (\s@ListScripts' {} a -> s {nextToken = a} :: ListScripts)

-- | The maximum number of results to return. Use this parameter with
-- @NextToken@ to get results as a set of sequential pages.
listScripts_limit :: Lens.Lens' ListScripts (Core.Maybe Core.Natural)
listScripts_limit = Lens.lens (\ListScripts' {limit} -> limit) (\s@ListScripts' {} a -> s {limit = a} :: ListScripts)

instance Core.AWSPager ListScripts where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listScriptsResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listScriptsResponse_scripts Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listScripts_nextToken
          Lens..~ rs
          Lens.^? listScriptsResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest ListScripts where
  type AWSResponse ListScripts = ListScriptsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListScriptsResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "Scripts" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListScripts

instance Core.NFData ListScripts

instance Core.ToHeaders ListScripts where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("GameLift.ListScripts" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListScripts where
  toJSON ListScripts' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("Limit" Core..=) Core.<$> limit
          ]
      )

instance Core.ToPath ListScripts where
  toPath = Core.const "/"

instance Core.ToQuery ListScripts where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListScriptsResponse' smart constructor.
data ListScriptsResponse = ListScriptsResponse'
  { -- | A token that indicates where to resume retrieving results on the next
    -- call to this operation. If no token is returned, these results represent
    -- the end of the list.
    nextToken :: Core.Maybe Core.Text,
    -- | A set of properties describing the requested script.
    scripts :: Core.Maybe [Script],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListScriptsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listScriptsResponse_nextToken' - A token that indicates where to resume retrieving results on the next
-- call to this operation. If no token is returned, these results represent
-- the end of the list.
--
-- 'scripts', 'listScriptsResponse_scripts' - A set of properties describing the requested script.
--
-- 'httpStatus', 'listScriptsResponse_httpStatus' - The response's http status code.
newListScriptsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListScriptsResponse
newListScriptsResponse pHttpStatus_ =
  ListScriptsResponse'
    { nextToken = Core.Nothing,
      scripts = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A token that indicates where to resume retrieving results on the next
-- call to this operation. If no token is returned, these results represent
-- the end of the list.
listScriptsResponse_nextToken :: Lens.Lens' ListScriptsResponse (Core.Maybe Core.Text)
listScriptsResponse_nextToken = Lens.lens (\ListScriptsResponse' {nextToken} -> nextToken) (\s@ListScriptsResponse' {} a -> s {nextToken = a} :: ListScriptsResponse)

-- | A set of properties describing the requested script.
listScriptsResponse_scripts :: Lens.Lens' ListScriptsResponse (Core.Maybe [Script])
listScriptsResponse_scripts = Lens.lens (\ListScriptsResponse' {scripts} -> scripts) (\s@ListScriptsResponse' {} a -> s {scripts = a} :: ListScriptsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listScriptsResponse_httpStatus :: Lens.Lens' ListScriptsResponse Core.Int
listScriptsResponse_httpStatus = Lens.lens (\ListScriptsResponse' {httpStatus} -> httpStatus) (\s@ListScriptsResponse' {} a -> s {httpStatus = a} :: ListScriptsResponse)

instance Core.NFData ListScriptsResponse
