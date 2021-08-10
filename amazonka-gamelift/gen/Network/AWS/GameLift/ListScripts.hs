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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListScripts' smart constructor.
data ListScripts = ListScripts'
  { -- | A token that indicates the start of the next sequential page of results.
    -- Use the token that is returned with a previous call to this operation.
    -- To start at the beginning of the result set, do not specify a value.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return. Use this parameter with
    -- @NextToken@ to get results as a set of sequential pages.
    limit :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
    { nextToken = Prelude.Nothing,
      limit = Prelude.Nothing
    }

-- | A token that indicates the start of the next sequential page of results.
-- Use the token that is returned with a previous call to this operation.
-- To start at the beginning of the result set, do not specify a value.
listScripts_nextToken :: Lens.Lens' ListScripts (Prelude.Maybe Prelude.Text)
listScripts_nextToken = Lens.lens (\ListScripts' {nextToken} -> nextToken) (\s@ListScripts' {} a -> s {nextToken = a} :: ListScripts)

-- | The maximum number of results to return. Use this parameter with
-- @NextToken@ to get results as a set of sequential pages.
listScripts_limit :: Lens.Lens' ListScripts (Prelude.Maybe Prelude.Natural)
listScripts_limit = Lens.lens (\ListScripts' {limit} -> limit) (\s@ListScripts' {} a -> s {limit = a} :: ListScripts)

instance Core.AWSPager ListScripts where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listScriptsResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listScriptsResponse_scripts Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listScripts_nextToken
          Lens..~ rs
          Lens.^? listScriptsResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListScripts where
  type AWSResponse ListScripts = ListScriptsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListScriptsResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "Scripts" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListScripts

instance Prelude.NFData ListScripts

instance Core.ToHeaders ListScripts where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("GameLift.ListScripts" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListScripts where
  toJSON ListScripts' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("Limit" Core..=) Prelude.<$> limit
          ]
      )

instance Core.ToPath ListScripts where
  toPath = Prelude.const "/"

instance Core.ToQuery ListScripts where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListScriptsResponse' smart constructor.
data ListScriptsResponse = ListScriptsResponse'
  { -- | A token that indicates where to resume retrieving results on the next
    -- call to this operation. If no token is returned, these results represent
    -- the end of the list.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A set of properties describing the requested script.
    scripts :: Prelude.Maybe [Script],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  ListScriptsResponse
newListScriptsResponse pHttpStatus_ =
  ListScriptsResponse'
    { nextToken = Prelude.Nothing,
      scripts = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A token that indicates where to resume retrieving results on the next
-- call to this operation. If no token is returned, these results represent
-- the end of the list.
listScriptsResponse_nextToken :: Lens.Lens' ListScriptsResponse (Prelude.Maybe Prelude.Text)
listScriptsResponse_nextToken = Lens.lens (\ListScriptsResponse' {nextToken} -> nextToken) (\s@ListScriptsResponse' {} a -> s {nextToken = a} :: ListScriptsResponse)

-- | A set of properties describing the requested script.
listScriptsResponse_scripts :: Lens.Lens' ListScriptsResponse (Prelude.Maybe [Script])
listScriptsResponse_scripts = Lens.lens (\ListScriptsResponse' {scripts} -> scripts) (\s@ListScriptsResponse' {} a -> s {scripts = a} :: ListScriptsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listScriptsResponse_httpStatus :: Lens.Lens' ListScriptsResponse Prelude.Int
listScriptsResponse_httpStatus = Lens.lens (\ListScriptsResponse' {httpStatus} -> httpStatus) (\s@ListScriptsResponse' {} a -> s {httpStatus = a} :: ListScriptsResponse)

instance Prelude.NFData ListScriptsResponse
