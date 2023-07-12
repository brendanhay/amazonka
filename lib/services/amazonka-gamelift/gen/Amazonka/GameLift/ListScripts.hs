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
-- Module      : Amazonka.GameLift.ListScripts
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves script records for all Realtime scripts that are associated
-- with the Amazon Web Services account in use.
--
-- __Learn more__
--
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/realtime-intro.html Amazon GameLift Realtime Servers>
--
-- __Related actions__
--
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/reference-awssdk.html#reference-awssdk-resources-fleets All APIs by task>
--
-- This operation returns paginated results.
module Amazonka.GameLift.ListScripts
  ( -- * Creating a Request
    ListScripts (..),
    newListScripts,

    -- * Request Lenses
    listScripts_limit,
    listScripts_nextToken,

    -- * Destructuring the Response
    ListScriptsResponse (..),
    newListScriptsResponse,

    -- * Response Lenses
    listScriptsResponse_nextToken,
    listScriptsResponse_scripts,
    listScriptsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GameLift.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListScripts' smart constructor.
data ListScripts = ListScripts'
  { -- | The maximum number of results to return. Use this parameter with
    -- @NextToken@ to get results as a set of sequential pages.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | A token that indicates the start of the next sequential page of results.
    -- Use the token that is returned with a previous call to this operation.
    -- To start at the beginning of the result set, do not specify a value.
    nextToken :: Prelude.Maybe Prelude.Text
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
-- 'limit', 'listScripts_limit' - The maximum number of results to return. Use this parameter with
-- @NextToken@ to get results as a set of sequential pages.
--
-- 'nextToken', 'listScripts_nextToken' - A token that indicates the start of the next sequential page of results.
-- Use the token that is returned with a previous call to this operation.
-- To start at the beginning of the result set, do not specify a value.
newListScripts ::
  ListScripts
newListScripts =
  ListScripts'
    { limit = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The maximum number of results to return. Use this parameter with
-- @NextToken@ to get results as a set of sequential pages.
listScripts_limit :: Lens.Lens' ListScripts (Prelude.Maybe Prelude.Natural)
listScripts_limit = Lens.lens (\ListScripts' {limit} -> limit) (\s@ListScripts' {} a -> s {limit = a} :: ListScripts)

-- | A token that indicates the start of the next sequential page of results.
-- Use the token that is returned with a previous call to this operation.
-- To start at the beginning of the result set, do not specify a value.
listScripts_nextToken :: Lens.Lens' ListScripts (Prelude.Maybe Prelude.Text)
listScripts_nextToken = Lens.lens (\ListScripts' {nextToken} -> nextToken) (\s@ListScripts' {} a -> s {nextToken = a} :: ListScripts)

instance Core.AWSPager ListScripts where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listScriptsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listScriptsResponse_scripts
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listScripts_nextToken
          Lens..~ rs
          Lens.^? listScriptsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListScripts where
  type AWSResponse ListScripts = ListScriptsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListScriptsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "Scripts" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListScripts where
  hashWithSalt _salt ListScripts' {..} =
    _salt
      `Prelude.hashWithSalt` limit
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListScripts where
  rnf ListScripts' {..} =
    Prelude.rnf limit
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListScripts where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("GameLift.ListScripts" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListScripts where
  toJSON ListScripts' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Limit" Data..=) Prelude.<$> limit,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListScripts where
  toPath = Prelude.const "/"

instance Data.ToQuery ListScripts where
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
listScriptsResponse_scripts = Lens.lens (\ListScriptsResponse' {scripts} -> scripts) (\s@ListScriptsResponse' {} a -> s {scripts = a} :: ListScriptsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listScriptsResponse_httpStatus :: Lens.Lens' ListScriptsResponse Prelude.Int
listScriptsResponse_httpStatus = Lens.lens (\ListScriptsResponse' {httpStatus} -> httpStatus) (\s@ListScriptsResponse' {} a -> s {httpStatus = a} :: ListScriptsResponse)

instance Prelude.NFData ListScriptsResponse where
  rnf ListScriptsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf scripts
      `Prelude.seq` Prelude.rnf httpStatus
