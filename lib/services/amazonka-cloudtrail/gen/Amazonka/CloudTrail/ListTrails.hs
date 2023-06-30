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
-- Module      : Amazonka.CloudTrail.ListTrails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists trails that are in the current account.
--
-- This operation returns paginated results.
module Amazonka.CloudTrail.ListTrails
  ( -- * Creating a Request
    ListTrails (..),
    newListTrails,

    -- * Request Lenses
    listTrails_nextToken,

    -- * Destructuring the Response
    ListTrailsResponse (..),
    newListTrailsResponse,

    -- * Response Lenses
    listTrailsResponse_nextToken,
    listTrailsResponse_trails,
    listTrailsResponse_httpStatus,
  )
where

import Amazonka.CloudTrail.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListTrails' smart constructor.
data ListTrails = ListTrails'
  { -- | The token to use to get the next page of results after a previous API
    -- call. This token must be passed in with the same parameters that were
    -- specified in the original call. For example, if the original call
    -- specified an AttributeKey of \'Username\' with a value of \'root\', the
    -- call with NextToken should include those same parameters.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTrails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listTrails_nextToken' - The token to use to get the next page of results after a previous API
-- call. This token must be passed in with the same parameters that were
-- specified in the original call. For example, if the original call
-- specified an AttributeKey of \'Username\' with a value of \'root\', the
-- call with NextToken should include those same parameters.
newListTrails ::
  ListTrails
newListTrails =
  ListTrails' {nextToken = Prelude.Nothing}

-- | The token to use to get the next page of results after a previous API
-- call. This token must be passed in with the same parameters that were
-- specified in the original call. For example, if the original call
-- specified an AttributeKey of \'Username\' with a value of \'root\', the
-- call with NextToken should include those same parameters.
listTrails_nextToken :: Lens.Lens' ListTrails (Prelude.Maybe Prelude.Text)
listTrails_nextToken = Lens.lens (\ListTrails' {nextToken} -> nextToken) (\s@ListTrails' {} a -> s {nextToken = a} :: ListTrails)

instance Core.AWSPager ListTrails where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listTrailsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listTrailsResponse_trails
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listTrails_nextToken
          Lens..~ rs
          Lens.^? listTrailsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListTrails where
  type AWSResponse ListTrails = ListTrailsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTrailsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "Trails" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListTrails where
  hashWithSalt _salt ListTrails' {..} =
    _salt `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListTrails where
  rnf ListTrails' {..} = Prelude.rnf nextToken

instance Data.ToHeaders ListTrails where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "com.amazonaws.cloudtrail.v20131101.CloudTrail_20131101.ListTrails" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListTrails where
  toJSON ListTrails' {..} =
    Data.object
      ( Prelude.catMaybes
          [("NextToken" Data..=) Prelude.<$> nextToken]
      )

instance Data.ToPath ListTrails where
  toPath = Prelude.const "/"

instance Data.ToQuery ListTrails where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListTrailsResponse' smart constructor.
data ListTrailsResponse = ListTrailsResponse'
  { -- | The token to use to get the next page of results after a previous API
    -- call. If the token does not appear, there are no more results to return.
    -- The token must be passed in with the same parameters as the previous
    -- call. For example, if the original call specified an AttributeKey of
    -- \'Username\' with a value of \'root\', the call with NextToken should
    -- include those same parameters.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Returns the name, ARN, and home region of trails in the current account.
    trails :: Prelude.Maybe [TrailInfo],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTrailsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listTrailsResponse_nextToken' - The token to use to get the next page of results after a previous API
-- call. If the token does not appear, there are no more results to return.
-- The token must be passed in with the same parameters as the previous
-- call. For example, if the original call specified an AttributeKey of
-- \'Username\' with a value of \'root\', the call with NextToken should
-- include those same parameters.
--
-- 'trails', 'listTrailsResponse_trails' - Returns the name, ARN, and home region of trails in the current account.
--
-- 'httpStatus', 'listTrailsResponse_httpStatus' - The response's http status code.
newListTrailsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListTrailsResponse
newListTrailsResponse pHttpStatus_ =
  ListTrailsResponse'
    { nextToken = Prelude.Nothing,
      trails = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use to get the next page of results after a previous API
-- call. If the token does not appear, there are no more results to return.
-- The token must be passed in with the same parameters as the previous
-- call. For example, if the original call specified an AttributeKey of
-- \'Username\' with a value of \'root\', the call with NextToken should
-- include those same parameters.
listTrailsResponse_nextToken :: Lens.Lens' ListTrailsResponse (Prelude.Maybe Prelude.Text)
listTrailsResponse_nextToken = Lens.lens (\ListTrailsResponse' {nextToken} -> nextToken) (\s@ListTrailsResponse' {} a -> s {nextToken = a} :: ListTrailsResponse)

-- | Returns the name, ARN, and home region of trails in the current account.
listTrailsResponse_trails :: Lens.Lens' ListTrailsResponse (Prelude.Maybe [TrailInfo])
listTrailsResponse_trails = Lens.lens (\ListTrailsResponse' {trails} -> trails) (\s@ListTrailsResponse' {} a -> s {trails = a} :: ListTrailsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listTrailsResponse_httpStatus :: Lens.Lens' ListTrailsResponse Prelude.Int
listTrailsResponse_httpStatus = Lens.lens (\ListTrailsResponse' {httpStatus} -> httpStatus) (\s@ListTrailsResponse' {} a -> s {httpStatus = a} :: ListTrailsResponse)

instance Prelude.NFData ListTrailsResponse where
  rnf ListTrailsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf trails
      `Prelude.seq` Prelude.rnf httpStatus
