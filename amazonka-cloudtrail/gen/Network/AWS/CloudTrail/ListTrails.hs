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
-- Module      : Network.AWS.CloudTrail.ListTrails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists trails that are in the current account.
--
-- This operation returns paginated results.
module Network.AWS.CloudTrail.ListTrails
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

import Network.AWS.CloudTrail.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListTrails' smart constructor.
data ListTrails = ListTrails'
  { -- | The token to use to get the next page of results after a previous API
    -- call. This token must be passed in with the same parameters that were
    -- specified in the the original call. For example, if the original call
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
-- specified in the the original call. For example, if the original call
-- specified an AttributeKey of \'Username\' with a value of \'root\', the
-- call with NextToken should include those same parameters.
newListTrails ::
  ListTrails
newListTrails =
  ListTrails' {nextToken = Prelude.Nothing}

-- | The token to use to get the next page of results after a previous API
-- call. This token must be passed in with the same parameters that were
-- specified in the the original call. For example, if the original call
-- specified an AttributeKey of \'Username\' with a value of \'root\', the
-- call with NextToken should include those same parameters.
listTrails_nextToken :: Lens.Lens' ListTrails (Prelude.Maybe Prelude.Text)
listTrails_nextToken = Lens.lens (\ListTrails' {nextToken} -> nextToken) (\s@ListTrails' {} a -> s {nextToken = a} :: ListTrails)

instance Core.AWSPager ListTrails where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listTrailsResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listTrailsResponse_trails Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listTrails_nextToken
          Lens..~ rs
          Lens.^? listTrailsResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListTrails where
  type AWSResponse ListTrails = ListTrailsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTrailsResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "Trails" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListTrails

instance Prelude.NFData ListTrails

instance Core.ToHeaders ListTrails where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "com.amazonaws.cloudtrail.v20131101.CloudTrail_20131101.ListTrails" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListTrails where
  toJSON ListTrails' {..} =
    Core.object
      ( Prelude.catMaybes
          [("NextToken" Core..=) Prelude.<$> nextToken]
      )

instance Core.ToPath ListTrails where
  toPath = Prelude.const "/"

instance Core.ToQuery ListTrails where
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
listTrailsResponse_trails = Lens.lens (\ListTrailsResponse' {trails} -> trails) (\s@ListTrailsResponse' {} a -> s {trails = a} :: ListTrailsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listTrailsResponse_httpStatus :: Lens.Lens' ListTrailsResponse Prelude.Int
listTrailsResponse_httpStatus = Lens.lens (\ListTrailsResponse' {httpStatus} -> httpStatus) (\s@ListTrailsResponse' {} a -> s {httpStatus = a} :: ListTrailsResponse)

instance Prelude.NFData ListTrailsResponse
