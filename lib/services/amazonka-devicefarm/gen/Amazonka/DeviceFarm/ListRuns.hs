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
-- Module      : Amazonka.DeviceFarm.ListRuns
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about runs, given an AWS Device Farm project ARN.
--
-- This operation returns paginated results.
module Amazonka.DeviceFarm.ListRuns
  ( -- * Creating a Request
    ListRuns (..),
    newListRuns,

    -- * Request Lenses
    listRuns_nextToken,
    listRuns_arn,

    -- * Destructuring the Response
    ListRunsResponse (..),
    newListRunsResponse,

    -- * Response Lenses
    listRunsResponse_nextToken,
    listRunsResponse_runs,
    listRunsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DeviceFarm.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents a request to the list runs operation.
--
-- /See:/ 'newListRuns' smart constructor.
data ListRuns = ListRuns'
  { -- | An identifier that was returned from the previous call to this
    -- operation, which can be used to return the next set of items in the
    -- list.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the project for which you want to list
    -- runs.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListRuns' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listRuns_nextToken' - An identifier that was returned from the previous call to this
-- operation, which can be used to return the next set of items in the
-- list.
--
-- 'arn', 'listRuns_arn' - The Amazon Resource Name (ARN) of the project for which you want to list
-- runs.
newListRuns ::
  -- | 'arn'
  Prelude.Text ->
  ListRuns
newListRuns pArn_ =
  ListRuns' {nextToken = Prelude.Nothing, arn = pArn_}

-- | An identifier that was returned from the previous call to this
-- operation, which can be used to return the next set of items in the
-- list.
listRuns_nextToken :: Lens.Lens' ListRuns (Prelude.Maybe Prelude.Text)
listRuns_nextToken = Lens.lens (\ListRuns' {nextToken} -> nextToken) (\s@ListRuns' {} a -> s {nextToken = a} :: ListRuns)

-- | The Amazon Resource Name (ARN) of the project for which you want to list
-- runs.
listRuns_arn :: Lens.Lens' ListRuns Prelude.Text
listRuns_arn = Lens.lens (\ListRuns' {arn} -> arn) (\s@ListRuns' {} a -> s {arn = a} :: ListRuns)

instance Core.AWSPager ListRuns where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listRunsResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listRunsResponse_runs Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listRuns_nextToken
          Lens..~ rs
          Lens.^? listRunsResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListRuns where
  type AWSResponse ListRuns = ListRunsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListRunsResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> (x Core..?> "runs" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListRuns where
  hashWithSalt _salt ListRuns' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` arn

instance Prelude.NFData ListRuns where
  rnf ListRuns' {..} =
    Prelude.rnf nextToken `Prelude.seq` Prelude.rnf arn

instance Core.ToHeaders ListRuns where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DeviceFarm_20150623.ListRuns" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListRuns where
  toJSON ListRuns' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("nextToken" Core..=) Prelude.<$> nextToken,
            Prelude.Just ("arn" Core..= arn)
          ]
      )

instance Core.ToPath ListRuns where
  toPath = Prelude.const "/"

instance Core.ToQuery ListRuns where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the result of a list runs request.
--
-- /See:/ 'newListRunsResponse' smart constructor.
data ListRunsResponse = ListRunsResponse'
  { -- | If the number of items that are returned is significantly large, this is
    -- an identifier that is also returned. It can be used in a subsequent call
    -- to this operation to return the next set of items in the list.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the runs.
    runs :: Prelude.Maybe [Run],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListRunsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listRunsResponse_nextToken' - If the number of items that are returned is significantly large, this is
-- an identifier that is also returned. It can be used in a subsequent call
-- to this operation to return the next set of items in the list.
--
-- 'runs', 'listRunsResponse_runs' - Information about the runs.
--
-- 'httpStatus', 'listRunsResponse_httpStatus' - The response's http status code.
newListRunsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListRunsResponse
newListRunsResponse pHttpStatus_ =
  ListRunsResponse'
    { nextToken = Prelude.Nothing,
      runs = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If the number of items that are returned is significantly large, this is
-- an identifier that is also returned. It can be used in a subsequent call
-- to this operation to return the next set of items in the list.
listRunsResponse_nextToken :: Lens.Lens' ListRunsResponse (Prelude.Maybe Prelude.Text)
listRunsResponse_nextToken = Lens.lens (\ListRunsResponse' {nextToken} -> nextToken) (\s@ListRunsResponse' {} a -> s {nextToken = a} :: ListRunsResponse)

-- | Information about the runs.
listRunsResponse_runs :: Lens.Lens' ListRunsResponse (Prelude.Maybe [Run])
listRunsResponse_runs = Lens.lens (\ListRunsResponse' {runs} -> runs) (\s@ListRunsResponse' {} a -> s {runs = a} :: ListRunsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listRunsResponse_httpStatus :: Lens.Lens' ListRunsResponse Prelude.Int
listRunsResponse_httpStatus = Lens.lens (\ListRunsResponse' {httpStatus} -> httpStatus) (\s@ListRunsResponse' {} a -> s {httpStatus = a} :: ListRunsResponse)

instance Prelude.NFData ListRunsResponse where
  rnf ListRunsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf runs
      `Prelude.seq` Prelude.rnf httpStatus
