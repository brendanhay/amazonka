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
-- Module      : Network.AWS.DeviceFarm.ListRuns
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about runs, given an AWS Device Farm project ARN.
--
-- This operation returns paginated results.
module Network.AWS.DeviceFarm.ListRuns
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

import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Pager.AWSPager ListRuns where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? listRunsResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? listRunsResponse_runs Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& listRuns_nextToken
          Lens..~ rs
          Lens.^? listRunsResponse_nextToken Prelude.. Lens._Just

instance Prelude.AWSRequest ListRuns where
  type Rs ListRuns = ListRunsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListRunsResponse'
            Prelude.<$> (x Prelude..?> "nextToken")
            Prelude.<*> (x Prelude..?> "runs" Prelude..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListRuns

instance Prelude.NFData ListRuns

instance Prelude.ToHeaders ListRuns where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "DeviceFarm_20150623.ListRuns" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON ListRuns where
  toJSON ListRuns' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("nextToken" Prelude..=) Prelude.<$> nextToken,
            Prelude.Just ("arn" Prelude..= arn)
          ]
      )

instance Prelude.ToPath ListRuns where
  toPath = Prelude.const "/"

instance Prelude.ToQuery ListRuns where
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
listRunsResponse_runs = Lens.lens (\ListRunsResponse' {runs} -> runs) (\s@ListRunsResponse' {} a -> s {runs = a} :: ListRunsResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
listRunsResponse_httpStatus :: Lens.Lens' ListRunsResponse Prelude.Int
listRunsResponse_httpStatus = Lens.lens (\ListRunsResponse' {httpStatus} -> httpStatus) (\s@ListRunsResponse' {} a -> s {httpStatus = a} :: ListRunsResponse)

instance Prelude.NFData ListRunsResponse
