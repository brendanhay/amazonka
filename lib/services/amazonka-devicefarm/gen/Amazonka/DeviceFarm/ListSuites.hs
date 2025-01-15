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
-- Module      : Amazonka.DeviceFarm.ListSuites
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about test suites for a given job.
--
-- This operation returns paginated results.
module Amazonka.DeviceFarm.ListSuites
  ( -- * Creating a Request
    ListSuites (..),
    newListSuites,

    -- * Request Lenses
    listSuites_nextToken,
    listSuites_arn,

    -- * Destructuring the Response
    ListSuitesResponse (..),
    newListSuitesResponse,

    -- * Response Lenses
    listSuitesResponse_nextToken,
    listSuitesResponse_suites,
    listSuitesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DeviceFarm.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents a request to the list suites operation.
--
-- /See:/ 'newListSuites' smart constructor.
data ListSuites = ListSuites'
  { -- | An identifier that was returned from the previous call to this
    -- operation, which can be used to return the next set of items in the
    -- list.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The job\'s Amazon Resource Name (ARN).
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSuites' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listSuites_nextToken' - An identifier that was returned from the previous call to this
-- operation, which can be used to return the next set of items in the
-- list.
--
-- 'arn', 'listSuites_arn' - The job\'s Amazon Resource Name (ARN).
newListSuites ::
  -- | 'arn'
  Prelude.Text ->
  ListSuites
newListSuites pArn_ =
  ListSuites'
    { nextToken = Prelude.Nothing,
      arn = pArn_
    }

-- | An identifier that was returned from the previous call to this
-- operation, which can be used to return the next set of items in the
-- list.
listSuites_nextToken :: Lens.Lens' ListSuites (Prelude.Maybe Prelude.Text)
listSuites_nextToken = Lens.lens (\ListSuites' {nextToken} -> nextToken) (\s@ListSuites' {} a -> s {nextToken = a} :: ListSuites)

-- | The job\'s Amazon Resource Name (ARN).
listSuites_arn :: Lens.Lens' ListSuites Prelude.Text
listSuites_arn = Lens.lens (\ListSuites' {arn} -> arn) (\s@ListSuites' {} a -> s {arn = a} :: ListSuites)

instance Core.AWSPager ListSuites where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listSuitesResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listSuitesResponse_suites
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& listSuites_nextToken
              Lens..~ rs
              Lens.^? listSuitesResponse_nextToken
              Prelude.. Lens._Just

instance Core.AWSRequest ListSuites where
  type AWSResponse ListSuites = ListSuitesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListSuitesResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (x Data..?> "suites" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListSuites where
  hashWithSalt _salt ListSuites' {..} =
    _salt
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` arn

instance Prelude.NFData ListSuites where
  rnf ListSuites' {..} =
    Prelude.rnf nextToken `Prelude.seq` Prelude.rnf arn

instance Data.ToHeaders ListSuites where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "DeviceFarm_20150623.ListSuites" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListSuites where
  toJSON ListSuites' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("nextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just ("arn" Data..= arn)
          ]
      )

instance Data.ToPath ListSuites where
  toPath = Prelude.const "/"

instance Data.ToQuery ListSuites where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the result of a list suites request.
--
-- /See:/ 'newListSuitesResponse' smart constructor.
data ListSuitesResponse = ListSuitesResponse'
  { -- | If the number of items that are returned is significantly large, this is
    -- an identifier that is also returned. It can be used in a subsequent call
    -- to this operation to return the next set of items in the list.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the suites.
    suites :: Prelude.Maybe [Suite],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSuitesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listSuitesResponse_nextToken' - If the number of items that are returned is significantly large, this is
-- an identifier that is also returned. It can be used in a subsequent call
-- to this operation to return the next set of items in the list.
--
-- 'suites', 'listSuitesResponse_suites' - Information about the suites.
--
-- 'httpStatus', 'listSuitesResponse_httpStatus' - The response's http status code.
newListSuitesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListSuitesResponse
newListSuitesResponse pHttpStatus_ =
  ListSuitesResponse'
    { nextToken = Prelude.Nothing,
      suites = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If the number of items that are returned is significantly large, this is
-- an identifier that is also returned. It can be used in a subsequent call
-- to this operation to return the next set of items in the list.
listSuitesResponse_nextToken :: Lens.Lens' ListSuitesResponse (Prelude.Maybe Prelude.Text)
listSuitesResponse_nextToken = Lens.lens (\ListSuitesResponse' {nextToken} -> nextToken) (\s@ListSuitesResponse' {} a -> s {nextToken = a} :: ListSuitesResponse)

-- | Information about the suites.
listSuitesResponse_suites :: Lens.Lens' ListSuitesResponse (Prelude.Maybe [Suite])
listSuitesResponse_suites = Lens.lens (\ListSuitesResponse' {suites} -> suites) (\s@ListSuitesResponse' {} a -> s {suites = a} :: ListSuitesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listSuitesResponse_httpStatus :: Lens.Lens' ListSuitesResponse Prelude.Int
listSuitesResponse_httpStatus = Lens.lens (\ListSuitesResponse' {httpStatus} -> httpStatus) (\s@ListSuitesResponse' {} a -> s {httpStatus = a} :: ListSuitesResponse)

instance Prelude.NFData ListSuitesResponse where
  rnf ListSuitesResponse' {..} =
    Prelude.rnf nextToken `Prelude.seq`
      Prelude.rnf suites `Prelude.seq`
        Prelude.rnf httpStatus
