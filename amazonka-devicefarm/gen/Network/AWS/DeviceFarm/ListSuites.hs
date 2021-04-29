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
-- Module      : Network.AWS.DeviceFarm.ListSuites
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about test suites for a given job.
--
-- This operation returns paginated results.
module Network.AWS.DeviceFarm.ListSuites
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

import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Pager.AWSPager ListSuites where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? listSuitesResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? listSuitesResponse_suites Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& listSuites_nextToken
          Lens..~ rs
          Lens.^? listSuitesResponse_nextToken Prelude.. Lens._Just

instance Prelude.AWSRequest ListSuites where
  type Rs ListSuites = ListSuitesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListSuitesResponse'
            Prelude.<$> (x Prelude..?> "nextToken")
            Prelude.<*> (x Prelude..?> "suites" Prelude..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListSuites

instance Prelude.NFData ListSuites

instance Prelude.ToHeaders ListSuites where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "DeviceFarm_20150623.ListSuites" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON ListSuites where
  toJSON ListSuites' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("nextToken" Prelude..=) Prelude.<$> nextToken,
            Prelude.Just ("arn" Prelude..= arn)
          ]
      )

instance Prelude.ToPath ListSuites where
  toPath = Prelude.const "/"

instance Prelude.ToQuery ListSuites where
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
listSuitesResponse_suites = Lens.lens (\ListSuitesResponse' {suites} -> suites) (\s@ListSuitesResponse' {} a -> s {suites = a} :: ListSuitesResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
listSuitesResponse_httpStatus :: Lens.Lens' ListSuitesResponse Prelude.Int
listSuitesResponse_httpStatus = Lens.lens (\ListSuitesResponse' {httpStatus} -> httpStatus) (\s@ListSuitesResponse' {} a -> s {httpStatus = a} :: ListSuitesResponse)

instance Prelude.NFData ListSuitesResponse
