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

import qualified Network.AWS.Core as Core
import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents a request to the list suites operation.
--
-- /See:/ 'newListSuites' smart constructor.
data ListSuites = ListSuites'
  { -- | An identifier that was returned from the previous call to this
    -- operation, which can be used to return the next set of items in the
    -- list.
    nextToken :: Core.Maybe Core.Text,
    -- | The job\'s Amazon Resource Name (ARN).
    arn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  ListSuites
newListSuites pArn_ =
  ListSuites' {nextToken = Core.Nothing, arn = pArn_}

-- | An identifier that was returned from the previous call to this
-- operation, which can be used to return the next set of items in the
-- list.
listSuites_nextToken :: Lens.Lens' ListSuites (Core.Maybe Core.Text)
listSuites_nextToken = Lens.lens (\ListSuites' {nextToken} -> nextToken) (\s@ListSuites' {} a -> s {nextToken = a} :: ListSuites)

-- | The job\'s Amazon Resource Name (ARN).
listSuites_arn :: Lens.Lens' ListSuites Core.Text
listSuites_arn = Lens.lens (\ListSuites' {arn} -> arn) (\s@ListSuites' {} a -> s {arn = a} :: ListSuites)

instance Core.AWSPager ListSuites where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listSuitesResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listSuitesResponse_suites Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listSuites_nextToken
          Lens..~ rs
          Lens.^? listSuitesResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest ListSuites where
  type AWSResponse ListSuites = ListSuitesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListSuitesResponse'
            Core.<$> (x Core..?> "nextToken")
            Core.<*> (x Core..?> "suites" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListSuites

instance Core.NFData ListSuites

instance Core.ToHeaders ListSuites where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DeviceFarm_20150623.ListSuites" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListSuites where
  toJSON ListSuites' {..} =
    Core.object
      ( Core.catMaybes
          [ ("nextToken" Core..=) Core.<$> nextToken,
            Core.Just ("arn" Core..= arn)
          ]
      )

instance Core.ToPath ListSuites where
  toPath = Core.const "/"

instance Core.ToQuery ListSuites where
  toQuery = Core.const Core.mempty

-- | Represents the result of a list suites request.
--
-- /See:/ 'newListSuitesResponse' smart constructor.
data ListSuitesResponse = ListSuitesResponse'
  { -- | If the number of items that are returned is significantly large, this is
    -- an identifier that is also returned. It can be used in a subsequent call
    -- to this operation to return the next set of items in the list.
    nextToken :: Core.Maybe Core.Text,
    -- | Information about the suites.
    suites :: Core.Maybe [Suite],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  ListSuitesResponse
newListSuitesResponse pHttpStatus_ =
  ListSuitesResponse'
    { nextToken = Core.Nothing,
      suites = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If the number of items that are returned is significantly large, this is
-- an identifier that is also returned. It can be used in a subsequent call
-- to this operation to return the next set of items in the list.
listSuitesResponse_nextToken :: Lens.Lens' ListSuitesResponse (Core.Maybe Core.Text)
listSuitesResponse_nextToken = Lens.lens (\ListSuitesResponse' {nextToken} -> nextToken) (\s@ListSuitesResponse' {} a -> s {nextToken = a} :: ListSuitesResponse)

-- | Information about the suites.
listSuitesResponse_suites :: Lens.Lens' ListSuitesResponse (Core.Maybe [Suite])
listSuitesResponse_suites = Lens.lens (\ListSuitesResponse' {suites} -> suites) (\s@ListSuitesResponse' {} a -> s {suites = a} :: ListSuitesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listSuitesResponse_httpStatus :: Lens.Lens' ListSuitesResponse Core.Int
listSuitesResponse_httpStatus = Lens.lens (\ListSuitesResponse' {httpStatus} -> httpStatus) (\s@ListSuitesResponse' {} a -> s {httpStatus = a} :: ListSuitesResponse)

instance Core.NFData ListSuitesResponse
