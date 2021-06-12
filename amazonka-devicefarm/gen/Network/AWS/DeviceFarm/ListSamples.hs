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
-- Module      : Network.AWS.DeviceFarm.ListSamples
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about samples, given an AWS Device Farm job ARN.
--
-- This operation returns paginated results.
module Network.AWS.DeviceFarm.ListSamples
  ( -- * Creating a Request
    ListSamples (..),
    newListSamples,

    -- * Request Lenses
    listSamples_nextToken,
    listSamples_arn,

    -- * Destructuring the Response
    ListSamplesResponse (..),
    newListSamplesResponse,

    -- * Response Lenses
    listSamplesResponse_nextToken,
    listSamplesResponse_samples,
    listSamplesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents a request to the list samples operation.
--
-- /See:/ 'newListSamples' smart constructor.
data ListSamples = ListSamples'
  { -- | An identifier that was returned from the previous call to this
    -- operation, which can be used to return the next set of items in the
    -- list.
    nextToken :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) of the job used to list samples.
    arn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListSamples' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listSamples_nextToken' - An identifier that was returned from the previous call to this
-- operation, which can be used to return the next set of items in the
-- list.
--
-- 'arn', 'listSamples_arn' - The Amazon Resource Name (ARN) of the job used to list samples.
newListSamples ::
  -- | 'arn'
  Core.Text ->
  ListSamples
newListSamples pArn_ =
  ListSamples' {nextToken = Core.Nothing, arn = pArn_}

-- | An identifier that was returned from the previous call to this
-- operation, which can be used to return the next set of items in the
-- list.
listSamples_nextToken :: Lens.Lens' ListSamples (Core.Maybe Core.Text)
listSamples_nextToken = Lens.lens (\ListSamples' {nextToken} -> nextToken) (\s@ListSamples' {} a -> s {nextToken = a} :: ListSamples)

-- | The Amazon Resource Name (ARN) of the job used to list samples.
listSamples_arn :: Lens.Lens' ListSamples Core.Text
listSamples_arn = Lens.lens (\ListSamples' {arn} -> arn) (\s@ListSamples' {} a -> s {arn = a} :: ListSamples)

instance Core.AWSPager ListSamples where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listSamplesResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listSamplesResponse_samples Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listSamples_nextToken
          Lens..~ rs
          Lens.^? listSamplesResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest ListSamples where
  type AWSResponse ListSamples = ListSamplesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListSamplesResponse'
            Core.<$> (x Core..?> "nextToken")
            Core.<*> (x Core..?> "samples" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListSamples

instance Core.NFData ListSamples

instance Core.ToHeaders ListSamples where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DeviceFarm_20150623.ListSamples" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListSamples where
  toJSON ListSamples' {..} =
    Core.object
      ( Core.catMaybes
          [ ("nextToken" Core..=) Core.<$> nextToken,
            Core.Just ("arn" Core..= arn)
          ]
      )

instance Core.ToPath ListSamples where
  toPath = Core.const "/"

instance Core.ToQuery ListSamples where
  toQuery = Core.const Core.mempty

-- | Represents the result of a list samples request.
--
-- /See:/ 'newListSamplesResponse' smart constructor.
data ListSamplesResponse = ListSamplesResponse'
  { -- | If the number of items that are returned is significantly large, this is
    -- an identifier that is also returned. It can be used in a subsequent call
    -- to this operation to return the next set of items in the list.
    nextToken :: Core.Maybe Core.Text,
    -- | Information about the samples.
    samples :: Core.Maybe [Sample],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListSamplesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listSamplesResponse_nextToken' - If the number of items that are returned is significantly large, this is
-- an identifier that is also returned. It can be used in a subsequent call
-- to this operation to return the next set of items in the list.
--
-- 'samples', 'listSamplesResponse_samples' - Information about the samples.
--
-- 'httpStatus', 'listSamplesResponse_httpStatus' - The response's http status code.
newListSamplesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListSamplesResponse
newListSamplesResponse pHttpStatus_ =
  ListSamplesResponse'
    { nextToken = Core.Nothing,
      samples = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If the number of items that are returned is significantly large, this is
-- an identifier that is also returned. It can be used in a subsequent call
-- to this operation to return the next set of items in the list.
listSamplesResponse_nextToken :: Lens.Lens' ListSamplesResponse (Core.Maybe Core.Text)
listSamplesResponse_nextToken = Lens.lens (\ListSamplesResponse' {nextToken} -> nextToken) (\s@ListSamplesResponse' {} a -> s {nextToken = a} :: ListSamplesResponse)

-- | Information about the samples.
listSamplesResponse_samples :: Lens.Lens' ListSamplesResponse (Core.Maybe [Sample])
listSamplesResponse_samples = Lens.lens (\ListSamplesResponse' {samples} -> samples) (\s@ListSamplesResponse' {} a -> s {samples = a} :: ListSamplesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listSamplesResponse_httpStatus :: Lens.Lens' ListSamplesResponse Core.Int
listSamplesResponse_httpStatus = Lens.lens (\ListSamplesResponse' {httpStatus} -> httpStatus) (\s@ListSamplesResponse' {} a -> s {httpStatus = a} :: ListSamplesResponse)

instance Core.NFData ListSamplesResponse
