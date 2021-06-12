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
-- Module      : Network.AWS.Rekognition.ListStreamProcessors
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of stream processors that you have created with
-- CreateStreamProcessor.
--
-- This operation returns paginated results.
module Network.AWS.Rekognition.ListStreamProcessors
  ( -- * Creating a Request
    ListStreamProcessors (..),
    newListStreamProcessors,

    -- * Request Lenses
    listStreamProcessors_nextToken,
    listStreamProcessors_maxResults,

    -- * Destructuring the Response
    ListStreamProcessorsResponse (..),
    newListStreamProcessorsResponse,

    -- * Response Lenses
    listStreamProcessorsResponse_nextToken,
    listStreamProcessorsResponse_streamProcessors,
    listStreamProcessorsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Rekognition.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListStreamProcessors' smart constructor.
data ListStreamProcessors = ListStreamProcessors'
  { -- | If the previous response was incomplete (because there are more stream
    -- processors to retrieve), Amazon Rekognition Video returns a pagination
    -- token in the response. You can use this pagination token to retrieve the
    -- next set of stream processors.
    nextToken :: Core.Maybe Core.Text,
    -- | Maximum number of stream processors you want Amazon Rekognition Video to
    -- return in the response. The default is 1000.
    maxResults :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListStreamProcessors' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listStreamProcessors_nextToken' - If the previous response was incomplete (because there are more stream
-- processors to retrieve), Amazon Rekognition Video returns a pagination
-- token in the response. You can use this pagination token to retrieve the
-- next set of stream processors.
--
-- 'maxResults', 'listStreamProcessors_maxResults' - Maximum number of stream processors you want Amazon Rekognition Video to
-- return in the response. The default is 1000.
newListStreamProcessors ::
  ListStreamProcessors
newListStreamProcessors =
  ListStreamProcessors'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing
    }

-- | If the previous response was incomplete (because there are more stream
-- processors to retrieve), Amazon Rekognition Video returns a pagination
-- token in the response. You can use this pagination token to retrieve the
-- next set of stream processors.
listStreamProcessors_nextToken :: Lens.Lens' ListStreamProcessors (Core.Maybe Core.Text)
listStreamProcessors_nextToken = Lens.lens (\ListStreamProcessors' {nextToken} -> nextToken) (\s@ListStreamProcessors' {} a -> s {nextToken = a} :: ListStreamProcessors)

-- | Maximum number of stream processors you want Amazon Rekognition Video to
-- return in the response. The default is 1000.
listStreamProcessors_maxResults :: Lens.Lens' ListStreamProcessors (Core.Maybe Core.Natural)
listStreamProcessors_maxResults = Lens.lens (\ListStreamProcessors' {maxResults} -> maxResults) (\s@ListStreamProcessors' {} a -> s {maxResults = a} :: ListStreamProcessors)

instance Core.AWSPager ListStreamProcessors where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listStreamProcessorsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listStreamProcessorsResponse_streamProcessors
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listStreamProcessors_nextToken
          Lens..~ rs
          Lens.^? listStreamProcessorsResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest ListStreamProcessors where
  type
    AWSResponse ListStreamProcessors =
      ListStreamProcessorsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListStreamProcessorsResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "StreamProcessors" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListStreamProcessors

instance Core.NFData ListStreamProcessors

instance Core.ToHeaders ListStreamProcessors where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "RekognitionService.ListStreamProcessors" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListStreamProcessors where
  toJSON ListStreamProcessors' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults
          ]
      )

instance Core.ToPath ListStreamProcessors where
  toPath = Core.const "/"

instance Core.ToQuery ListStreamProcessors where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListStreamProcessorsResponse' smart constructor.
data ListStreamProcessorsResponse = ListStreamProcessorsResponse'
  { -- | If the response is truncated, Amazon Rekognition Video returns this
    -- token that you can use in the subsequent request to retrieve the next
    -- set of stream processors.
    nextToken :: Core.Maybe Core.Text,
    -- | List of stream processors that you have created.
    streamProcessors :: Core.Maybe [StreamProcessor],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListStreamProcessorsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listStreamProcessorsResponse_nextToken' - If the response is truncated, Amazon Rekognition Video returns this
-- token that you can use in the subsequent request to retrieve the next
-- set of stream processors.
--
-- 'streamProcessors', 'listStreamProcessorsResponse_streamProcessors' - List of stream processors that you have created.
--
-- 'httpStatus', 'listStreamProcessorsResponse_httpStatus' - The response's http status code.
newListStreamProcessorsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListStreamProcessorsResponse
newListStreamProcessorsResponse pHttpStatus_ =
  ListStreamProcessorsResponse'
    { nextToken =
        Core.Nothing,
      streamProcessors = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If the response is truncated, Amazon Rekognition Video returns this
-- token that you can use in the subsequent request to retrieve the next
-- set of stream processors.
listStreamProcessorsResponse_nextToken :: Lens.Lens' ListStreamProcessorsResponse (Core.Maybe Core.Text)
listStreamProcessorsResponse_nextToken = Lens.lens (\ListStreamProcessorsResponse' {nextToken} -> nextToken) (\s@ListStreamProcessorsResponse' {} a -> s {nextToken = a} :: ListStreamProcessorsResponse)

-- | List of stream processors that you have created.
listStreamProcessorsResponse_streamProcessors :: Lens.Lens' ListStreamProcessorsResponse (Core.Maybe [StreamProcessor])
listStreamProcessorsResponse_streamProcessors = Lens.lens (\ListStreamProcessorsResponse' {streamProcessors} -> streamProcessors) (\s@ListStreamProcessorsResponse' {} a -> s {streamProcessors = a} :: ListStreamProcessorsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listStreamProcessorsResponse_httpStatus :: Lens.Lens' ListStreamProcessorsResponse Core.Int
listStreamProcessorsResponse_httpStatus = Lens.lens (\ListStreamProcessorsResponse' {httpStatus} -> httpStatus) (\s@ListStreamProcessorsResponse' {} a -> s {httpStatus = a} :: ListStreamProcessorsResponse)

instance Core.NFData ListStreamProcessorsResponse
