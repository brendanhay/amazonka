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
-- Module      : Amazonka.Rekognition.ListStreamProcessors
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of stream processors that you have created with
-- CreateStreamProcessor.
--
-- This operation returns paginated results.
module Amazonka.Rekognition.ListStreamProcessors
  ( -- * Creating a Request
    ListStreamProcessors (..),
    newListStreamProcessors,

    -- * Request Lenses
    listStreamProcessors_maxResults,
    listStreamProcessors_nextToken,

    -- * Destructuring the Response
    ListStreamProcessorsResponse (..),
    newListStreamProcessorsResponse,

    -- * Response Lenses
    listStreamProcessorsResponse_nextToken,
    listStreamProcessorsResponse_streamProcessors,
    listStreamProcessorsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rekognition.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListStreamProcessors' smart constructor.
data ListStreamProcessors = ListStreamProcessors'
  { -- | Maximum number of stream processors you want Amazon Rekognition Video to
    -- return in the response. The default is 1000.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | If the previous response was incomplete (because there are more stream
    -- processors to retrieve), Amazon Rekognition Video returns a pagination
    -- token in the response. You can use this pagination token to retrieve the
    -- next set of stream processors.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListStreamProcessors' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listStreamProcessors_maxResults' - Maximum number of stream processors you want Amazon Rekognition Video to
-- return in the response. The default is 1000.
--
-- 'nextToken', 'listStreamProcessors_nextToken' - If the previous response was incomplete (because there are more stream
-- processors to retrieve), Amazon Rekognition Video returns a pagination
-- token in the response. You can use this pagination token to retrieve the
-- next set of stream processors.
newListStreamProcessors ::
  ListStreamProcessors
newListStreamProcessors =
  ListStreamProcessors'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | Maximum number of stream processors you want Amazon Rekognition Video to
-- return in the response. The default is 1000.
listStreamProcessors_maxResults :: Lens.Lens' ListStreamProcessors (Prelude.Maybe Prelude.Natural)
listStreamProcessors_maxResults = Lens.lens (\ListStreamProcessors' {maxResults} -> maxResults) (\s@ListStreamProcessors' {} a -> s {maxResults = a} :: ListStreamProcessors)

-- | If the previous response was incomplete (because there are more stream
-- processors to retrieve), Amazon Rekognition Video returns a pagination
-- token in the response. You can use this pagination token to retrieve the
-- next set of stream processors.
listStreamProcessors_nextToken :: Lens.Lens' ListStreamProcessors (Prelude.Maybe Prelude.Text)
listStreamProcessors_nextToken = Lens.lens (\ListStreamProcessors' {nextToken} -> nextToken) (\s@ListStreamProcessors' {} a -> s {nextToken = a} :: ListStreamProcessors)

instance Core.AWSPager ListStreamProcessors where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listStreamProcessorsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listStreamProcessorsResponse_streamProcessors
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listStreamProcessors_nextToken
          Lens..~ rs
          Lens.^? listStreamProcessorsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListStreamProcessors where
  type
    AWSResponse ListStreamProcessors =
      ListStreamProcessorsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListStreamProcessorsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> ( x
                            Data..?> "StreamProcessors"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListStreamProcessors where
  hashWithSalt _salt ListStreamProcessors' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListStreamProcessors where
  rnf ListStreamProcessors' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListStreamProcessors where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "RekognitionService.ListStreamProcessors" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListStreamProcessors where
  toJSON ListStreamProcessors' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListStreamProcessors where
  toPath = Prelude.const "/"

instance Data.ToQuery ListStreamProcessors where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListStreamProcessorsResponse' smart constructor.
data ListStreamProcessorsResponse = ListStreamProcessorsResponse'
  { -- | If the response is truncated, Amazon Rekognition Video returns this
    -- token that you can use in the subsequent request to retrieve the next
    -- set of stream processors.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | List of stream processors that you have created.
    streamProcessors :: Prelude.Maybe [StreamProcessor],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  ListStreamProcessorsResponse
newListStreamProcessorsResponse pHttpStatus_ =
  ListStreamProcessorsResponse'
    { nextToken =
        Prelude.Nothing,
      streamProcessors = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If the response is truncated, Amazon Rekognition Video returns this
-- token that you can use in the subsequent request to retrieve the next
-- set of stream processors.
listStreamProcessorsResponse_nextToken :: Lens.Lens' ListStreamProcessorsResponse (Prelude.Maybe Prelude.Text)
listStreamProcessorsResponse_nextToken = Lens.lens (\ListStreamProcessorsResponse' {nextToken} -> nextToken) (\s@ListStreamProcessorsResponse' {} a -> s {nextToken = a} :: ListStreamProcessorsResponse)

-- | List of stream processors that you have created.
listStreamProcessorsResponse_streamProcessors :: Lens.Lens' ListStreamProcessorsResponse (Prelude.Maybe [StreamProcessor])
listStreamProcessorsResponse_streamProcessors = Lens.lens (\ListStreamProcessorsResponse' {streamProcessors} -> streamProcessors) (\s@ListStreamProcessorsResponse' {} a -> s {streamProcessors = a} :: ListStreamProcessorsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listStreamProcessorsResponse_httpStatus :: Lens.Lens' ListStreamProcessorsResponse Prelude.Int
listStreamProcessorsResponse_httpStatus = Lens.lens (\ListStreamProcessorsResponse' {httpStatus} -> httpStatus) (\s@ListStreamProcessorsResponse' {} a -> s {httpStatus = a} :: ListStreamProcessorsResponse)

instance Prelude.NFData ListStreamProcessorsResponse where
  rnf ListStreamProcessorsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf streamProcessors
      `Prelude.seq` Prelude.rnf httpStatus
