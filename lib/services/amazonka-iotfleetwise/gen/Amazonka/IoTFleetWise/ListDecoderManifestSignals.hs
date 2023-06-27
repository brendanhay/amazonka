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
-- Module      : Amazonka.IoTFleetWise.ListDecoderManifestSignals
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- A list of information about signal decoders specified in a decoder
-- manifest.
--
-- This API operation uses pagination. Specify the @nextToken@ parameter in
-- the request to return more results.
--
-- This operation returns paginated results.
module Amazonka.IoTFleetWise.ListDecoderManifestSignals
  ( -- * Creating a Request
    ListDecoderManifestSignals (..),
    newListDecoderManifestSignals,

    -- * Request Lenses
    listDecoderManifestSignals_maxResults,
    listDecoderManifestSignals_nextToken,
    listDecoderManifestSignals_name,

    -- * Destructuring the Response
    ListDecoderManifestSignalsResponse (..),
    newListDecoderManifestSignalsResponse,

    -- * Response Lenses
    listDecoderManifestSignalsResponse_nextToken,
    listDecoderManifestSignalsResponse_signalDecoders,
    listDecoderManifestSignalsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTFleetWise.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListDecoderManifestSignals' smart constructor.
data ListDecoderManifestSignals = ListDecoderManifestSignals'
  { -- | The maximum number of items to return, between 1 and 100, inclusive.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A pagination token for the next set of results.
    --
    -- If the results of a search are large, only a portion of the results are
    -- returned, and a @nextToken@ pagination token is returned in the
    -- response. To retrieve the next set of results, reissue the search
    -- request and include the returned token. When all results have been
    -- returned, the response does not contain a pagination token value.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the decoder manifest to list information about.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDecoderManifestSignals' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listDecoderManifestSignals_maxResults' - The maximum number of items to return, between 1 and 100, inclusive.
--
-- 'nextToken', 'listDecoderManifestSignals_nextToken' - A pagination token for the next set of results.
--
-- If the results of a search are large, only a portion of the results are
-- returned, and a @nextToken@ pagination token is returned in the
-- response. To retrieve the next set of results, reissue the search
-- request and include the returned token. When all results have been
-- returned, the response does not contain a pagination token value.
--
-- 'name', 'listDecoderManifestSignals_name' - The name of the decoder manifest to list information about.
newListDecoderManifestSignals ::
  -- | 'name'
  Prelude.Text ->
  ListDecoderManifestSignals
newListDecoderManifestSignals pName_ =
  ListDecoderManifestSignals'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      name = pName_
    }

-- | The maximum number of items to return, between 1 and 100, inclusive.
listDecoderManifestSignals_maxResults :: Lens.Lens' ListDecoderManifestSignals (Prelude.Maybe Prelude.Natural)
listDecoderManifestSignals_maxResults = Lens.lens (\ListDecoderManifestSignals' {maxResults} -> maxResults) (\s@ListDecoderManifestSignals' {} a -> s {maxResults = a} :: ListDecoderManifestSignals)

-- | A pagination token for the next set of results.
--
-- If the results of a search are large, only a portion of the results are
-- returned, and a @nextToken@ pagination token is returned in the
-- response. To retrieve the next set of results, reissue the search
-- request and include the returned token. When all results have been
-- returned, the response does not contain a pagination token value.
listDecoderManifestSignals_nextToken :: Lens.Lens' ListDecoderManifestSignals (Prelude.Maybe Prelude.Text)
listDecoderManifestSignals_nextToken = Lens.lens (\ListDecoderManifestSignals' {nextToken} -> nextToken) (\s@ListDecoderManifestSignals' {} a -> s {nextToken = a} :: ListDecoderManifestSignals)

-- | The name of the decoder manifest to list information about.
listDecoderManifestSignals_name :: Lens.Lens' ListDecoderManifestSignals Prelude.Text
listDecoderManifestSignals_name = Lens.lens (\ListDecoderManifestSignals' {name} -> name) (\s@ListDecoderManifestSignals' {} a -> s {name = a} :: ListDecoderManifestSignals)

instance Core.AWSPager ListDecoderManifestSignals where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listDecoderManifestSignalsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listDecoderManifestSignalsResponse_signalDecoders
            Prelude.. Lens._Just
            Prelude.. Lens.to Prelude.toList
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listDecoderManifestSignals_nextToken
          Lens..~ rs
          Lens.^? listDecoderManifestSignalsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListDecoderManifestSignals where
  type
    AWSResponse ListDecoderManifestSignals =
      ListDecoderManifestSignalsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDecoderManifestSignalsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (x Data..?> "signalDecoders")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListDecoderManifestSignals where
  hashWithSalt _salt ListDecoderManifestSignals' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` name

instance Prelude.NFData ListDecoderManifestSignals where
  rnf ListDecoderManifestSignals' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf name

instance Data.ToHeaders ListDecoderManifestSignals where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "IoTAutobahnControlPlane.ListDecoderManifestSignals" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListDecoderManifestSignals where
  toJSON ListDecoderManifestSignals' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just ("name" Data..= name)
          ]
      )

instance Data.ToPath ListDecoderManifestSignals where
  toPath = Prelude.const "/"

instance Data.ToQuery ListDecoderManifestSignals where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListDecoderManifestSignalsResponse' smart constructor.
data ListDecoderManifestSignalsResponse = ListDecoderManifestSignalsResponse'
  { -- | The token to retrieve the next set of results, or @null@ if there are no
    -- more results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Information about a list of signals to decode.
    signalDecoders :: Prelude.Maybe (Prelude.NonEmpty SignalDecoder),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDecoderManifestSignalsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listDecoderManifestSignalsResponse_nextToken' - The token to retrieve the next set of results, or @null@ if there are no
-- more results.
--
-- 'signalDecoders', 'listDecoderManifestSignalsResponse_signalDecoders' - Information about a list of signals to decode.
--
-- 'httpStatus', 'listDecoderManifestSignalsResponse_httpStatus' - The response's http status code.
newListDecoderManifestSignalsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListDecoderManifestSignalsResponse
newListDecoderManifestSignalsResponse pHttpStatus_ =
  ListDecoderManifestSignalsResponse'
    { nextToken =
        Prelude.Nothing,
      signalDecoders = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to retrieve the next set of results, or @null@ if there are no
-- more results.
listDecoderManifestSignalsResponse_nextToken :: Lens.Lens' ListDecoderManifestSignalsResponse (Prelude.Maybe Prelude.Text)
listDecoderManifestSignalsResponse_nextToken = Lens.lens (\ListDecoderManifestSignalsResponse' {nextToken} -> nextToken) (\s@ListDecoderManifestSignalsResponse' {} a -> s {nextToken = a} :: ListDecoderManifestSignalsResponse)

-- | Information about a list of signals to decode.
listDecoderManifestSignalsResponse_signalDecoders :: Lens.Lens' ListDecoderManifestSignalsResponse (Prelude.Maybe (Prelude.NonEmpty SignalDecoder))
listDecoderManifestSignalsResponse_signalDecoders = Lens.lens (\ListDecoderManifestSignalsResponse' {signalDecoders} -> signalDecoders) (\s@ListDecoderManifestSignalsResponse' {} a -> s {signalDecoders = a} :: ListDecoderManifestSignalsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listDecoderManifestSignalsResponse_httpStatus :: Lens.Lens' ListDecoderManifestSignalsResponse Prelude.Int
listDecoderManifestSignalsResponse_httpStatus = Lens.lens (\ListDecoderManifestSignalsResponse' {httpStatus} -> httpStatus) (\s@ListDecoderManifestSignalsResponse' {} a -> s {httpStatus = a} :: ListDecoderManifestSignalsResponse)

instance
  Prelude.NFData
    ListDecoderManifestSignalsResponse
  where
  rnf ListDecoderManifestSignalsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf signalDecoders
      `Prelude.seq` Prelude.rnf httpStatus
