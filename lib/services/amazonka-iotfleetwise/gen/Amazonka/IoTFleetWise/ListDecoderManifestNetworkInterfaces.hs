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
-- Module      : Amazonka.IoTFleetWise.ListDecoderManifestNetworkInterfaces
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the network interfaces specified in a decoder manifest.
--
-- This API operation uses pagination. Specify the @nextToken@ parameter in
-- the request to return more results.
--
-- This operation returns paginated results.
module Amazonka.IoTFleetWise.ListDecoderManifestNetworkInterfaces
  ( -- * Creating a Request
    ListDecoderManifestNetworkInterfaces (..),
    newListDecoderManifestNetworkInterfaces,

    -- * Request Lenses
    listDecoderManifestNetworkInterfaces_maxResults,
    listDecoderManifestNetworkInterfaces_nextToken,
    listDecoderManifestNetworkInterfaces_name,

    -- * Destructuring the Response
    ListDecoderManifestNetworkInterfacesResponse (..),
    newListDecoderManifestNetworkInterfacesResponse,

    -- * Response Lenses
    listDecoderManifestNetworkInterfacesResponse_networkInterfaces,
    listDecoderManifestNetworkInterfacesResponse_nextToken,
    listDecoderManifestNetworkInterfacesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTFleetWise.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListDecoderManifestNetworkInterfaces' smart constructor.
data ListDecoderManifestNetworkInterfaces = ListDecoderManifestNetworkInterfaces'
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
-- Create a value of 'ListDecoderManifestNetworkInterfaces' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listDecoderManifestNetworkInterfaces_maxResults' - The maximum number of items to return, between 1 and 100, inclusive.
--
-- 'nextToken', 'listDecoderManifestNetworkInterfaces_nextToken' - A pagination token for the next set of results.
--
-- If the results of a search are large, only a portion of the results are
-- returned, and a @nextToken@ pagination token is returned in the
-- response. To retrieve the next set of results, reissue the search
-- request and include the returned token. When all results have been
-- returned, the response does not contain a pagination token value.
--
-- 'name', 'listDecoderManifestNetworkInterfaces_name' - The name of the decoder manifest to list information about.
newListDecoderManifestNetworkInterfaces ::
  -- | 'name'
  Prelude.Text ->
  ListDecoderManifestNetworkInterfaces
newListDecoderManifestNetworkInterfaces pName_ =
  ListDecoderManifestNetworkInterfaces'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      name = pName_
    }

-- | The maximum number of items to return, between 1 and 100, inclusive.
listDecoderManifestNetworkInterfaces_maxResults :: Lens.Lens' ListDecoderManifestNetworkInterfaces (Prelude.Maybe Prelude.Natural)
listDecoderManifestNetworkInterfaces_maxResults = Lens.lens (\ListDecoderManifestNetworkInterfaces' {maxResults} -> maxResults) (\s@ListDecoderManifestNetworkInterfaces' {} a -> s {maxResults = a} :: ListDecoderManifestNetworkInterfaces)

-- | A pagination token for the next set of results.
--
-- If the results of a search are large, only a portion of the results are
-- returned, and a @nextToken@ pagination token is returned in the
-- response. To retrieve the next set of results, reissue the search
-- request and include the returned token. When all results have been
-- returned, the response does not contain a pagination token value.
listDecoderManifestNetworkInterfaces_nextToken :: Lens.Lens' ListDecoderManifestNetworkInterfaces (Prelude.Maybe Prelude.Text)
listDecoderManifestNetworkInterfaces_nextToken = Lens.lens (\ListDecoderManifestNetworkInterfaces' {nextToken} -> nextToken) (\s@ListDecoderManifestNetworkInterfaces' {} a -> s {nextToken = a} :: ListDecoderManifestNetworkInterfaces)

-- | The name of the decoder manifest to list information about.
listDecoderManifestNetworkInterfaces_name :: Lens.Lens' ListDecoderManifestNetworkInterfaces Prelude.Text
listDecoderManifestNetworkInterfaces_name = Lens.lens (\ListDecoderManifestNetworkInterfaces' {name} -> name) (\s@ListDecoderManifestNetworkInterfaces' {} a -> s {name = a} :: ListDecoderManifestNetworkInterfaces)

instance
  Core.AWSPager
    ListDecoderManifestNetworkInterfaces
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listDecoderManifestNetworkInterfacesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listDecoderManifestNetworkInterfacesResponse_networkInterfaces
              Prelude.. Lens._Just
              Prelude.. Lens.to Prelude.toList
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listDecoderManifestNetworkInterfaces_nextToken
          Lens..~ rs
            Lens.^? listDecoderManifestNetworkInterfacesResponse_nextToken
              Prelude.. Lens._Just

instance
  Core.AWSRequest
    ListDecoderManifestNetworkInterfaces
  where
  type
    AWSResponse ListDecoderManifestNetworkInterfaces =
      ListDecoderManifestNetworkInterfacesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDecoderManifestNetworkInterfacesResponse'
            Prelude.<$> (x Data..?> "networkInterfaces")
              Prelude.<*> (x Data..?> "nextToken")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListDecoderManifestNetworkInterfaces
  where
  hashWithSalt
    _salt
    ListDecoderManifestNetworkInterfaces' {..} =
      _salt `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` name

instance
  Prelude.NFData
    ListDecoderManifestNetworkInterfaces
  where
  rnf ListDecoderManifestNetworkInterfaces' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf name

instance
  Data.ToHeaders
    ListDecoderManifestNetworkInterfaces
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "IoTAutobahnControlPlane.ListDecoderManifestNetworkInterfaces" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    ListDecoderManifestNetworkInterfaces
  where
  toJSON ListDecoderManifestNetworkInterfaces' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just ("name" Data..= name)
          ]
      )

instance
  Data.ToPath
    ListDecoderManifestNetworkInterfaces
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    ListDecoderManifestNetworkInterfaces
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListDecoderManifestNetworkInterfacesResponse' smart constructor.
data ListDecoderManifestNetworkInterfacesResponse = ListDecoderManifestNetworkInterfacesResponse'
  { -- | A list of information about network interfaces.
    networkInterfaces :: Prelude.Maybe (Prelude.NonEmpty NetworkInterface),
    -- | The token to retrieve the next set of results, or @null@ if there are no
    -- more results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDecoderManifestNetworkInterfacesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'networkInterfaces', 'listDecoderManifestNetworkInterfacesResponse_networkInterfaces' - A list of information about network interfaces.
--
-- 'nextToken', 'listDecoderManifestNetworkInterfacesResponse_nextToken' - The token to retrieve the next set of results, or @null@ if there are no
-- more results.
--
-- 'httpStatus', 'listDecoderManifestNetworkInterfacesResponse_httpStatus' - The response's http status code.
newListDecoderManifestNetworkInterfacesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListDecoderManifestNetworkInterfacesResponse
newListDecoderManifestNetworkInterfacesResponse
  pHttpStatus_ =
    ListDecoderManifestNetworkInterfacesResponse'
      { networkInterfaces =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | A list of information about network interfaces.
listDecoderManifestNetworkInterfacesResponse_networkInterfaces :: Lens.Lens' ListDecoderManifestNetworkInterfacesResponse (Prelude.Maybe (Prelude.NonEmpty NetworkInterface))
listDecoderManifestNetworkInterfacesResponse_networkInterfaces = Lens.lens (\ListDecoderManifestNetworkInterfacesResponse' {networkInterfaces} -> networkInterfaces) (\s@ListDecoderManifestNetworkInterfacesResponse' {} a -> s {networkInterfaces = a} :: ListDecoderManifestNetworkInterfacesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token to retrieve the next set of results, or @null@ if there are no
-- more results.
listDecoderManifestNetworkInterfacesResponse_nextToken :: Lens.Lens' ListDecoderManifestNetworkInterfacesResponse (Prelude.Maybe Prelude.Text)
listDecoderManifestNetworkInterfacesResponse_nextToken = Lens.lens (\ListDecoderManifestNetworkInterfacesResponse' {nextToken} -> nextToken) (\s@ListDecoderManifestNetworkInterfacesResponse' {} a -> s {nextToken = a} :: ListDecoderManifestNetworkInterfacesResponse)

-- | The response's http status code.
listDecoderManifestNetworkInterfacesResponse_httpStatus :: Lens.Lens' ListDecoderManifestNetworkInterfacesResponse Prelude.Int
listDecoderManifestNetworkInterfacesResponse_httpStatus = Lens.lens (\ListDecoderManifestNetworkInterfacesResponse' {httpStatus} -> httpStatus) (\s@ListDecoderManifestNetworkInterfacesResponse' {} a -> s {httpStatus = a} :: ListDecoderManifestNetworkInterfacesResponse)

instance
  Prelude.NFData
    ListDecoderManifestNetworkInterfacesResponse
  where
  rnf ListDecoderManifestNetworkInterfacesResponse' {..} =
    Prelude.rnf networkInterfaces
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
