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
-- Module      : Amazonka.DataExchange.SendApiAsset
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation invokes an API Gateway API asset. The request is proxied
-- to the provider’s API Gateway API.
module Amazonka.DataExchange.SendApiAsset
  ( -- * Creating a Request
    SendApiAsset (..),
    newSendApiAsset,

    -- * Request Lenses
    sendApiAsset_body,
    sendApiAsset_method,
    sendApiAsset_path,
    sendApiAsset_queryStringParameters,
    sendApiAsset_requestHeaders,
    sendApiAsset_assetId,
    sendApiAsset_dataSetId,
    sendApiAsset_revisionId,

    -- * Destructuring the Response
    SendApiAssetResponse (..),
    newSendApiAssetResponse,

    -- * Response Lenses
    sendApiAssetResponse_body,
    sendApiAssetResponse_responseHeaders,
    sendApiAssetResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataExchange.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newSendApiAsset' smart constructor.
data SendApiAsset = SendApiAsset'
  { -- | The request body.
    body :: Prelude.Maybe Prelude.ByteString,
    -- | HTTP method value for the API request. Alternatively, you can use the
    -- appropriate verb in your request.
    method :: Prelude.Maybe Prelude.Text,
    -- | URI path value for the API request. Alternatively, you can set the URI
    -- path directly by invoking \/v1\/{pathValue}.
    path :: Prelude.Maybe Prelude.Text,
    -- | Attach query string parameters to the end of the URI (for example,
    -- \/v1\/examplePath?exampleParam=exampleValue).
    queryStringParameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Any header value prefixed with x-amzn-dataexchange-header- will have
    -- that stripped before sending the Asset API request. Use this when you
    -- want to override a header that AWS Data Exchange uses. Alternatively,
    -- you can use the header without a prefix to the HTTP request.
    requestHeaders :: Prelude.HashMap Prelude.Text Prelude.Text,
    -- | Asset ID value for the API request.
    assetId :: Prelude.Text,
    -- | Data set ID value for the API request.
    dataSetId :: Prelude.Text,
    -- | Revision ID value for the API request.
    revisionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SendApiAsset' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'body', 'sendApiAsset_body' - The request body.
--
-- 'method', 'sendApiAsset_method' - HTTP method value for the API request. Alternatively, you can use the
-- appropriate verb in your request.
--
-- 'path', 'sendApiAsset_path' - URI path value for the API request. Alternatively, you can set the URI
-- path directly by invoking \/v1\/{pathValue}.
--
-- 'queryStringParameters', 'sendApiAsset_queryStringParameters' - Attach query string parameters to the end of the URI (for example,
-- \/v1\/examplePath?exampleParam=exampleValue).
--
-- 'requestHeaders', 'sendApiAsset_requestHeaders' - Any header value prefixed with x-amzn-dataexchange-header- will have
-- that stripped before sending the Asset API request. Use this when you
-- want to override a header that AWS Data Exchange uses. Alternatively,
-- you can use the header without a prefix to the HTTP request.
--
-- 'assetId', 'sendApiAsset_assetId' - Asset ID value for the API request.
--
-- 'dataSetId', 'sendApiAsset_dataSetId' - Data set ID value for the API request.
--
-- 'revisionId', 'sendApiAsset_revisionId' - Revision ID value for the API request.
newSendApiAsset ::
  -- | 'assetId'
  Prelude.Text ->
  -- | 'dataSetId'
  Prelude.Text ->
  -- | 'revisionId'
  Prelude.Text ->
  SendApiAsset
newSendApiAsset pAssetId_ pDataSetId_ pRevisionId_ =
  SendApiAsset'
    { body = Prelude.Nothing,
      method = Prelude.Nothing,
      path = Prelude.Nothing,
      queryStringParameters = Prelude.Nothing,
      requestHeaders = Prelude.mempty,
      assetId = pAssetId_,
      dataSetId = pDataSetId_,
      revisionId = pRevisionId_
    }

-- | The request body.
sendApiAsset_body :: Lens.Lens' SendApiAsset (Prelude.Maybe Prelude.ByteString)
sendApiAsset_body = Lens.lens (\SendApiAsset' {body} -> body) (\s@SendApiAsset' {} a -> s {body = a} :: SendApiAsset)

-- | HTTP method value for the API request. Alternatively, you can use the
-- appropriate verb in your request.
sendApiAsset_method :: Lens.Lens' SendApiAsset (Prelude.Maybe Prelude.Text)
sendApiAsset_method = Lens.lens (\SendApiAsset' {method} -> method) (\s@SendApiAsset' {} a -> s {method = a} :: SendApiAsset)

-- | URI path value for the API request. Alternatively, you can set the URI
-- path directly by invoking \/v1\/{pathValue}.
sendApiAsset_path :: Lens.Lens' SendApiAsset (Prelude.Maybe Prelude.Text)
sendApiAsset_path = Lens.lens (\SendApiAsset' {path} -> path) (\s@SendApiAsset' {} a -> s {path = a} :: SendApiAsset)

-- | Attach query string parameters to the end of the URI (for example,
-- \/v1\/examplePath?exampleParam=exampleValue).
sendApiAsset_queryStringParameters :: Lens.Lens' SendApiAsset (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
sendApiAsset_queryStringParameters = Lens.lens (\SendApiAsset' {queryStringParameters} -> queryStringParameters) (\s@SendApiAsset' {} a -> s {queryStringParameters = a} :: SendApiAsset) Prelude.. Lens.mapping Lens.coerced

-- | Any header value prefixed with x-amzn-dataexchange-header- will have
-- that stripped before sending the Asset API request. Use this when you
-- want to override a header that AWS Data Exchange uses. Alternatively,
-- you can use the header without a prefix to the HTTP request.
sendApiAsset_requestHeaders :: Lens.Lens' SendApiAsset (Prelude.HashMap Prelude.Text Prelude.Text)
sendApiAsset_requestHeaders = Lens.lens (\SendApiAsset' {requestHeaders} -> requestHeaders) (\s@SendApiAsset' {} a -> s {requestHeaders = a} :: SendApiAsset) Prelude.. Lens.coerced

-- | Asset ID value for the API request.
sendApiAsset_assetId :: Lens.Lens' SendApiAsset Prelude.Text
sendApiAsset_assetId = Lens.lens (\SendApiAsset' {assetId} -> assetId) (\s@SendApiAsset' {} a -> s {assetId = a} :: SendApiAsset)

-- | Data set ID value for the API request.
sendApiAsset_dataSetId :: Lens.Lens' SendApiAsset Prelude.Text
sendApiAsset_dataSetId = Lens.lens (\SendApiAsset' {dataSetId} -> dataSetId) (\s@SendApiAsset' {} a -> s {dataSetId = a} :: SendApiAsset)

-- | Revision ID value for the API request.
sendApiAsset_revisionId :: Lens.Lens' SendApiAsset Prelude.Text
sendApiAsset_revisionId = Lens.lens (\SendApiAsset' {revisionId} -> revisionId) (\s@SendApiAsset' {} a -> s {revisionId = a} :: SendApiAsset)

instance Core.AWSRequest SendApiAsset where
  type AWSResponse SendApiAsset = SendApiAssetResponse
  request overrides =
    Request.postBody (overrides defaultService)
  response =
    Response.receiveBytes
      ( \s h x ->
          SendApiAssetResponse'
            Prelude.<$> (Prelude.pure (Prelude.Just (Prelude.coerce x)))
            Prelude.<*> (Data.parseHeadersMap "" h)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable SendApiAsset where
  hashWithSalt _salt SendApiAsset' {..} =
    _salt
      `Prelude.hashWithSalt` body
      `Prelude.hashWithSalt` method
      `Prelude.hashWithSalt` path
      `Prelude.hashWithSalt` queryStringParameters
      `Prelude.hashWithSalt` requestHeaders
      `Prelude.hashWithSalt` assetId
      `Prelude.hashWithSalt` dataSetId
      `Prelude.hashWithSalt` revisionId

instance Prelude.NFData SendApiAsset where
  rnf SendApiAsset' {..} =
    Prelude.rnf body
      `Prelude.seq` Prelude.rnf method
      `Prelude.seq` Prelude.rnf path
      `Prelude.seq` Prelude.rnf queryStringParameters
      `Prelude.seq` Prelude.rnf requestHeaders
      `Prelude.seq` Prelude.rnf assetId
      `Prelude.seq` Prelude.rnf dataSetId
      `Prelude.seq` Prelude.rnf revisionId

instance Data.ToBody SendApiAsset where
  toBody SendApiAsset' {..} = Data.toBody body

instance Data.ToHeaders SendApiAsset where
  toHeaders SendApiAsset' {..} =
    Prelude.mconcat
      [ "x-amzn-dataexchange-http-method" Data.=# method,
        "x-amzn-dataexchange-path" Data.=# path,
        "x-amzn-dataexchange-header-" Data.=# requestHeaders,
        "x-amzn-dataexchange-asset-id" Data.=# assetId,
        "x-amzn-dataexchange-data-set-id" Data.=# dataSetId,
        "x-amzn-dataexchange-revision-id" Data.=# revisionId,
        "Content-Type"
          Data.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance Data.ToPath SendApiAsset where
  toPath = Prelude.const "/v1"

instance Data.ToQuery SendApiAsset where
  toQuery SendApiAsset' {..} =
    Prelude.mconcat
      [ "QueryStringParameters"
          Data.=: Data.toQuery
            ( Data.toQueryMap "entry" "key" "value"
                Prelude.<$> queryStringParameters
            )
      ]

-- | /See:/ 'newSendApiAssetResponse' smart constructor.
data SendApiAssetResponse = SendApiAssetResponse'
  { -- | The response body from the underlying API tracked by the API asset.
    body :: Prelude.Maybe Prelude.ByteString,
    -- | The response headers from the underlying API tracked by the API asset.
    responseHeaders :: Prelude.HashMap Prelude.Text Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SendApiAssetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'body', 'sendApiAssetResponse_body' - The response body from the underlying API tracked by the API asset.
--
-- 'responseHeaders', 'sendApiAssetResponse_responseHeaders' - The response headers from the underlying API tracked by the API asset.
--
-- 'httpStatus', 'sendApiAssetResponse_httpStatus' - The response's http status code.
newSendApiAssetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  SendApiAssetResponse
newSendApiAssetResponse pHttpStatus_ =
  SendApiAssetResponse'
    { body = Prelude.Nothing,
      responseHeaders = Prelude.mempty,
      httpStatus = pHttpStatus_
    }

-- | The response body from the underlying API tracked by the API asset.
sendApiAssetResponse_body :: Lens.Lens' SendApiAssetResponse (Prelude.Maybe Prelude.ByteString)
sendApiAssetResponse_body = Lens.lens (\SendApiAssetResponse' {body} -> body) (\s@SendApiAssetResponse' {} a -> s {body = a} :: SendApiAssetResponse)

-- | The response headers from the underlying API tracked by the API asset.
sendApiAssetResponse_responseHeaders :: Lens.Lens' SendApiAssetResponse (Prelude.HashMap Prelude.Text Prelude.Text)
sendApiAssetResponse_responseHeaders = Lens.lens (\SendApiAssetResponse' {responseHeaders} -> responseHeaders) (\s@SendApiAssetResponse' {} a -> s {responseHeaders = a} :: SendApiAssetResponse) Prelude.. Lens.coerced

-- | The response's http status code.
sendApiAssetResponse_httpStatus :: Lens.Lens' SendApiAssetResponse Prelude.Int
sendApiAssetResponse_httpStatus = Lens.lens (\SendApiAssetResponse' {httpStatus} -> httpStatus) (\s@SendApiAssetResponse' {} a -> s {httpStatus = a} :: SendApiAssetResponse)

instance Prelude.NFData SendApiAssetResponse where
  rnf SendApiAssetResponse' {..} =
    Prelude.rnf body
      `Prelude.seq` Prelude.rnf responseHeaders
      `Prelude.seq` Prelude.rnf httpStatus
