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
-- Module      : Amazonka.WorkSpacesWeb.ListTrustStoreCertificates
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of trust store certificates.
module Amazonka.WorkSpacesWeb.ListTrustStoreCertificates
  ( -- * Creating a Request
    ListTrustStoreCertificates (..),
    newListTrustStoreCertificates,

    -- * Request Lenses
    listTrustStoreCertificates_nextToken,
    listTrustStoreCertificates_maxResults,
    listTrustStoreCertificates_trustStoreArn,

    -- * Destructuring the Response
    ListTrustStoreCertificatesResponse (..),
    newListTrustStoreCertificatesResponse,

    -- * Response Lenses
    listTrustStoreCertificatesResponse_trustStoreArn,
    listTrustStoreCertificatesResponse_nextToken,
    listTrustStoreCertificatesResponse_certificateList,
    listTrustStoreCertificatesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkSpacesWeb.Types

-- | /See:/ 'newListTrustStoreCertificates' smart constructor.
data ListTrustStoreCertificates = ListTrustStoreCertificates'
  { -- | The pagination token used to retrieve the next page of results for this
    -- operation.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to be included in the next page.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The ARN of the trust store
    trustStoreArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTrustStoreCertificates' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listTrustStoreCertificates_nextToken' - The pagination token used to retrieve the next page of results for this
-- operation.
--
-- 'maxResults', 'listTrustStoreCertificates_maxResults' - The maximum number of results to be included in the next page.
--
-- 'trustStoreArn', 'listTrustStoreCertificates_trustStoreArn' - The ARN of the trust store
newListTrustStoreCertificates ::
  -- | 'trustStoreArn'
  Prelude.Text ->
  ListTrustStoreCertificates
newListTrustStoreCertificates pTrustStoreArn_ =
  ListTrustStoreCertificates'
    { nextToken =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      trustStoreArn = pTrustStoreArn_
    }

-- | The pagination token used to retrieve the next page of results for this
-- operation.
listTrustStoreCertificates_nextToken :: Lens.Lens' ListTrustStoreCertificates (Prelude.Maybe Prelude.Text)
listTrustStoreCertificates_nextToken = Lens.lens (\ListTrustStoreCertificates' {nextToken} -> nextToken) (\s@ListTrustStoreCertificates' {} a -> s {nextToken = a} :: ListTrustStoreCertificates)

-- | The maximum number of results to be included in the next page.
listTrustStoreCertificates_maxResults :: Lens.Lens' ListTrustStoreCertificates (Prelude.Maybe Prelude.Natural)
listTrustStoreCertificates_maxResults = Lens.lens (\ListTrustStoreCertificates' {maxResults} -> maxResults) (\s@ListTrustStoreCertificates' {} a -> s {maxResults = a} :: ListTrustStoreCertificates)

-- | The ARN of the trust store
listTrustStoreCertificates_trustStoreArn :: Lens.Lens' ListTrustStoreCertificates Prelude.Text
listTrustStoreCertificates_trustStoreArn = Lens.lens (\ListTrustStoreCertificates' {trustStoreArn} -> trustStoreArn) (\s@ListTrustStoreCertificates' {} a -> s {trustStoreArn = a} :: ListTrustStoreCertificates)

instance Core.AWSRequest ListTrustStoreCertificates where
  type
    AWSResponse ListTrustStoreCertificates =
      ListTrustStoreCertificatesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTrustStoreCertificatesResponse'
            Prelude.<$> (x Data..?> "trustStoreArn")
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> ( x Data..?> "certificateList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListTrustStoreCertificates where
  hashWithSalt _salt ListTrustStoreCertificates' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` trustStoreArn

instance Prelude.NFData ListTrustStoreCertificates where
  rnf ListTrustStoreCertificates' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf trustStoreArn

instance Data.ToHeaders ListTrustStoreCertificates where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListTrustStoreCertificates where
  toPath ListTrustStoreCertificates' {..} =
    Prelude.mconcat
      [ "/trustStores/",
        Data.toBS trustStoreArn,
        "/certificates"
      ]

instance Data.ToQuery ListTrustStoreCertificates where
  toQuery ListTrustStoreCertificates' {..} =
    Prelude.mconcat
      [ "nextToken" Data.=: nextToken,
        "maxResults" Data.=: maxResults
      ]

-- | /See:/ 'newListTrustStoreCertificatesResponse' smart constructor.
data ListTrustStoreCertificatesResponse = ListTrustStoreCertificatesResponse'
  { -- | The ARN of the trust store.
    trustStoreArn :: Prelude.Maybe Prelude.Text,
    -- | The pagination token used to retrieve the next page of results for this
    -- operation.>
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The certificate list.
    certificateList :: Prelude.Maybe [CertificateSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTrustStoreCertificatesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'trustStoreArn', 'listTrustStoreCertificatesResponse_trustStoreArn' - The ARN of the trust store.
--
-- 'nextToken', 'listTrustStoreCertificatesResponse_nextToken' - The pagination token used to retrieve the next page of results for this
-- operation.>
--
-- 'certificateList', 'listTrustStoreCertificatesResponse_certificateList' - The certificate list.
--
-- 'httpStatus', 'listTrustStoreCertificatesResponse_httpStatus' - The response's http status code.
newListTrustStoreCertificatesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListTrustStoreCertificatesResponse
newListTrustStoreCertificatesResponse pHttpStatus_ =
  ListTrustStoreCertificatesResponse'
    { trustStoreArn =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      certificateList = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the trust store.
listTrustStoreCertificatesResponse_trustStoreArn :: Lens.Lens' ListTrustStoreCertificatesResponse (Prelude.Maybe Prelude.Text)
listTrustStoreCertificatesResponse_trustStoreArn = Lens.lens (\ListTrustStoreCertificatesResponse' {trustStoreArn} -> trustStoreArn) (\s@ListTrustStoreCertificatesResponse' {} a -> s {trustStoreArn = a} :: ListTrustStoreCertificatesResponse)

-- | The pagination token used to retrieve the next page of results for this
-- operation.>
listTrustStoreCertificatesResponse_nextToken :: Lens.Lens' ListTrustStoreCertificatesResponse (Prelude.Maybe Prelude.Text)
listTrustStoreCertificatesResponse_nextToken = Lens.lens (\ListTrustStoreCertificatesResponse' {nextToken} -> nextToken) (\s@ListTrustStoreCertificatesResponse' {} a -> s {nextToken = a} :: ListTrustStoreCertificatesResponse)

-- | The certificate list.
listTrustStoreCertificatesResponse_certificateList :: Lens.Lens' ListTrustStoreCertificatesResponse (Prelude.Maybe [CertificateSummary])
listTrustStoreCertificatesResponse_certificateList = Lens.lens (\ListTrustStoreCertificatesResponse' {certificateList} -> certificateList) (\s@ListTrustStoreCertificatesResponse' {} a -> s {certificateList = a} :: ListTrustStoreCertificatesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listTrustStoreCertificatesResponse_httpStatus :: Lens.Lens' ListTrustStoreCertificatesResponse Prelude.Int
listTrustStoreCertificatesResponse_httpStatus = Lens.lens (\ListTrustStoreCertificatesResponse' {httpStatus} -> httpStatus) (\s@ListTrustStoreCertificatesResponse' {} a -> s {httpStatus = a} :: ListTrustStoreCertificatesResponse)

instance
  Prelude.NFData
    ListTrustStoreCertificatesResponse
  where
  rnf ListTrustStoreCertificatesResponse' {..} =
    Prelude.rnf trustStoreArn
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf certificateList
      `Prelude.seq` Prelude.rnf httpStatus
