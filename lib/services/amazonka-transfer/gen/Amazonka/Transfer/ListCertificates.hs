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
-- Module      : Amazonka.Transfer.ListCertificates
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of the current certificates that have been imported into
-- Transfer Family. If you want to limit the results to a certain number,
-- supply a value for the @MaxResults@ parameter. If you ran the command
-- previously and received a value for the @NextToken@ parameter, you can
-- supply that value to continue listing certificates from where you left
-- off.
--
-- This operation returns paginated results.
module Amazonka.Transfer.ListCertificates
  ( -- * Creating a Request
    ListCertificates (..),
    newListCertificates,

    -- * Request Lenses
    listCertificates_maxResults,
    listCertificates_nextToken,

    -- * Destructuring the Response
    ListCertificatesResponse (..),
    newListCertificatesResponse,

    -- * Response Lenses
    listCertificatesResponse_nextToken,
    listCertificatesResponse_httpStatus,
    listCertificatesResponse_certificates,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Transfer.Types

-- | /See:/ 'newListCertificates' smart constructor.
data ListCertificates = ListCertificates'
  { -- | The maximum number of certificates to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | When you can get additional results from the @ListCertificates@ call, a
    -- @NextToken@ parameter is returned in the output. You can then pass in a
    -- subsequent command to the @NextToken@ parameter to continue listing
    -- additional certificates.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListCertificates' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listCertificates_maxResults' - The maximum number of certificates to return.
--
-- 'nextToken', 'listCertificates_nextToken' - When you can get additional results from the @ListCertificates@ call, a
-- @NextToken@ parameter is returned in the output. You can then pass in a
-- subsequent command to the @NextToken@ parameter to continue listing
-- additional certificates.
newListCertificates ::
  ListCertificates
newListCertificates =
  ListCertificates'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The maximum number of certificates to return.
listCertificates_maxResults :: Lens.Lens' ListCertificates (Prelude.Maybe Prelude.Natural)
listCertificates_maxResults = Lens.lens (\ListCertificates' {maxResults} -> maxResults) (\s@ListCertificates' {} a -> s {maxResults = a} :: ListCertificates)

-- | When you can get additional results from the @ListCertificates@ call, a
-- @NextToken@ parameter is returned in the output. You can then pass in a
-- subsequent command to the @NextToken@ parameter to continue listing
-- additional certificates.
listCertificates_nextToken :: Lens.Lens' ListCertificates (Prelude.Maybe Prelude.Text)
listCertificates_nextToken = Lens.lens (\ListCertificates' {nextToken} -> nextToken) (\s@ListCertificates' {} a -> s {nextToken = a} :: ListCertificates)

instance Core.AWSPager ListCertificates where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listCertificatesResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        (rs Lens.^. listCertificatesResponse_certificates) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listCertificates_nextToken
          Lens..~ rs
          Lens.^? listCertificatesResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListCertificates where
  type
    AWSResponse ListCertificates =
      ListCertificatesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListCertificatesResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "Certificates" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable ListCertificates where
  hashWithSalt _salt ListCertificates' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListCertificates where
  rnf ListCertificates' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListCertificates where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "TransferService.ListCertificates" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListCertificates where
  toJSON ListCertificates' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListCertificates where
  toPath = Prelude.const "/"

instance Data.ToQuery ListCertificates where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListCertificatesResponse' smart constructor.
data ListCertificatesResponse = ListCertificatesResponse'
  { -- | Returns the next token, which you can use to list the next certificate.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Returns an array of the certificates that are specified in the
    -- @ListCertificates@ call.
    certificates :: [ListedCertificate]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListCertificatesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listCertificatesResponse_nextToken' - Returns the next token, which you can use to list the next certificate.
--
-- 'httpStatus', 'listCertificatesResponse_httpStatus' - The response's http status code.
--
-- 'certificates', 'listCertificatesResponse_certificates' - Returns an array of the certificates that are specified in the
-- @ListCertificates@ call.
newListCertificatesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListCertificatesResponse
newListCertificatesResponse pHttpStatus_ =
  ListCertificatesResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      certificates = Prelude.mempty
    }

-- | Returns the next token, which you can use to list the next certificate.
listCertificatesResponse_nextToken :: Lens.Lens' ListCertificatesResponse (Prelude.Maybe Prelude.Text)
listCertificatesResponse_nextToken = Lens.lens (\ListCertificatesResponse' {nextToken} -> nextToken) (\s@ListCertificatesResponse' {} a -> s {nextToken = a} :: ListCertificatesResponse)

-- | The response's http status code.
listCertificatesResponse_httpStatus :: Lens.Lens' ListCertificatesResponse Prelude.Int
listCertificatesResponse_httpStatus = Lens.lens (\ListCertificatesResponse' {httpStatus} -> httpStatus) (\s@ListCertificatesResponse' {} a -> s {httpStatus = a} :: ListCertificatesResponse)

-- | Returns an array of the certificates that are specified in the
-- @ListCertificates@ call.
listCertificatesResponse_certificates :: Lens.Lens' ListCertificatesResponse [ListedCertificate]
listCertificatesResponse_certificates = Lens.lens (\ListCertificatesResponse' {certificates} -> certificates) (\s@ListCertificatesResponse' {} a -> s {certificates = a} :: ListCertificatesResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListCertificatesResponse where
  rnf ListCertificatesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf certificates
