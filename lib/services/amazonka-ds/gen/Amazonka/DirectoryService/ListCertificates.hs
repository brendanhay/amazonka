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
-- Module      : Amazonka.DirectoryService.ListCertificates
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- For the specified directory, lists all the certificates registered for a
-- secure LDAP or client certificate authentication.
--
-- This operation returns paginated results.
module Amazonka.DirectoryService.ListCertificates
  ( -- * Creating a Request
    ListCertificates (..),
    newListCertificates,

    -- * Request Lenses
    listCertificates_limit,
    listCertificates_nextToken,
    listCertificates_directoryId,

    -- * Destructuring the Response
    ListCertificatesResponse (..),
    newListCertificatesResponse,

    -- * Response Lenses
    listCertificatesResponse_certificatesInfo,
    listCertificatesResponse_nextToken,
    listCertificatesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DirectoryService.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListCertificates' smart constructor.
data ListCertificates = ListCertificates'
  { -- | The number of items that should show up on one page
    limit :: Prelude.Maybe Prelude.Natural,
    -- | A token for requesting another page of certificates if the @NextToken@
    -- response element indicates that more certificates are available. Use the
    -- value of the returned @NextToken@ element in your request until the
    -- token comes back as @null@. Pass @null@ if this is the first call.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the directory.
    directoryId :: Prelude.Text
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
-- 'limit', 'listCertificates_limit' - The number of items that should show up on one page
--
-- 'nextToken', 'listCertificates_nextToken' - A token for requesting another page of certificates if the @NextToken@
-- response element indicates that more certificates are available. Use the
-- value of the returned @NextToken@ element in your request until the
-- token comes back as @null@. Pass @null@ if this is the first call.
--
-- 'directoryId', 'listCertificates_directoryId' - The identifier of the directory.
newListCertificates ::
  -- | 'directoryId'
  Prelude.Text ->
  ListCertificates
newListCertificates pDirectoryId_ =
  ListCertificates'
    { limit = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      directoryId = pDirectoryId_
    }

-- | The number of items that should show up on one page
listCertificates_limit :: Lens.Lens' ListCertificates (Prelude.Maybe Prelude.Natural)
listCertificates_limit = Lens.lens (\ListCertificates' {limit} -> limit) (\s@ListCertificates' {} a -> s {limit = a} :: ListCertificates)

-- | A token for requesting another page of certificates if the @NextToken@
-- response element indicates that more certificates are available. Use the
-- value of the returned @NextToken@ element in your request until the
-- token comes back as @null@. Pass @null@ if this is the first call.
listCertificates_nextToken :: Lens.Lens' ListCertificates (Prelude.Maybe Prelude.Text)
listCertificates_nextToken = Lens.lens (\ListCertificates' {nextToken} -> nextToken) (\s@ListCertificates' {} a -> s {nextToken = a} :: ListCertificates)

-- | The identifier of the directory.
listCertificates_directoryId :: Lens.Lens' ListCertificates Prelude.Text
listCertificates_directoryId = Lens.lens (\ListCertificates' {directoryId} -> directoryId) (\s@ListCertificates' {} a -> s {directoryId = a} :: ListCertificates)

instance Core.AWSPager ListCertificates where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listCertificatesResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listCertificatesResponse_certificatesInfo
            Prelude.. Lens._Just
        ) =
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
            Prelude.<$> ( x
                            Data..?> "CertificatesInfo"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListCertificates where
  hashWithSalt _salt ListCertificates' {..} =
    _salt
      `Prelude.hashWithSalt` limit
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` directoryId

instance Prelude.NFData ListCertificates where
  rnf ListCertificates' {..} =
    Prelude.rnf limit
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf directoryId

instance Data.ToHeaders ListCertificates where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "DirectoryService_20150416.ListCertificates" ::
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
          [ ("Limit" Data..=) Prelude.<$> limit,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just ("DirectoryId" Data..= directoryId)
          ]
      )

instance Data.ToPath ListCertificates where
  toPath = Prelude.const "/"

instance Data.ToQuery ListCertificates where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListCertificatesResponse' smart constructor.
data ListCertificatesResponse = ListCertificatesResponse'
  { -- | A list of certificates with basic details including certificate ID,
    -- certificate common name, certificate state.
    certificatesInfo :: Prelude.Maybe [CertificateInfo],
    -- | Indicates whether another page of certificates is available when the
    -- number of available certificates exceeds the page limit.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
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
-- 'certificatesInfo', 'listCertificatesResponse_certificatesInfo' - A list of certificates with basic details including certificate ID,
-- certificate common name, certificate state.
--
-- 'nextToken', 'listCertificatesResponse_nextToken' - Indicates whether another page of certificates is available when the
-- number of available certificates exceeds the page limit.
--
-- 'httpStatus', 'listCertificatesResponse_httpStatus' - The response's http status code.
newListCertificatesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListCertificatesResponse
newListCertificatesResponse pHttpStatus_ =
  ListCertificatesResponse'
    { certificatesInfo =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of certificates with basic details including certificate ID,
-- certificate common name, certificate state.
listCertificatesResponse_certificatesInfo :: Lens.Lens' ListCertificatesResponse (Prelude.Maybe [CertificateInfo])
listCertificatesResponse_certificatesInfo = Lens.lens (\ListCertificatesResponse' {certificatesInfo} -> certificatesInfo) (\s@ListCertificatesResponse' {} a -> s {certificatesInfo = a} :: ListCertificatesResponse) Prelude.. Lens.mapping Lens.coerced

-- | Indicates whether another page of certificates is available when the
-- number of available certificates exceeds the page limit.
listCertificatesResponse_nextToken :: Lens.Lens' ListCertificatesResponse (Prelude.Maybe Prelude.Text)
listCertificatesResponse_nextToken = Lens.lens (\ListCertificatesResponse' {nextToken} -> nextToken) (\s@ListCertificatesResponse' {} a -> s {nextToken = a} :: ListCertificatesResponse)

-- | The response's http status code.
listCertificatesResponse_httpStatus :: Lens.Lens' ListCertificatesResponse Prelude.Int
listCertificatesResponse_httpStatus = Lens.lens (\ListCertificatesResponse' {httpStatus} -> httpStatus) (\s@ListCertificatesResponse' {} a -> s {httpStatus = a} :: ListCertificatesResponse)

instance Prelude.NFData ListCertificatesResponse where
  rnf ListCertificatesResponse' {..} =
    Prelude.rnf certificatesInfo
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
