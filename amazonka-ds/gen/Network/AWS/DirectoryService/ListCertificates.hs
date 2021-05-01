{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.DirectoryService.ListCertificates
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- For the specified directory, lists all the certificates registered for a
-- secure LDAP or client certificate authentication.
module Network.AWS.DirectoryService.ListCertificates
  ( -- * Creating a Request
    ListCertificates (..),
    newListCertificates,

    -- * Request Lenses
    listCertificates_nextToken,
    listCertificates_limit,
    listCertificates_directoryId,

    -- * Destructuring the Response
    ListCertificatesResponse (..),
    newListCertificatesResponse,

    -- * Response Lenses
    listCertificatesResponse_nextToken,
    listCertificatesResponse_certificatesInfo,
    listCertificatesResponse_httpStatus,
  )
where

import Network.AWS.DirectoryService.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListCertificates' smart constructor.
data ListCertificates = ListCertificates'
  { -- | A token for requesting another page of certificates if the @NextToken@
    -- response element indicates that more certificates are available. Use the
    -- value of the returned @NextToken@ element in your request until the
    -- token comes back as @null@. Pass @null@ if this is the first call.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The number of items that should show up on one page
    limit :: Prelude.Maybe Prelude.Natural,
    -- | The identifier of the directory.
    directoryId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ListCertificates' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listCertificates_nextToken' - A token for requesting another page of certificates if the @NextToken@
-- response element indicates that more certificates are available. Use the
-- value of the returned @NextToken@ element in your request until the
-- token comes back as @null@. Pass @null@ if this is the first call.
--
-- 'limit', 'listCertificates_limit' - The number of items that should show up on one page
--
-- 'directoryId', 'listCertificates_directoryId' - The identifier of the directory.
newListCertificates ::
  -- | 'directoryId'
  Prelude.Text ->
  ListCertificates
newListCertificates pDirectoryId_ =
  ListCertificates'
    { nextToken = Prelude.Nothing,
      limit = Prelude.Nothing,
      directoryId = pDirectoryId_
    }

-- | A token for requesting another page of certificates if the @NextToken@
-- response element indicates that more certificates are available. Use the
-- value of the returned @NextToken@ element in your request until the
-- token comes back as @null@. Pass @null@ if this is the first call.
listCertificates_nextToken :: Lens.Lens' ListCertificates (Prelude.Maybe Prelude.Text)
listCertificates_nextToken = Lens.lens (\ListCertificates' {nextToken} -> nextToken) (\s@ListCertificates' {} a -> s {nextToken = a} :: ListCertificates)

-- | The number of items that should show up on one page
listCertificates_limit :: Lens.Lens' ListCertificates (Prelude.Maybe Prelude.Natural)
listCertificates_limit = Lens.lens (\ListCertificates' {limit} -> limit) (\s@ListCertificates' {} a -> s {limit = a} :: ListCertificates)

-- | The identifier of the directory.
listCertificates_directoryId :: Lens.Lens' ListCertificates Prelude.Text
listCertificates_directoryId = Lens.lens (\ListCertificates' {directoryId} -> directoryId) (\s@ListCertificates' {} a -> s {directoryId = a} :: ListCertificates)

instance Prelude.AWSRequest ListCertificates where
  type Rs ListCertificates = ListCertificatesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListCertificatesResponse'
            Prelude.<$> (x Prelude..?> "NextToken")
            Prelude.<*> ( x Prelude..?> "CertificatesInfo"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListCertificates

instance Prelude.NFData ListCertificates

instance Prelude.ToHeaders ListCertificates where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "DirectoryService_20150416.ListCertificates" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON ListCertificates where
  toJSON ListCertificates' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("NextToken" Prelude..=) Prelude.<$> nextToken,
            ("Limit" Prelude..=) Prelude.<$> limit,
            Prelude.Just ("DirectoryId" Prelude..= directoryId)
          ]
      )

instance Prelude.ToPath ListCertificates where
  toPath = Prelude.const "/"

instance Prelude.ToQuery ListCertificates where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListCertificatesResponse' smart constructor.
data ListCertificatesResponse = ListCertificatesResponse'
  { -- | Indicates whether another page of certificates is available when the
    -- number of available certificates exceeds the page limit.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of certificates with basic details including certificate ID,
    -- certificate common name, certificate state.
    certificatesInfo :: Prelude.Maybe [CertificateInfo],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ListCertificatesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listCertificatesResponse_nextToken' - Indicates whether another page of certificates is available when the
-- number of available certificates exceeds the page limit.
--
-- 'certificatesInfo', 'listCertificatesResponse_certificatesInfo' - A list of certificates with basic details including certificate ID,
-- certificate common name, certificate state.
--
-- 'httpStatus', 'listCertificatesResponse_httpStatus' - The response's http status code.
newListCertificatesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListCertificatesResponse
newListCertificatesResponse pHttpStatus_ =
  ListCertificatesResponse'
    { nextToken =
        Prelude.Nothing,
      certificatesInfo = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Indicates whether another page of certificates is available when the
-- number of available certificates exceeds the page limit.
listCertificatesResponse_nextToken :: Lens.Lens' ListCertificatesResponse (Prelude.Maybe Prelude.Text)
listCertificatesResponse_nextToken = Lens.lens (\ListCertificatesResponse' {nextToken} -> nextToken) (\s@ListCertificatesResponse' {} a -> s {nextToken = a} :: ListCertificatesResponse)

-- | A list of certificates with basic details including certificate ID,
-- certificate common name, certificate state.
listCertificatesResponse_certificatesInfo :: Lens.Lens' ListCertificatesResponse (Prelude.Maybe [CertificateInfo])
listCertificatesResponse_certificatesInfo = Lens.lens (\ListCertificatesResponse' {certificatesInfo} -> certificatesInfo) (\s@ListCertificatesResponse' {} a -> s {certificatesInfo = a} :: ListCertificatesResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
listCertificatesResponse_httpStatus :: Lens.Lens' ListCertificatesResponse Prelude.Int
listCertificatesResponse_httpStatus = Lens.lens (\ListCertificatesResponse' {httpStatus} -> httpStatus) (\s@ListCertificatesResponse' {} a -> s {httpStatus = a} :: ListCertificatesResponse)

instance Prelude.NFData ListCertificatesResponse
