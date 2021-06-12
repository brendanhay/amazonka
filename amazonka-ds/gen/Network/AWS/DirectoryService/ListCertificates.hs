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

import qualified Network.AWS.Core as Core
import Network.AWS.DirectoryService.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListCertificates' smart constructor.
data ListCertificates = ListCertificates'
  { -- | A token for requesting another page of certificates if the @NextToken@
    -- response element indicates that more certificates are available. Use the
    -- value of the returned @NextToken@ element in your request until the
    -- token comes back as @null@. Pass @null@ if this is the first call.
    nextToken :: Core.Maybe Core.Text,
    -- | The number of items that should show up on one page
    limit :: Core.Maybe Core.Natural,
    -- | The identifier of the directory.
    directoryId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  ListCertificates
newListCertificates pDirectoryId_ =
  ListCertificates'
    { nextToken = Core.Nothing,
      limit = Core.Nothing,
      directoryId = pDirectoryId_
    }

-- | A token for requesting another page of certificates if the @NextToken@
-- response element indicates that more certificates are available. Use the
-- value of the returned @NextToken@ element in your request until the
-- token comes back as @null@. Pass @null@ if this is the first call.
listCertificates_nextToken :: Lens.Lens' ListCertificates (Core.Maybe Core.Text)
listCertificates_nextToken = Lens.lens (\ListCertificates' {nextToken} -> nextToken) (\s@ListCertificates' {} a -> s {nextToken = a} :: ListCertificates)

-- | The number of items that should show up on one page
listCertificates_limit :: Lens.Lens' ListCertificates (Core.Maybe Core.Natural)
listCertificates_limit = Lens.lens (\ListCertificates' {limit} -> limit) (\s@ListCertificates' {} a -> s {limit = a} :: ListCertificates)

-- | The identifier of the directory.
listCertificates_directoryId :: Lens.Lens' ListCertificates Core.Text
listCertificates_directoryId = Lens.lens (\ListCertificates' {directoryId} -> directoryId) (\s@ListCertificates' {} a -> s {directoryId = a} :: ListCertificates)

instance Core.AWSRequest ListCertificates where
  type
    AWSResponse ListCertificates =
      ListCertificatesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListCertificatesResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "CertificatesInfo" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListCertificates

instance Core.NFData ListCertificates

instance Core.ToHeaders ListCertificates where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DirectoryService_20150416.ListCertificates" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListCertificates where
  toJSON ListCertificates' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("Limit" Core..=) Core.<$> limit,
            Core.Just ("DirectoryId" Core..= directoryId)
          ]
      )

instance Core.ToPath ListCertificates where
  toPath = Core.const "/"

instance Core.ToQuery ListCertificates where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListCertificatesResponse' smart constructor.
data ListCertificatesResponse = ListCertificatesResponse'
  { -- | Indicates whether another page of certificates is available when the
    -- number of available certificates exceeds the page limit.
    nextToken :: Core.Maybe Core.Text,
    -- | A list of certificates with basic details including certificate ID,
    -- certificate common name, certificate state.
    certificatesInfo :: Core.Maybe [CertificateInfo],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  ListCertificatesResponse
newListCertificatesResponse pHttpStatus_ =
  ListCertificatesResponse'
    { nextToken = Core.Nothing,
      certificatesInfo = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Indicates whether another page of certificates is available when the
-- number of available certificates exceeds the page limit.
listCertificatesResponse_nextToken :: Lens.Lens' ListCertificatesResponse (Core.Maybe Core.Text)
listCertificatesResponse_nextToken = Lens.lens (\ListCertificatesResponse' {nextToken} -> nextToken) (\s@ListCertificatesResponse' {} a -> s {nextToken = a} :: ListCertificatesResponse)

-- | A list of certificates with basic details including certificate ID,
-- certificate common name, certificate state.
listCertificatesResponse_certificatesInfo :: Lens.Lens' ListCertificatesResponse (Core.Maybe [CertificateInfo])
listCertificatesResponse_certificatesInfo = Lens.lens (\ListCertificatesResponse' {certificatesInfo} -> certificatesInfo) (\s@ListCertificatesResponse' {} a -> s {certificatesInfo = a} :: ListCertificatesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listCertificatesResponse_httpStatus :: Lens.Lens' ListCertificatesResponse Core.Int
listCertificatesResponse_httpStatus = Lens.lens (\ListCertificatesResponse' {httpStatus} -> httpStatus) (\s@ListCertificatesResponse' {} a -> s {httpStatus = a} :: ListCertificatesResponse)

instance Core.NFData ListCertificatesResponse
