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
-- Module      : Network.AWS.CertificateManager.ListCertificates
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of certificate ARNs and domain names. You can request
-- that only certificates that match a specific status be listed. You can
-- also filter by specific attributes of the certificate. Default filtering
-- returns only @RSA_2048@ certificates. For more information, see Filters.
--
-- This operation returns paginated results.
module Network.AWS.CertificateManager.ListCertificates
  ( -- * Creating a Request
    ListCertificates (..),
    newListCertificates,

    -- * Request Lenses
    listCertificates_nextToken,
    listCertificates_includes,
    listCertificates_certificateStatuses,
    listCertificates_maxItems,

    -- * Destructuring the Response
    ListCertificatesResponse (..),
    newListCertificatesResponse,

    -- * Response Lenses
    listCertificatesResponse_nextToken,
    listCertificatesResponse_certificateSummaryList,
    listCertificatesResponse_httpStatus,
  )
where

import Network.AWS.CertificateManager.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListCertificates' smart constructor.
data ListCertificates = ListCertificates'
  { -- | Use this parameter only when paginating results and only in a subsequent
    -- request after you receive a response with truncated results. Set it to
    -- the value of @NextToken@ from the response you just received.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Filter the certificate list. For more information, see the Filters
    -- structure.
    includes :: Prelude.Maybe Filters,
    -- | Filter the certificate list by status value.
    certificateStatuses :: Prelude.Maybe [CertificateStatus],
    -- | Use this parameter when paginating results to specify the maximum number
    -- of items to return in the response. If additional items exist beyond the
    -- number you specify, the @NextToken@ element is sent in the response. Use
    -- this @NextToken@ value in a subsequent request to retrieve additional
    -- items.
    maxItems :: Prelude.Maybe Prelude.Natural
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
-- 'nextToken', 'listCertificates_nextToken' - Use this parameter only when paginating results and only in a subsequent
-- request after you receive a response with truncated results. Set it to
-- the value of @NextToken@ from the response you just received.
--
-- 'includes', 'listCertificates_includes' - Filter the certificate list. For more information, see the Filters
-- structure.
--
-- 'certificateStatuses', 'listCertificates_certificateStatuses' - Filter the certificate list by status value.
--
-- 'maxItems', 'listCertificates_maxItems' - Use this parameter when paginating results to specify the maximum number
-- of items to return in the response. If additional items exist beyond the
-- number you specify, the @NextToken@ element is sent in the response. Use
-- this @NextToken@ value in a subsequent request to retrieve additional
-- items.
newListCertificates ::
  ListCertificates
newListCertificates =
  ListCertificates'
    { nextToken = Prelude.Nothing,
      includes = Prelude.Nothing,
      certificateStatuses = Prelude.Nothing,
      maxItems = Prelude.Nothing
    }

-- | Use this parameter only when paginating results and only in a subsequent
-- request after you receive a response with truncated results. Set it to
-- the value of @NextToken@ from the response you just received.
listCertificates_nextToken :: Lens.Lens' ListCertificates (Prelude.Maybe Prelude.Text)
listCertificates_nextToken = Lens.lens (\ListCertificates' {nextToken} -> nextToken) (\s@ListCertificates' {} a -> s {nextToken = a} :: ListCertificates)

-- | Filter the certificate list. For more information, see the Filters
-- structure.
listCertificates_includes :: Lens.Lens' ListCertificates (Prelude.Maybe Filters)
listCertificates_includes = Lens.lens (\ListCertificates' {includes} -> includes) (\s@ListCertificates' {} a -> s {includes = a} :: ListCertificates)

-- | Filter the certificate list by status value.
listCertificates_certificateStatuses :: Lens.Lens' ListCertificates (Prelude.Maybe [CertificateStatus])
listCertificates_certificateStatuses = Lens.lens (\ListCertificates' {certificateStatuses} -> certificateStatuses) (\s@ListCertificates' {} a -> s {certificateStatuses = a} :: ListCertificates) Prelude.. Lens.mapping Lens._Coerce

-- | Use this parameter when paginating results to specify the maximum number
-- of items to return in the response. If additional items exist beyond the
-- number you specify, the @NextToken@ element is sent in the response. Use
-- this @NextToken@ value in a subsequent request to retrieve additional
-- items.
listCertificates_maxItems :: Lens.Lens' ListCertificates (Prelude.Maybe Prelude.Natural)
listCertificates_maxItems = Lens.lens (\ListCertificates' {maxItems} -> maxItems) (\s@ListCertificates' {} a -> s {maxItems = a} :: ListCertificates)

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
            Lens.^? listCertificatesResponse_certificateSummaryList
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listCertificates_nextToken
          Lens..~ rs
          Lens.^? listCertificatesResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListCertificates where
  type
    AWSResponse ListCertificates =
      ListCertificatesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListCertificatesResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> ( x Core..?> "CertificateSummaryList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListCertificates

instance Prelude.NFData ListCertificates

instance Core.ToHeaders ListCertificates where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CertificateManager.ListCertificates" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListCertificates where
  toJSON ListCertificates' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("Includes" Core..=) Prelude.<$> includes,
            ("CertificateStatuses" Core..=)
              Prelude.<$> certificateStatuses,
            ("MaxItems" Core..=) Prelude.<$> maxItems
          ]
      )

instance Core.ToPath ListCertificates where
  toPath = Prelude.const "/"

instance Core.ToQuery ListCertificates where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListCertificatesResponse' smart constructor.
data ListCertificatesResponse = ListCertificatesResponse'
  { -- | When the list is truncated, this value is present and contains the value
    -- to use for the @NextToken@ parameter in a subsequent pagination request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of ACM certificates.
    certificateSummaryList :: Prelude.Maybe [CertificateSummary],
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
-- 'nextToken', 'listCertificatesResponse_nextToken' - When the list is truncated, this value is present and contains the value
-- to use for the @NextToken@ parameter in a subsequent pagination request.
--
-- 'certificateSummaryList', 'listCertificatesResponse_certificateSummaryList' - A list of ACM certificates.
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
      certificateSummaryList = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | When the list is truncated, this value is present and contains the value
-- to use for the @NextToken@ parameter in a subsequent pagination request.
listCertificatesResponse_nextToken :: Lens.Lens' ListCertificatesResponse (Prelude.Maybe Prelude.Text)
listCertificatesResponse_nextToken = Lens.lens (\ListCertificatesResponse' {nextToken} -> nextToken) (\s@ListCertificatesResponse' {} a -> s {nextToken = a} :: ListCertificatesResponse)

-- | A list of ACM certificates.
listCertificatesResponse_certificateSummaryList :: Lens.Lens' ListCertificatesResponse (Prelude.Maybe [CertificateSummary])
listCertificatesResponse_certificateSummaryList = Lens.lens (\ListCertificatesResponse' {certificateSummaryList} -> certificateSummaryList) (\s@ListCertificatesResponse' {} a -> s {certificateSummaryList = a} :: ListCertificatesResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listCertificatesResponse_httpStatus :: Lens.Lens' ListCertificatesResponse Prelude.Int
listCertificatesResponse_httpStatus = Lens.lens (\ListCertificatesResponse' {httpStatus} -> httpStatus) (\s@ListCertificatesResponse' {} a -> s {httpStatus = a} :: ListCertificatesResponse)

instance Prelude.NFData ListCertificatesResponse
