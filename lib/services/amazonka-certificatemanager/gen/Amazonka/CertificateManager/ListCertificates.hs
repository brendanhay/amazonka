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
-- Module      : Amazonka.CertificateManager.ListCertificates
-- Copyright   : (c) 2013-2022 Brendan Hay
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
module Amazonka.CertificateManager.ListCertificates
  ( -- * Creating a Request
    ListCertificates (..),
    newListCertificates,

    -- * Request Lenses
    listCertificates_sortOrder,
    listCertificates_nextToken,
    listCertificates_maxItems,
    listCertificates_sortBy,
    listCertificates_includes,
    listCertificates_certificateStatuses,

    -- * Destructuring the Response
    ListCertificatesResponse (..),
    newListCertificatesResponse,

    -- * Response Lenses
    listCertificatesResponse_nextToken,
    listCertificatesResponse_certificateSummaryList,
    listCertificatesResponse_httpStatus,
  )
where

import Amazonka.CertificateManager.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListCertificates' smart constructor.
data ListCertificates = ListCertificates'
  { -- | Specifies the order of sorted results. If you specify @SortOrder@, you
    -- must also specify @SortBy@.
    sortOrder :: Prelude.Maybe SortOrder,
    -- | Use this parameter only when paginating results and only in a subsequent
    -- request after you receive a response with truncated results. Set it to
    -- the value of @NextToken@ from the response you just received.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Use this parameter when paginating results to specify the maximum number
    -- of items to return in the response. If additional items exist beyond the
    -- number you specify, the @NextToken@ element is sent in the response. Use
    -- this @NextToken@ value in a subsequent request to retrieve additional
    -- items.
    maxItems :: Prelude.Maybe Prelude.Natural,
    -- | Specifies the field to sort results by. If you specify @SortBy@, you
    -- must also specify @SortOrder@.
    sortBy :: Prelude.Maybe SortBy,
    -- | Filter the certificate list. For more information, see the Filters
    -- structure.
    includes :: Prelude.Maybe Filters,
    -- | Filter the certificate list by status value.
    certificateStatuses :: Prelude.Maybe [CertificateStatus]
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
-- 'sortOrder', 'listCertificates_sortOrder' - Specifies the order of sorted results. If you specify @SortOrder@, you
-- must also specify @SortBy@.
--
-- 'nextToken', 'listCertificates_nextToken' - Use this parameter only when paginating results and only in a subsequent
-- request after you receive a response with truncated results. Set it to
-- the value of @NextToken@ from the response you just received.
--
-- 'maxItems', 'listCertificates_maxItems' - Use this parameter when paginating results to specify the maximum number
-- of items to return in the response. If additional items exist beyond the
-- number you specify, the @NextToken@ element is sent in the response. Use
-- this @NextToken@ value in a subsequent request to retrieve additional
-- items.
--
-- 'sortBy', 'listCertificates_sortBy' - Specifies the field to sort results by. If you specify @SortBy@, you
-- must also specify @SortOrder@.
--
-- 'includes', 'listCertificates_includes' - Filter the certificate list. For more information, see the Filters
-- structure.
--
-- 'certificateStatuses', 'listCertificates_certificateStatuses' - Filter the certificate list by status value.
newListCertificates ::
  ListCertificates
newListCertificates =
  ListCertificates'
    { sortOrder = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      maxItems = Prelude.Nothing,
      sortBy = Prelude.Nothing,
      includes = Prelude.Nothing,
      certificateStatuses = Prelude.Nothing
    }

-- | Specifies the order of sorted results. If you specify @SortOrder@, you
-- must also specify @SortBy@.
listCertificates_sortOrder :: Lens.Lens' ListCertificates (Prelude.Maybe SortOrder)
listCertificates_sortOrder = Lens.lens (\ListCertificates' {sortOrder} -> sortOrder) (\s@ListCertificates' {} a -> s {sortOrder = a} :: ListCertificates)

-- | Use this parameter only when paginating results and only in a subsequent
-- request after you receive a response with truncated results. Set it to
-- the value of @NextToken@ from the response you just received.
listCertificates_nextToken :: Lens.Lens' ListCertificates (Prelude.Maybe Prelude.Text)
listCertificates_nextToken = Lens.lens (\ListCertificates' {nextToken} -> nextToken) (\s@ListCertificates' {} a -> s {nextToken = a} :: ListCertificates)

-- | Use this parameter when paginating results to specify the maximum number
-- of items to return in the response. If additional items exist beyond the
-- number you specify, the @NextToken@ element is sent in the response. Use
-- this @NextToken@ value in a subsequent request to retrieve additional
-- items.
listCertificates_maxItems :: Lens.Lens' ListCertificates (Prelude.Maybe Prelude.Natural)
listCertificates_maxItems = Lens.lens (\ListCertificates' {maxItems} -> maxItems) (\s@ListCertificates' {} a -> s {maxItems = a} :: ListCertificates)

-- | Specifies the field to sort results by. If you specify @SortBy@, you
-- must also specify @SortOrder@.
listCertificates_sortBy :: Lens.Lens' ListCertificates (Prelude.Maybe SortBy)
listCertificates_sortBy = Lens.lens (\ListCertificates' {sortBy} -> sortBy) (\s@ListCertificates' {} a -> s {sortBy = a} :: ListCertificates)

-- | Filter the certificate list. For more information, see the Filters
-- structure.
listCertificates_includes :: Lens.Lens' ListCertificates (Prelude.Maybe Filters)
listCertificates_includes = Lens.lens (\ListCertificates' {includes} -> includes) (\s@ListCertificates' {} a -> s {includes = a} :: ListCertificates)

-- | Filter the certificate list by status value.
listCertificates_certificateStatuses :: Lens.Lens' ListCertificates (Prelude.Maybe [CertificateStatus])
listCertificates_certificateStatuses = Lens.lens (\ListCertificates' {certificateStatuses} -> certificateStatuses) (\s@ListCertificates' {} a -> s {certificateStatuses = a} :: ListCertificates) Prelude.. Lens.mapping Lens.coerced

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
  request overrides =
    Request.postJSON (overrides defaultService)
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

instance Prelude.Hashable ListCertificates where
  hashWithSalt _salt ListCertificates' {..} =
    _salt `Prelude.hashWithSalt` sortOrder
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxItems
      `Prelude.hashWithSalt` sortBy
      `Prelude.hashWithSalt` includes
      `Prelude.hashWithSalt` certificateStatuses

instance Prelude.NFData ListCertificates where
  rnf ListCertificates' {..} =
    Prelude.rnf sortOrder
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxItems
      `Prelude.seq` Prelude.rnf sortBy
      `Prelude.seq` Prelude.rnf includes
      `Prelude.seq` Prelude.rnf certificateStatuses

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
          [ ("SortOrder" Core..=) Prelude.<$> sortOrder,
            ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxItems" Core..=) Prelude.<$> maxItems,
            ("SortBy" Core..=) Prelude.<$> sortBy,
            ("Includes" Core..=) Prelude.<$> includes,
            ("CertificateStatuses" Core..=)
              Prelude.<$> certificateStatuses
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
listCertificatesResponse_certificateSummaryList = Lens.lens (\ListCertificatesResponse' {certificateSummaryList} -> certificateSummaryList) (\s@ListCertificatesResponse' {} a -> s {certificateSummaryList = a} :: ListCertificatesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listCertificatesResponse_httpStatus :: Lens.Lens' ListCertificatesResponse Prelude.Int
listCertificatesResponse_httpStatus = Lens.lens (\ListCertificatesResponse' {httpStatus} -> httpStatus) (\s@ListCertificatesResponse' {} a -> s {httpStatus = a} :: ListCertificatesResponse)

instance Prelude.NFData ListCertificatesResponse where
  rnf ListCertificatesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf certificateSummaryList
      `Prelude.seq` Prelude.rnf httpStatus
