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
    listCertificates_certificateStatuses,
    listCertificates_includes,
    listCertificates_maxItems,
    listCertificates_nextToken,
    listCertificates_sortBy,
    listCertificates_sortOrder,

    -- * Destructuring the Response
    ListCertificatesResponse (..),
    newListCertificatesResponse,

    -- * Response Lenses
    listCertificatesResponse_certificateSummaryList,
    listCertificatesResponse_nextToken,
    listCertificatesResponse_httpStatus,
  )
where

import Amazonka.CertificateManager.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListCertificates' smart constructor.
data ListCertificates = ListCertificates'
  { -- | Filter the certificate list by status value.
    certificateStatuses :: Prelude.Maybe [CertificateStatus],
    -- | Filter the certificate list. For more information, see the Filters
    -- structure.
    includes :: Prelude.Maybe Filters,
    -- | Use this parameter when paginating results to specify the maximum number
    -- of items to return in the response. If additional items exist beyond the
    -- number you specify, the @NextToken@ element is sent in the response. Use
    -- this @NextToken@ value in a subsequent request to retrieve additional
    -- items.
    maxItems :: Prelude.Maybe Prelude.Natural,
    -- | Use this parameter only when paginating results and only in a subsequent
    -- request after you receive a response with truncated results. Set it to
    -- the value of @NextToken@ from the response you just received.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Specifies the field to sort results by. If you specify @SortBy@, you
    -- must also specify @SortOrder@.
    sortBy :: Prelude.Maybe SortBy,
    -- | Specifies the order of sorted results. If you specify @SortOrder@, you
    -- must also specify @SortBy@.
    sortOrder :: Prelude.Maybe SortOrder
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
-- 'certificateStatuses', 'listCertificates_certificateStatuses' - Filter the certificate list by status value.
--
-- 'includes', 'listCertificates_includes' - Filter the certificate list. For more information, see the Filters
-- structure.
--
-- 'maxItems', 'listCertificates_maxItems' - Use this parameter when paginating results to specify the maximum number
-- of items to return in the response. If additional items exist beyond the
-- number you specify, the @NextToken@ element is sent in the response. Use
-- this @NextToken@ value in a subsequent request to retrieve additional
-- items.
--
-- 'nextToken', 'listCertificates_nextToken' - Use this parameter only when paginating results and only in a subsequent
-- request after you receive a response with truncated results. Set it to
-- the value of @NextToken@ from the response you just received.
--
-- 'sortBy', 'listCertificates_sortBy' - Specifies the field to sort results by. If you specify @SortBy@, you
-- must also specify @SortOrder@.
--
-- 'sortOrder', 'listCertificates_sortOrder' - Specifies the order of sorted results. If you specify @SortOrder@, you
-- must also specify @SortBy@.
newListCertificates ::
  ListCertificates
newListCertificates =
  ListCertificates'
    { certificateStatuses =
        Prelude.Nothing,
      includes = Prelude.Nothing,
      maxItems = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      sortBy = Prelude.Nothing,
      sortOrder = Prelude.Nothing
    }

-- | Filter the certificate list by status value.
listCertificates_certificateStatuses :: Lens.Lens' ListCertificates (Prelude.Maybe [CertificateStatus])
listCertificates_certificateStatuses = Lens.lens (\ListCertificates' {certificateStatuses} -> certificateStatuses) (\s@ListCertificates' {} a -> s {certificateStatuses = a} :: ListCertificates) Prelude.. Lens.mapping Lens.coerced

-- | Filter the certificate list. For more information, see the Filters
-- structure.
listCertificates_includes :: Lens.Lens' ListCertificates (Prelude.Maybe Filters)
listCertificates_includes = Lens.lens (\ListCertificates' {includes} -> includes) (\s@ListCertificates' {} a -> s {includes = a} :: ListCertificates)

-- | Use this parameter when paginating results to specify the maximum number
-- of items to return in the response. If additional items exist beyond the
-- number you specify, the @NextToken@ element is sent in the response. Use
-- this @NextToken@ value in a subsequent request to retrieve additional
-- items.
listCertificates_maxItems :: Lens.Lens' ListCertificates (Prelude.Maybe Prelude.Natural)
listCertificates_maxItems = Lens.lens (\ListCertificates' {maxItems} -> maxItems) (\s@ListCertificates' {} a -> s {maxItems = a} :: ListCertificates)

-- | Use this parameter only when paginating results and only in a subsequent
-- request after you receive a response with truncated results. Set it to
-- the value of @NextToken@ from the response you just received.
listCertificates_nextToken :: Lens.Lens' ListCertificates (Prelude.Maybe Prelude.Text)
listCertificates_nextToken = Lens.lens (\ListCertificates' {nextToken} -> nextToken) (\s@ListCertificates' {} a -> s {nextToken = a} :: ListCertificates)

-- | Specifies the field to sort results by. If you specify @SortBy@, you
-- must also specify @SortOrder@.
listCertificates_sortBy :: Lens.Lens' ListCertificates (Prelude.Maybe SortBy)
listCertificates_sortBy = Lens.lens (\ListCertificates' {sortBy} -> sortBy) (\s@ListCertificates' {} a -> s {sortBy = a} :: ListCertificates)

-- | Specifies the order of sorted results. If you specify @SortOrder@, you
-- must also specify @SortBy@.
listCertificates_sortOrder :: Lens.Lens' ListCertificates (Prelude.Maybe SortOrder)
listCertificates_sortOrder = Lens.lens (\ListCertificates' {sortOrder} -> sortOrder) (\s@ListCertificates' {} a -> s {sortOrder = a} :: ListCertificates)

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
            Prelude.<$> ( x Data..?> "CertificateSummaryList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListCertificates where
  hashWithSalt _salt ListCertificates' {..} =
    _salt `Prelude.hashWithSalt` certificateStatuses
      `Prelude.hashWithSalt` includes
      `Prelude.hashWithSalt` maxItems
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` sortBy
      `Prelude.hashWithSalt` sortOrder

instance Prelude.NFData ListCertificates where
  rnf ListCertificates' {..} =
    Prelude.rnf certificateStatuses
      `Prelude.seq` Prelude.rnf includes
      `Prelude.seq` Prelude.rnf maxItems
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf sortBy
      `Prelude.seq` Prelude.rnf sortOrder

instance Data.ToHeaders ListCertificates where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "CertificateManager.ListCertificates" ::
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
          [ ("CertificateStatuses" Data..=)
              Prelude.<$> certificateStatuses,
            ("Includes" Data..=) Prelude.<$> includes,
            ("MaxItems" Data..=) Prelude.<$> maxItems,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("SortBy" Data..=) Prelude.<$> sortBy,
            ("SortOrder" Data..=) Prelude.<$> sortOrder
          ]
      )

instance Data.ToPath ListCertificates where
  toPath = Prelude.const "/"

instance Data.ToQuery ListCertificates where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListCertificatesResponse' smart constructor.
data ListCertificatesResponse = ListCertificatesResponse'
  { -- | A list of ACM certificates.
    certificateSummaryList :: Prelude.Maybe [CertificateSummary],
    -- | When the list is truncated, this value is present and contains the value
    -- to use for the @NextToken@ parameter in a subsequent pagination request.
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
-- 'certificateSummaryList', 'listCertificatesResponse_certificateSummaryList' - A list of ACM certificates.
--
-- 'nextToken', 'listCertificatesResponse_nextToken' - When the list is truncated, this value is present and contains the value
-- to use for the @NextToken@ parameter in a subsequent pagination request.
--
-- 'httpStatus', 'listCertificatesResponse_httpStatus' - The response's http status code.
newListCertificatesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListCertificatesResponse
newListCertificatesResponse pHttpStatus_ =
  ListCertificatesResponse'
    { certificateSummaryList =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of ACM certificates.
listCertificatesResponse_certificateSummaryList :: Lens.Lens' ListCertificatesResponse (Prelude.Maybe [CertificateSummary])
listCertificatesResponse_certificateSummaryList = Lens.lens (\ListCertificatesResponse' {certificateSummaryList} -> certificateSummaryList) (\s@ListCertificatesResponse' {} a -> s {certificateSummaryList = a} :: ListCertificatesResponse) Prelude.. Lens.mapping Lens.coerced

-- | When the list is truncated, this value is present and contains the value
-- to use for the @NextToken@ parameter in a subsequent pagination request.
listCertificatesResponse_nextToken :: Lens.Lens' ListCertificatesResponse (Prelude.Maybe Prelude.Text)
listCertificatesResponse_nextToken = Lens.lens (\ListCertificatesResponse' {nextToken} -> nextToken) (\s@ListCertificatesResponse' {} a -> s {nextToken = a} :: ListCertificatesResponse)

-- | The response's http status code.
listCertificatesResponse_httpStatus :: Lens.Lens' ListCertificatesResponse Prelude.Int
listCertificatesResponse_httpStatus = Lens.lens (\ListCertificatesResponse' {httpStatus} -> httpStatus) (\s@ListCertificatesResponse' {} a -> s {httpStatus = a} :: ListCertificatesResponse)

instance Prelude.NFData ListCertificatesResponse where
  rnf ListCertificatesResponse' {..} =
    Prelude.rnf certificateSummaryList
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
