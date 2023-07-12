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
-- Module      : Amazonka.SSM.ListDocuments
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns all Systems Manager (SSM) documents in the current Amazon Web
-- Services account and Amazon Web Services Region. You can limit the
-- results of this request by using a filter.
--
-- This operation returns paginated results.
module Amazonka.SSM.ListDocuments
  ( -- * Creating a Request
    ListDocuments (..),
    newListDocuments,

    -- * Request Lenses
    listDocuments_documentFilterList,
    listDocuments_filters,
    listDocuments_maxResults,
    listDocuments_nextToken,

    -- * Destructuring the Response
    ListDocumentsResponse (..),
    newListDocumentsResponse,

    -- * Response Lenses
    listDocumentsResponse_documentIdentifiers,
    listDocumentsResponse_nextToken,
    listDocumentsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSM.Types

-- | /See:/ 'newListDocuments' smart constructor.
data ListDocuments = ListDocuments'
  { -- | This data type is deprecated. Instead, use @Filters@.
    documentFilterList :: Prelude.Maybe (Prelude.NonEmpty DocumentFilter),
    -- | One or more @DocumentKeyValuesFilter@ objects. Use a filter to return a
    -- more specific list of results. For keys, you can specify one or more
    -- key-value pair tags that have been applied to a document. Other valid
    -- keys include @Owner@, @Name@, @PlatformTypes@, @DocumentType@, and
    -- @TargetType@. For example, to return documents you own use
    -- @Key=Owner,Values=Self@. To specify a custom key-value pair, use the
    -- format @Key=tag:tagName,Values=valueName@.
    --
    -- This API operation only supports filtering documents by using a single
    -- tag key and one or more tag values. For example:
    -- @Key=tag:tagName,Values=valueName1,valueName2@
    filters :: Prelude.Maybe [DocumentKeyValuesFilter],
    -- | The maximum number of items to return for this call. The call also
    -- returns a token that you can specify in a subsequent call to get the
    -- next set of results.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next set of items to return. (You received this token
    -- from a previous call.)
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDocuments' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'documentFilterList', 'listDocuments_documentFilterList' - This data type is deprecated. Instead, use @Filters@.
--
-- 'filters', 'listDocuments_filters' - One or more @DocumentKeyValuesFilter@ objects. Use a filter to return a
-- more specific list of results. For keys, you can specify one or more
-- key-value pair tags that have been applied to a document. Other valid
-- keys include @Owner@, @Name@, @PlatformTypes@, @DocumentType@, and
-- @TargetType@. For example, to return documents you own use
-- @Key=Owner,Values=Self@. To specify a custom key-value pair, use the
-- format @Key=tag:tagName,Values=valueName@.
--
-- This API operation only supports filtering documents by using a single
-- tag key and one or more tag values. For example:
-- @Key=tag:tagName,Values=valueName1,valueName2@
--
-- 'maxResults', 'listDocuments_maxResults' - The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
--
-- 'nextToken', 'listDocuments_nextToken' - The token for the next set of items to return. (You received this token
-- from a previous call.)
newListDocuments ::
  ListDocuments
newListDocuments =
  ListDocuments'
    { documentFilterList =
        Prelude.Nothing,
      filters = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | This data type is deprecated. Instead, use @Filters@.
listDocuments_documentFilterList :: Lens.Lens' ListDocuments (Prelude.Maybe (Prelude.NonEmpty DocumentFilter))
listDocuments_documentFilterList = Lens.lens (\ListDocuments' {documentFilterList} -> documentFilterList) (\s@ListDocuments' {} a -> s {documentFilterList = a} :: ListDocuments) Prelude.. Lens.mapping Lens.coerced

-- | One or more @DocumentKeyValuesFilter@ objects. Use a filter to return a
-- more specific list of results. For keys, you can specify one or more
-- key-value pair tags that have been applied to a document. Other valid
-- keys include @Owner@, @Name@, @PlatformTypes@, @DocumentType@, and
-- @TargetType@. For example, to return documents you own use
-- @Key=Owner,Values=Self@. To specify a custom key-value pair, use the
-- format @Key=tag:tagName,Values=valueName@.
--
-- This API operation only supports filtering documents by using a single
-- tag key and one or more tag values. For example:
-- @Key=tag:tagName,Values=valueName1,valueName2@
listDocuments_filters :: Lens.Lens' ListDocuments (Prelude.Maybe [DocumentKeyValuesFilter])
listDocuments_filters = Lens.lens (\ListDocuments' {filters} -> filters) (\s@ListDocuments' {} a -> s {filters = a} :: ListDocuments) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
listDocuments_maxResults :: Lens.Lens' ListDocuments (Prelude.Maybe Prelude.Natural)
listDocuments_maxResults = Lens.lens (\ListDocuments' {maxResults} -> maxResults) (\s@ListDocuments' {} a -> s {maxResults = a} :: ListDocuments)

-- | The token for the next set of items to return. (You received this token
-- from a previous call.)
listDocuments_nextToken :: Lens.Lens' ListDocuments (Prelude.Maybe Prelude.Text)
listDocuments_nextToken = Lens.lens (\ListDocuments' {nextToken} -> nextToken) (\s@ListDocuments' {} a -> s {nextToken = a} :: ListDocuments)

instance Core.AWSPager ListDocuments where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listDocumentsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listDocumentsResponse_documentIdentifiers
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listDocuments_nextToken
          Lens..~ rs
          Lens.^? listDocumentsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListDocuments where
  type
    AWSResponse ListDocuments =
      ListDocumentsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDocumentsResponse'
            Prelude.<$> ( x
                            Data..?> "DocumentIdentifiers"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListDocuments where
  hashWithSalt _salt ListDocuments' {..} =
    _salt
      `Prelude.hashWithSalt` documentFilterList
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListDocuments where
  rnf ListDocuments' {..} =
    Prelude.rnf documentFilterList
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListDocuments where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("AmazonSSM.ListDocuments" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListDocuments where
  toJSON ListDocuments' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DocumentFilterList" Data..=)
              Prelude.<$> documentFilterList,
            ("Filters" Data..=) Prelude.<$> filters,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListDocuments where
  toPath = Prelude.const "/"

instance Data.ToQuery ListDocuments where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListDocumentsResponse' smart constructor.
data ListDocumentsResponse = ListDocumentsResponse'
  { -- | The names of the SSM documents.
    documentIdentifiers :: Prelude.Maybe [DocumentIdentifier],
    -- | The token to use when requesting the next set of items. If there are no
    -- additional items to return, the string is empty.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDocumentsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'documentIdentifiers', 'listDocumentsResponse_documentIdentifiers' - The names of the SSM documents.
--
-- 'nextToken', 'listDocumentsResponse_nextToken' - The token to use when requesting the next set of items. If there are no
-- additional items to return, the string is empty.
--
-- 'httpStatus', 'listDocumentsResponse_httpStatus' - The response's http status code.
newListDocumentsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListDocumentsResponse
newListDocumentsResponse pHttpStatus_ =
  ListDocumentsResponse'
    { documentIdentifiers =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The names of the SSM documents.
listDocumentsResponse_documentIdentifiers :: Lens.Lens' ListDocumentsResponse (Prelude.Maybe [DocumentIdentifier])
listDocumentsResponse_documentIdentifiers = Lens.lens (\ListDocumentsResponse' {documentIdentifiers} -> documentIdentifiers) (\s@ListDocumentsResponse' {} a -> s {documentIdentifiers = a} :: ListDocumentsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token to use when requesting the next set of items. If there are no
-- additional items to return, the string is empty.
listDocumentsResponse_nextToken :: Lens.Lens' ListDocumentsResponse (Prelude.Maybe Prelude.Text)
listDocumentsResponse_nextToken = Lens.lens (\ListDocumentsResponse' {nextToken} -> nextToken) (\s@ListDocumentsResponse' {} a -> s {nextToken = a} :: ListDocumentsResponse)

-- | The response's http status code.
listDocumentsResponse_httpStatus :: Lens.Lens' ListDocumentsResponse Prelude.Int
listDocumentsResponse_httpStatus = Lens.lens (\ListDocumentsResponse' {httpStatus} -> httpStatus) (\s@ListDocumentsResponse' {} a -> s {httpStatus = a} :: ListDocumentsResponse)

instance Prelude.NFData ListDocumentsResponse where
  rnf ListDocumentsResponse' {..} =
    Prelude.rnf documentIdentifiers
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
