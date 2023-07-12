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
-- Module      : Amazonka.SSM.ListOpsMetadata
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Amazon Web Services Systems Manager calls this API operation when
-- displaying all Application Manager OpsMetadata objects or blobs.
--
-- This operation returns paginated results.
module Amazonka.SSM.ListOpsMetadata
  ( -- * Creating a Request
    ListOpsMetadata (..),
    newListOpsMetadata,

    -- * Request Lenses
    listOpsMetadata_filters,
    listOpsMetadata_maxResults,
    listOpsMetadata_nextToken,

    -- * Destructuring the Response
    ListOpsMetadataResponse (..),
    newListOpsMetadataResponse,

    -- * Response Lenses
    listOpsMetadataResponse_nextToken,
    listOpsMetadataResponse_opsMetadataList,
    listOpsMetadataResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSM.Types

-- | /See:/ 'newListOpsMetadata' smart constructor.
data ListOpsMetadata = ListOpsMetadata'
  { -- | One or more filters to limit the number of OpsMetadata objects returned
    -- by the call.
    filters :: Prelude.Maybe [OpsMetadataFilter],
    -- | The maximum number of items to return for this call. The call also
    -- returns a token that you can specify in a subsequent call to get the
    -- next set of results.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A token to start the list. Use this token to get the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListOpsMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filters', 'listOpsMetadata_filters' - One or more filters to limit the number of OpsMetadata objects returned
-- by the call.
--
-- 'maxResults', 'listOpsMetadata_maxResults' - The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
--
-- 'nextToken', 'listOpsMetadata_nextToken' - A token to start the list. Use this token to get the next set of
-- results.
newListOpsMetadata ::
  ListOpsMetadata
newListOpsMetadata =
  ListOpsMetadata'
    { filters = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | One or more filters to limit the number of OpsMetadata objects returned
-- by the call.
listOpsMetadata_filters :: Lens.Lens' ListOpsMetadata (Prelude.Maybe [OpsMetadataFilter])
listOpsMetadata_filters = Lens.lens (\ListOpsMetadata' {filters} -> filters) (\s@ListOpsMetadata' {} a -> s {filters = a} :: ListOpsMetadata) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
listOpsMetadata_maxResults :: Lens.Lens' ListOpsMetadata (Prelude.Maybe Prelude.Natural)
listOpsMetadata_maxResults = Lens.lens (\ListOpsMetadata' {maxResults} -> maxResults) (\s@ListOpsMetadata' {} a -> s {maxResults = a} :: ListOpsMetadata)

-- | A token to start the list. Use this token to get the next set of
-- results.
listOpsMetadata_nextToken :: Lens.Lens' ListOpsMetadata (Prelude.Maybe Prelude.Text)
listOpsMetadata_nextToken = Lens.lens (\ListOpsMetadata' {nextToken} -> nextToken) (\s@ListOpsMetadata' {} a -> s {nextToken = a} :: ListOpsMetadata)

instance Core.AWSPager ListOpsMetadata where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listOpsMetadataResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listOpsMetadataResponse_opsMetadataList
            Prelude.. Lens._Just
            Prelude.. Lens.to Prelude.toList
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listOpsMetadata_nextToken
          Lens..~ rs
          Lens.^? listOpsMetadataResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListOpsMetadata where
  type
    AWSResponse ListOpsMetadata =
      ListOpsMetadataResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListOpsMetadataResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "OpsMetadataList")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListOpsMetadata where
  hashWithSalt _salt ListOpsMetadata' {..} =
    _salt
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListOpsMetadata where
  rnf ListOpsMetadata' {..} =
    Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListOpsMetadata where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("AmazonSSM.ListOpsMetadata" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListOpsMetadata where
  toJSON ListOpsMetadata' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Filters" Data..=) Prelude.<$> filters,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListOpsMetadata where
  toPath = Prelude.const "/"

instance Data.ToQuery ListOpsMetadata where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListOpsMetadataResponse' smart constructor.
data ListOpsMetadataResponse = ListOpsMetadataResponse'
  { -- | The token for the next set of items to return. Use this token to get the
    -- next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Returns a list of OpsMetadata objects.
    opsMetadataList :: Prelude.Maybe (Prelude.NonEmpty OpsMetadata),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListOpsMetadataResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listOpsMetadataResponse_nextToken' - The token for the next set of items to return. Use this token to get the
-- next set of results.
--
-- 'opsMetadataList', 'listOpsMetadataResponse_opsMetadataList' - Returns a list of OpsMetadata objects.
--
-- 'httpStatus', 'listOpsMetadataResponse_httpStatus' - The response's http status code.
newListOpsMetadataResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListOpsMetadataResponse
newListOpsMetadataResponse pHttpStatus_ =
  ListOpsMetadataResponse'
    { nextToken =
        Prelude.Nothing,
      opsMetadataList = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token for the next set of items to return. Use this token to get the
-- next set of results.
listOpsMetadataResponse_nextToken :: Lens.Lens' ListOpsMetadataResponse (Prelude.Maybe Prelude.Text)
listOpsMetadataResponse_nextToken = Lens.lens (\ListOpsMetadataResponse' {nextToken} -> nextToken) (\s@ListOpsMetadataResponse' {} a -> s {nextToken = a} :: ListOpsMetadataResponse)

-- | Returns a list of OpsMetadata objects.
listOpsMetadataResponse_opsMetadataList :: Lens.Lens' ListOpsMetadataResponse (Prelude.Maybe (Prelude.NonEmpty OpsMetadata))
listOpsMetadataResponse_opsMetadataList = Lens.lens (\ListOpsMetadataResponse' {opsMetadataList} -> opsMetadataList) (\s@ListOpsMetadataResponse' {} a -> s {opsMetadataList = a} :: ListOpsMetadataResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listOpsMetadataResponse_httpStatus :: Lens.Lens' ListOpsMetadataResponse Prelude.Int
listOpsMetadataResponse_httpStatus = Lens.lens (\ListOpsMetadataResponse' {httpStatus} -> httpStatus) (\s@ListOpsMetadataResponse' {} a -> s {httpStatus = a} :: ListOpsMetadataResponse)

instance Prelude.NFData ListOpsMetadataResponse where
  rnf ListOpsMetadataResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf opsMetadataList
      `Prelude.seq` Prelude.rnf httpStatus
