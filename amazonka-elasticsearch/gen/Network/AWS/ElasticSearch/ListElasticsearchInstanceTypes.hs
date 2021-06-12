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
-- Module      : Network.AWS.ElasticSearch.ListElasticsearchInstanceTypes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List all Elasticsearch instance types that are supported for given
-- ElasticsearchVersion
--
-- This operation returns paginated results.
module Network.AWS.ElasticSearch.ListElasticsearchInstanceTypes
  ( -- * Creating a Request
    ListElasticsearchInstanceTypes (..),
    newListElasticsearchInstanceTypes,

    -- * Request Lenses
    listElasticsearchInstanceTypes_nextToken,
    listElasticsearchInstanceTypes_maxResults,
    listElasticsearchInstanceTypes_domainName,
    listElasticsearchInstanceTypes_elasticsearchVersion,

    -- * Destructuring the Response
    ListElasticsearchInstanceTypesResponse (..),
    newListElasticsearchInstanceTypesResponse,

    -- * Response Lenses
    listElasticsearchInstanceTypesResponse_nextToken,
    listElasticsearchInstanceTypesResponse_elasticsearchInstanceTypes,
    listElasticsearchInstanceTypesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ElasticSearch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Container for the parameters to the @ ListElasticsearchInstanceTypes @
-- operation.
--
-- /See:/ 'newListElasticsearchInstanceTypes' smart constructor.
data ListElasticsearchInstanceTypes = ListElasticsearchInstanceTypes'
  { -- | NextToken should be sent in case if earlier API call produced result
    -- containing NextToken. It is used for pagination.
    nextToken :: Core.Maybe Core.Text,
    -- | Set this value to limit the number of results returned. Value provided
    -- must be greater than 30 else it wont be honored.
    maxResults :: Core.Maybe Core.Int,
    -- | DomainName represents the name of the Domain that we are trying to
    -- modify. This should be present only if we are querying for list of
    -- available Elasticsearch instance types when modifying existing domain.
    domainName :: Core.Maybe Core.Text,
    -- | Version of Elasticsearch for which list of supported elasticsearch
    -- instance types are needed.
    elasticsearchVersion :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListElasticsearchInstanceTypes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listElasticsearchInstanceTypes_nextToken' - NextToken should be sent in case if earlier API call produced result
-- containing NextToken. It is used for pagination.
--
-- 'maxResults', 'listElasticsearchInstanceTypes_maxResults' - Set this value to limit the number of results returned. Value provided
-- must be greater than 30 else it wont be honored.
--
-- 'domainName', 'listElasticsearchInstanceTypes_domainName' - DomainName represents the name of the Domain that we are trying to
-- modify. This should be present only if we are querying for list of
-- available Elasticsearch instance types when modifying existing domain.
--
-- 'elasticsearchVersion', 'listElasticsearchInstanceTypes_elasticsearchVersion' - Version of Elasticsearch for which list of supported elasticsearch
-- instance types are needed.
newListElasticsearchInstanceTypes ::
  -- | 'elasticsearchVersion'
  Core.Text ->
  ListElasticsearchInstanceTypes
newListElasticsearchInstanceTypes
  pElasticsearchVersion_ =
    ListElasticsearchInstanceTypes'
      { nextToken =
          Core.Nothing,
        maxResults = Core.Nothing,
        domainName = Core.Nothing,
        elasticsearchVersion =
          pElasticsearchVersion_
      }

-- | NextToken should be sent in case if earlier API call produced result
-- containing NextToken. It is used for pagination.
listElasticsearchInstanceTypes_nextToken :: Lens.Lens' ListElasticsearchInstanceTypes (Core.Maybe Core.Text)
listElasticsearchInstanceTypes_nextToken = Lens.lens (\ListElasticsearchInstanceTypes' {nextToken} -> nextToken) (\s@ListElasticsearchInstanceTypes' {} a -> s {nextToken = a} :: ListElasticsearchInstanceTypes)

-- | Set this value to limit the number of results returned. Value provided
-- must be greater than 30 else it wont be honored.
listElasticsearchInstanceTypes_maxResults :: Lens.Lens' ListElasticsearchInstanceTypes (Core.Maybe Core.Int)
listElasticsearchInstanceTypes_maxResults = Lens.lens (\ListElasticsearchInstanceTypes' {maxResults} -> maxResults) (\s@ListElasticsearchInstanceTypes' {} a -> s {maxResults = a} :: ListElasticsearchInstanceTypes)

-- | DomainName represents the name of the Domain that we are trying to
-- modify. This should be present only if we are querying for list of
-- available Elasticsearch instance types when modifying existing domain.
listElasticsearchInstanceTypes_domainName :: Lens.Lens' ListElasticsearchInstanceTypes (Core.Maybe Core.Text)
listElasticsearchInstanceTypes_domainName = Lens.lens (\ListElasticsearchInstanceTypes' {domainName} -> domainName) (\s@ListElasticsearchInstanceTypes' {} a -> s {domainName = a} :: ListElasticsearchInstanceTypes)

-- | Version of Elasticsearch for which list of supported elasticsearch
-- instance types are needed.
listElasticsearchInstanceTypes_elasticsearchVersion :: Lens.Lens' ListElasticsearchInstanceTypes Core.Text
listElasticsearchInstanceTypes_elasticsearchVersion = Lens.lens (\ListElasticsearchInstanceTypes' {elasticsearchVersion} -> elasticsearchVersion) (\s@ListElasticsearchInstanceTypes' {} a -> s {elasticsearchVersion = a} :: ListElasticsearchInstanceTypes)

instance Core.AWSPager ListElasticsearchInstanceTypes where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listElasticsearchInstanceTypesResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listElasticsearchInstanceTypesResponse_elasticsearchInstanceTypes
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listElasticsearchInstanceTypes_nextToken
          Lens..~ rs
          Lens.^? listElasticsearchInstanceTypesResponse_nextToken
            Core.. Lens._Just

instance
  Core.AWSRequest
    ListElasticsearchInstanceTypes
  where
  type
    AWSResponse ListElasticsearchInstanceTypes =
      ListElasticsearchInstanceTypesResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListElasticsearchInstanceTypesResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> ( x Core..?> "ElasticsearchInstanceTypes"
                         Core..!@ Core.mempty
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListElasticsearchInstanceTypes

instance Core.NFData ListElasticsearchInstanceTypes

instance
  Core.ToHeaders
    ListElasticsearchInstanceTypes
  where
  toHeaders = Core.const Core.mempty

instance Core.ToPath ListElasticsearchInstanceTypes where
  toPath ListElasticsearchInstanceTypes' {..} =
    Core.mconcat
      [ "/2015-01-01/es/instanceTypes/",
        Core.toBS elasticsearchVersion
      ]

instance Core.ToQuery ListElasticsearchInstanceTypes where
  toQuery ListElasticsearchInstanceTypes' {..} =
    Core.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults,
        "domainName" Core.=: domainName
      ]

-- | Container for the parameters returned by
-- @ ListElasticsearchInstanceTypes @ operation.
--
-- /See:/ 'newListElasticsearchInstanceTypesResponse' smart constructor.
data ListElasticsearchInstanceTypesResponse = ListElasticsearchInstanceTypesResponse'
  { -- | In case if there are more results available NextToken would be present,
    -- make further request to the same API with received NextToken to paginate
    -- remaining results.
    nextToken :: Core.Maybe Core.Text,
    -- | List of instance types supported by Amazon Elasticsearch service for
    -- given @ ElasticsearchVersion @
    elasticsearchInstanceTypes :: Core.Maybe [ESPartitionInstanceType],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListElasticsearchInstanceTypesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listElasticsearchInstanceTypesResponse_nextToken' - In case if there are more results available NextToken would be present,
-- make further request to the same API with received NextToken to paginate
-- remaining results.
--
-- 'elasticsearchInstanceTypes', 'listElasticsearchInstanceTypesResponse_elasticsearchInstanceTypes' - List of instance types supported by Amazon Elasticsearch service for
-- given @ ElasticsearchVersion @
--
-- 'httpStatus', 'listElasticsearchInstanceTypesResponse_httpStatus' - The response's http status code.
newListElasticsearchInstanceTypesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListElasticsearchInstanceTypesResponse
newListElasticsearchInstanceTypesResponse
  pHttpStatus_ =
    ListElasticsearchInstanceTypesResponse'
      { nextToken =
          Core.Nothing,
        elasticsearchInstanceTypes =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | In case if there are more results available NextToken would be present,
-- make further request to the same API with received NextToken to paginate
-- remaining results.
listElasticsearchInstanceTypesResponse_nextToken :: Lens.Lens' ListElasticsearchInstanceTypesResponse (Core.Maybe Core.Text)
listElasticsearchInstanceTypesResponse_nextToken = Lens.lens (\ListElasticsearchInstanceTypesResponse' {nextToken} -> nextToken) (\s@ListElasticsearchInstanceTypesResponse' {} a -> s {nextToken = a} :: ListElasticsearchInstanceTypesResponse)

-- | List of instance types supported by Amazon Elasticsearch service for
-- given @ ElasticsearchVersion @
listElasticsearchInstanceTypesResponse_elasticsearchInstanceTypes :: Lens.Lens' ListElasticsearchInstanceTypesResponse (Core.Maybe [ESPartitionInstanceType])
listElasticsearchInstanceTypesResponse_elasticsearchInstanceTypes = Lens.lens (\ListElasticsearchInstanceTypesResponse' {elasticsearchInstanceTypes} -> elasticsearchInstanceTypes) (\s@ListElasticsearchInstanceTypesResponse' {} a -> s {elasticsearchInstanceTypes = a} :: ListElasticsearchInstanceTypesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listElasticsearchInstanceTypesResponse_httpStatus :: Lens.Lens' ListElasticsearchInstanceTypesResponse Core.Int
listElasticsearchInstanceTypesResponse_httpStatus = Lens.lens (\ListElasticsearchInstanceTypesResponse' {httpStatus} -> httpStatus) (\s@ListElasticsearchInstanceTypesResponse' {} a -> s {httpStatus = a} :: ListElasticsearchInstanceTypesResponse)

instance
  Core.NFData
    ListElasticsearchInstanceTypesResponse
