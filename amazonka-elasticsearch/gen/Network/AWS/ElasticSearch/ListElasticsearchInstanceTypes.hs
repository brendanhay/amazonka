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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Container for the parameters to the @ ListElasticsearchInstanceTypes @
-- operation.
--
-- /See:/ 'newListElasticsearchInstanceTypes' smart constructor.
data ListElasticsearchInstanceTypes = ListElasticsearchInstanceTypes'
  { -- | NextToken should be sent in case if earlier API call produced result
    -- containing NextToken. It is used for pagination.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Set this value to limit the number of results returned. Value provided
    -- must be greater than 30 else it wont be honored.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | DomainName represents the name of the Domain that we are trying to
    -- modify. This should be present only if we are querying for list of
    -- available Elasticsearch instance types when modifying existing domain.
    domainName :: Prelude.Maybe Prelude.Text,
    -- | Version of Elasticsearch for which list of supported elasticsearch
    -- instance types are needed.
    elasticsearchVersion :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  ListElasticsearchInstanceTypes
newListElasticsearchInstanceTypes
  pElasticsearchVersion_ =
    ListElasticsearchInstanceTypes'
      { nextToken =
          Prelude.Nothing,
        maxResults = Prelude.Nothing,
        domainName = Prelude.Nothing,
        elasticsearchVersion =
          pElasticsearchVersion_
      }

-- | NextToken should be sent in case if earlier API call produced result
-- containing NextToken. It is used for pagination.
listElasticsearchInstanceTypes_nextToken :: Lens.Lens' ListElasticsearchInstanceTypes (Prelude.Maybe Prelude.Text)
listElasticsearchInstanceTypes_nextToken = Lens.lens (\ListElasticsearchInstanceTypes' {nextToken} -> nextToken) (\s@ListElasticsearchInstanceTypes' {} a -> s {nextToken = a} :: ListElasticsearchInstanceTypes)

-- | Set this value to limit the number of results returned. Value provided
-- must be greater than 30 else it wont be honored.
listElasticsearchInstanceTypes_maxResults :: Lens.Lens' ListElasticsearchInstanceTypes (Prelude.Maybe Prelude.Int)
listElasticsearchInstanceTypes_maxResults = Lens.lens (\ListElasticsearchInstanceTypes' {maxResults} -> maxResults) (\s@ListElasticsearchInstanceTypes' {} a -> s {maxResults = a} :: ListElasticsearchInstanceTypes)

-- | DomainName represents the name of the Domain that we are trying to
-- modify. This should be present only if we are querying for list of
-- available Elasticsearch instance types when modifying existing domain.
listElasticsearchInstanceTypes_domainName :: Lens.Lens' ListElasticsearchInstanceTypes (Prelude.Maybe Prelude.Text)
listElasticsearchInstanceTypes_domainName = Lens.lens (\ListElasticsearchInstanceTypes' {domainName} -> domainName) (\s@ListElasticsearchInstanceTypes' {} a -> s {domainName = a} :: ListElasticsearchInstanceTypes)

-- | Version of Elasticsearch for which list of supported elasticsearch
-- instance types are needed.
listElasticsearchInstanceTypes_elasticsearchVersion :: Lens.Lens' ListElasticsearchInstanceTypes Prelude.Text
listElasticsearchInstanceTypes_elasticsearchVersion = Lens.lens (\ListElasticsearchInstanceTypes' {elasticsearchVersion} -> elasticsearchVersion) (\s@ListElasticsearchInstanceTypes' {} a -> s {elasticsearchVersion = a} :: ListElasticsearchInstanceTypes)

instance Core.AWSPager ListElasticsearchInstanceTypes where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listElasticsearchInstanceTypesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listElasticsearchInstanceTypesResponse_elasticsearchInstanceTypes
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listElasticsearchInstanceTypes_nextToken
          Lens..~ rs
          Lens.^? listElasticsearchInstanceTypesResponse_nextToken
            Prelude.. Lens._Just

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
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> ( x Core..?> "ElasticsearchInstanceTypes"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListElasticsearchInstanceTypes

instance
  Prelude.NFData
    ListElasticsearchInstanceTypes

instance
  Core.ToHeaders
    ListElasticsearchInstanceTypes
  where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ListElasticsearchInstanceTypes where
  toPath ListElasticsearchInstanceTypes' {..} =
    Prelude.mconcat
      [ "/2015-01-01/es/instanceTypes/",
        Core.toBS elasticsearchVersion
      ]

instance Core.ToQuery ListElasticsearchInstanceTypes where
  toQuery ListElasticsearchInstanceTypes' {..} =
    Prelude.mconcat
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
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | List of instance types supported by Amazon Elasticsearch service for
    -- given @ ElasticsearchVersion @
    elasticsearchInstanceTypes :: Prelude.Maybe [ESPartitionInstanceType],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  ListElasticsearchInstanceTypesResponse
newListElasticsearchInstanceTypesResponse
  pHttpStatus_ =
    ListElasticsearchInstanceTypesResponse'
      { nextToken =
          Prelude.Nothing,
        elasticsearchInstanceTypes =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | In case if there are more results available NextToken would be present,
-- make further request to the same API with received NextToken to paginate
-- remaining results.
listElasticsearchInstanceTypesResponse_nextToken :: Lens.Lens' ListElasticsearchInstanceTypesResponse (Prelude.Maybe Prelude.Text)
listElasticsearchInstanceTypesResponse_nextToken = Lens.lens (\ListElasticsearchInstanceTypesResponse' {nextToken} -> nextToken) (\s@ListElasticsearchInstanceTypesResponse' {} a -> s {nextToken = a} :: ListElasticsearchInstanceTypesResponse)

-- | List of instance types supported by Amazon Elasticsearch service for
-- given @ ElasticsearchVersion @
listElasticsearchInstanceTypesResponse_elasticsearchInstanceTypes :: Lens.Lens' ListElasticsearchInstanceTypesResponse (Prelude.Maybe [ESPartitionInstanceType])
listElasticsearchInstanceTypesResponse_elasticsearchInstanceTypes = Lens.lens (\ListElasticsearchInstanceTypesResponse' {elasticsearchInstanceTypes} -> elasticsearchInstanceTypes) (\s@ListElasticsearchInstanceTypesResponse' {} a -> s {elasticsearchInstanceTypes = a} :: ListElasticsearchInstanceTypesResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listElasticsearchInstanceTypesResponse_httpStatus :: Lens.Lens' ListElasticsearchInstanceTypesResponse Prelude.Int
listElasticsearchInstanceTypesResponse_httpStatus = Lens.lens (\ListElasticsearchInstanceTypesResponse' {httpStatus} -> httpStatus) (\s@ListElasticsearchInstanceTypesResponse' {} a -> s {httpStatus = a} :: ListElasticsearchInstanceTypesResponse)

instance
  Prelude.NFData
    ListElasticsearchInstanceTypesResponse
