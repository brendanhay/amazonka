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
-- Module      : Amazonka.ECS.ListAttributes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the attributes for Amazon ECS resources within a specified target
-- type and cluster. When you specify a target type and cluster,
-- @ListAttributes@ returns a list of attribute objects, one for each
-- attribute on each resource. You can filter the list of results to a
-- single attribute name to only return results that have that name. You
-- can also filter the results by attribute name and value, for example, to
-- see which container instances in a cluster are running a Linux AMI
-- (@ecs.os-type=linux@).
--
-- This operation returns paginated results.
module Amazonka.ECS.ListAttributes
  ( -- * Creating a Request
    ListAttributes (..),
    newListAttributes,

    -- * Request Lenses
    listAttributes_attributeValue,
    listAttributes_cluster,
    listAttributes_nextToken,
    listAttributes_attributeName,
    listAttributes_maxResults,
    listAttributes_targetType,

    -- * Destructuring the Response
    ListAttributesResponse (..),
    newListAttributesResponse,

    -- * Response Lenses
    listAttributesResponse_nextToken,
    listAttributesResponse_attributes,
    listAttributesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.ECS.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListAttributes' smart constructor.
data ListAttributes = ListAttributes'
  { -- | The value of the attribute with which to filter results. You must also
    -- specify an attribute name to use this parameter.
    attributeValue :: Prelude.Maybe Prelude.Text,
    -- | The short name or full Amazon Resource Name (ARN) of the cluster to list
    -- attributes. If you do not specify a cluster, the default cluster is
    -- assumed.
    cluster :: Prelude.Maybe Prelude.Text,
    -- | The @nextToken@ value returned from a @ListAttributes@ request
    -- indicating that more results are available to fulfill the request and
    -- further calls will be needed. If @maxResults@ was provided, it is
    -- possible the number of results to be fewer than @maxResults@.
    --
    -- This token should be treated as an opaque identifier that is only used
    -- to retrieve the next items in a list and not for other programmatic
    -- purposes.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the attribute with which to filter the results.
    attributeName :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of cluster results returned by @ListAttributes@ in
    -- paginated output. When this parameter is used, @ListAttributes@ only
    -- returns @maxResults@ results in a single page along with a @nextToken@
    -- response element. The remaining results of the initial request can be
    -- seen by sending another @ListAttributes@ request with the returned
    -- @nextToken@ value. This value can be between 1 and 100. If this
    -- parameter is not used, then @ListAttributes@ returns up to 100 results
    -- and a @nextToken@ value if applicable.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | The type of the target with which to list attributes.
    targetType :: TargetType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributeValue', 'listAttributes_attributeValue' - The value of the attribute with which to filter results. You must also
-- specify an attribute name to use this parameter.
--
-- 'cluster', 'listAttributes_cluster' - The short name or full Amazon Resource Name (ARN) of the cluster to list
-- attributes. If you do not specify a cluster, the default cluster is
-- assumed.
--
-- 'nextToken', 'listAttributes_nextToken' - The @nextToken@ value returned from a @ListAttributes@ request
-- indicating that more results are available to fulfill the request and
-- further calls will be needed. If @maxResults@ was provided, it is
-- possible the number of results to be fewer than @maxResults@.
--
-- This token should be treated as an opaque identifier that is only used
-- to retrieve the next items in a list and not for other programmatic
-- purposes.
--
-- 'attributeName', 'listAttributes_attributeName' - The name of the attribute with which to filter the results.
--
-- 'maxResults', 'listAttributes_maxResults' - The maximum number of cluster results returned by @ListAttributes@ in
-- paginated output. When this parameter is used, @ListAttributes@ only
-- returns @maxResults@ results in a single page along with a @nextToken@
-- response element. The remaining results of the initial request can be
-- seen by sending another @ListAttributes@ request with the returned
-- @nextToken@ value. This value can be between 1 and 100. If this
-- parameter is not used, then @ListAttributes@ returns up to 100 results
-- and a @nextToken@ value if applicable.
--
-- 'targetType', 'listAttributes_targetType' - The type of the target with which to list attributes.
newListAttributes ::
  -- | 'targetType'
  TargetType ->
  ListAttributes
newListAttributes pTargetType_ =
  ListAttributes'
    { attributeValue = Prelude.Nothing,
      cluster = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      attributeName = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      targetType = pTargetType_
    }

-- | The value of the attribute with which to filter results. You must also
-- specify an attribute name to use this parameter.
listAttributes_attributeValue :: Lens.Lens' ListAttributes (Prelude.Maybe Prelude.Text)
listAttributes_attributeValue = Lens.lens (\ListAttributes' {attributeValue} -> attributeValue) (\s@ListAttributes' {} a -> s {attributeValue = a} :: ListAttributes)

-- | The short name or full Amazon Resource Name (ARN) of the cluster to list
-- attributes. If you do not specify a cluster, the default cluster is
-- assumed.
listAttributes_cluster :: Lens.Lens' ListAttributes (Prelude.Maybe Prelude.Text)
listAttributes_cluster = Lens.lens (\ListAttributes' {cluster} -> cluster) (\s@ListAttributes' {} a -> s {cluster = a} :: ListAttributes)

-- | The @nextToken@ value returned from a @ListAttributes@ request
-- indicating that more results are available to fulfill the request and
-- further calls will be needed. If @maxResults@ was provided, it is
-- possible the number of results to be fewer than @maxResults@.
--
-- This token should be treated as an opaque identifier that is only used
-- to retrieve the next items in a list and not for other programmatic
-- purposes.
listAttributes_nextToken :: Lens.Lens' ListAttributes (Prelude.Maybe Prelude.Text)
listAttributes_nextToken = Lens.lens (\ListAttributes' {nextToken} -> nextToken) (\s@ListAttributes' {} a -> s {nextToken = a} :: ListAttributes)

-- | The name of the attribute with which to filter the results.
listAttributes_attributeName :: Lens.Lens' ListAttributes (Prelude.Maybe Prelude.Text)
listAttributes_attributeName = Lens.lens (\ListAttributes' {attributeName} -> attributeName) (\s@ListAttributes' {} a -> s {attributeName = a} :: ListAttributes)

-- | The maximum number of cluster results returned by @ListAttributes@ in
-- paginated output. When this parameter is used, @ListAttributes@ only
-- returns @maxResults@ results in a single page along with a @nextToken@
-- response element. The remaining results of the initial request can be
-- seen by sending another @ListAttributes@ request with the returned
-- @nextToken@ value. This value can be between 1 and 100. If this
-- parameter is not used, then @ListAttributes@ returns up to 100 results
-- and a @nextToken@ value if applicable.
listAttributes_maxResults :: Lens.Lens' ListAttributes (Prelude.Maybe Prelude.Int)
listAttributes_maxResults = Lens.lens (\ListAttributes' {maxResults} -> maxResults) (\s@ListAttributes' {} a -> s {maxResults = a} :: ListAttributes)

-- | The type of the target with which to list attributes.
listAttributes_targetType :: Lens.Lens' ListAttributes TargetType
listAttributes_targetType = Lens.lens (\ListAttributes' {targetType} -> targetType) (\s@ListAttributes' {} a -> s {targetType = a} :: ListAttributes)

instance Core.AWSPager ListAttributes where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listAttributesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listAttributesResponse_attributes
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listAttributes_nextToken
          Lens..~ rs
          Lens.^? listAttributesResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListAttributes where
  type
    AWSResponse ListAttributes =
      ListAttributesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAttributesResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> (x Core..?> "attributes" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListAttributes where
  hashWithSalt _salt ListAttributes' {..} =
    _salt `Prelude.hashWithSalt` attributeValue
      `Prelude.hashWithSalt` cluster
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` attributeName
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` targetType

instance Prelude.NFData ListAttributes where
  rnf ListAttributes' {..} =
    Prelude.rnf attributeValue
      `Prelude.seq` Prelude.rnf cluster
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf attributeName
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf targetType

instance Core.ToHeaders ListAttributes where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonEC2ContainerServiceV20141113.ListAttributes" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListAttributes where
  toJSON ListAttributes' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("attributeValue" Core..=)
              Prelude.<$> attributeValue,
            ("cluster" Core..=) Prelude.<$> cluster,
            ("nextToken" Core..=) Prelude.<$> nextToken,
            ("attributeName" Core..=) Prelude.<$> attributeName,
            ("maxResults" Core..=) Prelude.<$> maxResults,
            Prelude.Just ("targetType" Core..= targetType)
          ]
      )

instance Core.ToPath ListAttributes where
  toPath = Prelude.const "/"

instance Core.ToQuery ListAttributes where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListAttributesResponse' smart constructor.
data ListAttributesResponse = ListAttributesResponse'
  { -- | The @nextToken@ value to include in a future @ListAttributes@ request.
    -- When the results of a @ListAttributes@ request exceed @maxResults@, this
    -- value can be used to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of attribute objects that meet the criteria of the request.
    attributes :: Prelude.Maybe [Attribute],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAttributesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listAttributesResponse_nextToken' - The @nextToken@ value to include in a future @ListAttributes@ request.
-- When the results of a @ListAttributes@ request exceed @maxResults@, this
-- value can be used to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'attributes', 'listAttributesResponse_attributes' - A list of attribute objects that meet the criteria of the request.
--
-- 'httpStatus', 'listAttributesResponse_httpStatus' - The response's http status code.
newListAttributesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListAttributesResponse
newListAttributesResponse pHttpStatus_ =
  ListAttributesResponse'
    { nextToken =
        Prelude.Nothing,
      attributes = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The @nextToken@ value to include in a future @ListAttributes@ request.
-- When the results of a @ListAttributes@ request exceed @maxResults@, this
-- value can be used to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
listAttributesResponse_nextToken :: Lens.Lens' ListAttributesResponse (Prelude.Maybe Prelude.Text)
listAttributesResponse_nextToken = Lens.lens (\ListAttributesResponse' {nextToken} -> nextToken) (\s@ListAttributesResponse' {} a -> s {nextToken = a} :: ListAttributesResponse)

-- | A list of attribute objects that meet the criteria of the request.
listAttributesResponse_attributes :: Lens.Lens' ListAttributesResponse (Prelude.Maybe [Attribute])
listAttributesResponse_attributes = Lens.lens (\ListAttributesResponse' {attributes} -> attributes) (\s@ListAttributesResponse' {} a -> s {attributes = a} :: ListAttributesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listAttributesResponse_httpStatus :: Lens.Lens' ListAttributesResponse Prelude.Int
listAttributesResponse_httpStatus = Lens.lens (\ListAttributesResponse' {httpStatus} -> httpStatus) (\s@ListAttributesResponse' {} a -> s {httpStatus = a} :: ListAttributesResponse)

instance Prelude.NFData ListAttributesResponse where
  rnf ListAttributesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf attributes
      `Prelude.seq` Prelude.rnf httpStatus
