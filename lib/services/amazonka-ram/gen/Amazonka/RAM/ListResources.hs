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
-- Module      : Amazonka.RAM.ListResources
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the resources that you added to a resource share or the resources
-- that are shared with you.
--
-- This operation returns paginated results.
module Amazonka.RAM.ListResources
  ( -- * Creating a Request
    ListResources (..),
    newListResources,

    -- * Request Lenses
    listResources_maxResults,
    listResources_nextToken,
    listResources_principal,
    listResources_resourceArns,
    listResources_resourceRegionScope,
    listResources_resourceShareArns,
    listResources_resourceType,
    listResources_resourceOwner,

    -- * Destructuring the Response
    ListResourcesResponse (..),
    newListResourcesResponse,

    -- * Response Lenses
    listResourcesResponse_nextToken,
    listResourcesResponse_resources,
    listResourcesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RAM.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListResources' smart constructor.
data ListResources = ListResources'
  { -- | Specifies the total number of results that you want included on each
    -- page of the response. If you do not include this parameter, it defaults
    -- to a value that is specific to the operation. If additional items exist
    -- beyond the number you specify, the @NextToken@ response element is
    -- returned with a value (not null). Include the specified value as the
    -- @NextToken@ request parameter in the next call to the operation to get
    -- the next part of the results. Note that the service might return fewer
    -- results than the maximum even when there are more results available. You
    -- should check @NextToken@ after every operation to ensure that you
    -- receive all of the results.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Specifies that you want to receive the next page of results. Valid only
    -- if you received a @NextToken@ response in the previous request. If you
    -- did, it indicates that more output is available. Set this parameter to
    -- the value provided by the previous call\'s @NextToken@ response to
    -- request the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Specifies that you want to list only the resource shares that are
    -- associated with the specified principal.
    principal :: Prelude.Maybe Prelude.Text,
    -- | Specifies that you want to list only the resource shares that include
    -- resources with the specified
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>.
    resourceArns :: Prelude.Maybe [Prelude.Text],
    -- | Specifies that you want the results to include only resources that have
    -- the specified scope.
    --
    -- -   @ALL@ – the results include both global and regional resources or
    --     resource types.
    --
    -- -   @GLOBAL@ – the results include only global resources or resource
    --     types.
    --
    -- -   @REGIONAL@ – the results include only regional resources or resource
    --     types.
    --
    -- The default value is @ALL@.
    resourceRegionScope :: Prelude.Maybe ResourceRegionScopeFilter,
    -- | Specifies that you want to list only resources in the resource shares
    -- identified by the specified
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>.
    resourceShareArns :: Prelude.Maybe [Prelude.Text],
    -- | Specifies that you want to list only the resource shares that include
    -- resources of the specified resource type.
    --
    -- For valid values, query the ListResourceTypes operation.
    resourceType :: Prelude.Maybe Prelude.Text,
    -- | Specifies that you want to list only the resource shares that match the
    -- following:
    --
    -- -   __@SELF@__ – resources that your account shares with other accounts
    --
    -- -   __@OTHER-ACCOUNTS@__ – resources that other accounts share with your
    --     account
    resourceOwner :: ResourceOwner
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListResources' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listResources_maxResults' - Specifies the total number of results that you want included on each
-- page of the response. If you do not include this parameter, it defaults
-- to a value that is specific to the operation. If additional items exist
-- beyond the number you specify, the @NextToken@ response element is
-- returned with a value (not null). Include the specified value as the
-- @NextToken@ request parameter in the next call to the operation to get
-- the next part of the results. Note that the service might return fewer
-- results than the maximum even when there are more results available. You
-- should check @NextToken@ after every operation to ensure that you
-- receive all of the results.
--
-- 'nextToken', 'listResources_nextToken' - Specifies that you want to receive the next page of results. Valid only
-- if you received a @NextToken@ response in the previous request. If you
-- did, it indicates that more output is available. Set this parameter to
-- the value provided by the previous call\'s @NextToken@ response to
-- request the next page of results.
--
-- 'principal', 'listResources_principal' - Specifies that you want to list only the resource shares that are
-- associated with the specified principal.
--
-- 'resourceArns', 'listResources_resourceArns' - Specifies that you want to list only the resource shares that include
-- resources with the specified
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>.
--
-- 'resourceRegionScope', 'listResources_resourceRegionScope' - Specifies that you want the results to include only resources that have
-- the specified scope.
--
-- -   @ALL@ – the results include both global and regional resources or
--     resource types.
--
-- -   @GLOBAL@ – the results include only global resources or resource
--     types.
--
-- -   @REGIONAL@ – the results include only regional resources or resource
--     types.
--
-- The default value is @ALL@.
--
-- 'resourceShareArns', 'listResources_resourceShareArns' - Specifies that you want to list only resources in the resource shares
-- identified by the specified
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>.
--
-- 'resourceType', 'listResources_resourceType' - Specifies that you want to list only the resource shares that include
-- resources of the specified resource type.
--
-- For valid values, query the ListResourceTypes operation.
--
-- 'resourceOwner', 'listResources_resourceOwner' - Specifies that you want to list only the resource shares that match the
-- following:
--
-- -   __@SELF@__ – resources that your account shares with other accounts
--
-- -   __@OTHER-ACCOUNTS@__ – resources that other accounts share with your
--     account
newListResources ::
  -- | 'resourceOwner'
  ResourceOwner ->
  ListResources
newListResources pResourceOwner_ =
  ListResources'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      principal = Prelude.Nothing,
      resourceArns = Prelude.Nothing,
      resourceRegionScope = Prelude.Nothing,
      resourceShareArns = Prelude.Nothing,
      resourceType = Prelude.Nothing,
      resourceOwner = pResourceOwner_
    }

-- | Specifies the total number of results that you want included on each
-- page of the response. If you do not include this parameter, it defaults
-- to a value that is specific to the operation. If additional items exist
-- beyond the number you specify, the @NextToken@ response element is
-- returned with a value (not null). Include the specified value as the
-- @NextToken@ request parameter in the next call to the operation to get
-- the next part of the results. Note that the service might return fewer
-- results than the maximum even when there are more results available. You
-- should check @NextToken@ after every operation to ensure that you
-- receive all of the results.
listResources_maxResults :: Lens.Lens' ListResources (Prelude.Maybe Prelude.Natural)
listResources_maxResults = Lens.lens (\ListResources' {maxResults} -> maxResults) (\s@ListResources' {} a -> s {maxResults = a} :: ListResources)

-- | Specifies that you want to receive the next page of results. Valid only
-- if you received a @NextToken@ response in the previous request. If you
-- did, it indicates that more output is available. Set this parameter to
-- the value provided by the previous call\'s @NextToken@ response to
-- request the next page of results.
listResources_nextToken :: Lens.Lens' ListResources (Prelude.Maybe Prelude.Text)
listResources_nextToken = Lens.lens (\ListResources' {nextToken} -> nextToken) (\s@ListResources' {} a -> s {nextToken = a} :: ListResources)

-- | Specifies that you want to list only the resource shares that are
-- associated with the specified principal.
listResources_principal :: Lens.Lens' ListResources (Prelude.Maybe Prelude.Text)
listResources_principal = Lens.lens (\ListResources' {principal} -> principal) (\s@ListResources' {} a -> s {principal = a} :: ListResources)

-- | Specifies that you want to list only the resource shares that include
-- resources with the specified
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>.
listResources_resourceArns :: Lens.Lens' ListResources (Prelude.Maybe [Prelude.Text])
listResources_resourceArns = Lens.lens (\ListResources' {resourceArns} -> resourceArns) (\s@ListResources' {} a -> s {resourceArns = a} :: ListResources) Prelude.. Lens.mapping Lens.coerced

-- | Specifies that you want the results to include only resources that have
-- the specified scope.
--
-- -   @ALL@ – the results include both global and regional resources or
--     resource types.
--
-- -   @GLOBAL@ – the results include only global resources or resource
--     types.
--
-- -   @REGIONAL@ – the results include only regional resources or resource
--     types.
--
-- The default value is @ALL@.
listResources_resourceRegionScope :: Lens.Lens' ListResources (Prelude.Maybe ResourceRegionScopeFilter)
listResources_resourceRegionScope = Lens.lens (\ListResources' {resourceRegionScope} -> resourceRegionScope) (\s@ListResources' {} a -> s {resourceRegionScope = a} :: ListResources)

-- | Specifies that you want to list only resources in the resource shares
-- identified by the specified
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>.
listResources_resourceShareArns :: Lens.Lens' ListResources (Prelude.Maybe [Prelude.Text])
listResources_resourceShareArns = Lens.lens (\ListResources' {resourceShareArns} -> resourceShareArns) (\s@ListResources' {} a -> s {resourceShareArns = a} :: ListResources) Prelude.. Lens.mapping Lens.coerced

-- | Specifies that you want to list only the resource shares that include
-- resources of the specified resource type.
--
-- For valid values, query the ListResourceTypes operation.
listResources_resourceType :: Lens.Lens' ListResources (Prelude.Maybe Prelude.Text)
listResources_resourceType = Lens.lens (\ListResources' {resourceType} -> resourceType) (\s@ListResources' {} a -> s {resourceType = a} :: ListResources)

-- | Specifies that you want to list only the resource shares that match the
-- following:
--
-- -   __@SELF@__ – resources that your account shares with other accounts
--
-- -   __@OTHER-ACCOUNTS@__ – resources that other accounts share with your
--     account
listResources_resourceOwner :: Lens.Lens' ListResources ResourceOwner
listResources_resourceOwner = Lens.lens (\ListResources' {resourceOwner} -> resourceOwner) (\s@ListResources' {} a -> s {resourceOwner = a} :: ListResources)

instance Core.AWSPager ListResources where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listResourcesResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listResourcesResponse_resources
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listResources_nextToken
          Lens..~ rs
          Lens.^? listResourcesResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListResources where
  type
    AWSResponse ListResources =
      ListResourcesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListResourcesResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (x Data..?> "resources" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListResources where
  hashWithSalt _salt ListResources' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` principal
      `Prelude.hashWithSalt` resourceArns
      `Prelude.hashWithSalt` resourceRegionScope
      `Prelude.hashWithSalt` resourceShareArns
      `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` resourceOwner

instance Prelude.NFData ListResources where
  rnf ListResources' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf principal
      `Prelude.seq` Prelude.rnf resourceArns
      `Prelude.seq` Prelude.rnf resourceRegionScope
      `Prelude.seq` Prelude.rnf resourceShareArns
      `Prelude.seq` Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf resourceOwner

instance Data.ToHeaders ListResources where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListResources where
  toJSON ListResources' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken,
            ("principal" Data..=) Prelude.<$> principal,
            ("resourceArns" Data..=) Prelude.<$> resourceArns,
            ("resourceRegionScope" Data..=)
              Prelude.<$> resourceRegionScope,
            ("resourceShareArns" Data..=)
              Prelude.<$> resourceShareArns,
            ("resourceType" Data..=) Prelude.<$> resourceType,
            Prelude.Just
              ("resourceOwner" Data..= resourceOwner)
          ]
      )

instance Data.ToPath ListResources where
  toPath = Prelude.const "/listresources"

instance Data.ToQuery ListResources where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListResourcesResponse' smart constructor.
data ListResourcesResponse = ListResourcesResponse'
  { -- | If present, this value indicates that more output is available than is
    -- included in the current response. Use this value in the @NextToken@
    -- request parameter in a subsequent call to the operation to get the next
    -- part of the output. You should repeat this until the @NextToken@
    -- response element comes back as @null@. This indicates that this is the
    -- last page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An array of objects that contain information about the resources.
    resources :: Prelude.Maybe [Resource],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListResourcesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listResourcesResponse_nextToken' - If present, this value indicates that more output is available than is
-- included in the current response. Use this value in the @NextToken@
-- request parameter in a subsequent call to the operation to get the next
-- part of the output. You should repeat this until the @NextToken@
-- response element comes back as @null@. This indicates that this is the
-- last page of results.
--
-- 'resources', 'listResourcesResponse_resources' - An array of objects that contain information about the resources.
--
-- 'httpStatus', 'listResourcesResponse_httpStatus' - The response's http status code.
newListResourcesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListResourcesResponse
newListResourcesResponse pHttpStatus_ =
  ListResourcesResponse'
    { nextToken = Prelude.Nothing,
      resources = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If present, this value indicates that more output is available than is
-- included in the current response. Use this value in the @NextToken@
-- request parameter in a subsequent call to the operation to get the next
-- part of the output. You should repeat this until the @NextToken@
-- response element comes back as @null@. This indicates that this is the
-- last page of results.
listResourcesResponse_nextToken :: Lens.Lens' ListResourcesResponse (Prelude.Maybe Prelude.Text)
listResourcesResponse_nextToken = Lens.lens (\ListResourcesResponse' {nextToken} -> nextToken) (\s@ListResourcesResponse' {} a -> s {nextToken = a} :: ListResourcesResponse)

-- | An array of objects that contain information about the resources.
listResourcesResponse_resources :: Lens.Lens' ListResourcesResponse (Prelude.Maybe [Resource])
listResourcesResponse_resources = Lens.lens (\ListResourcesResponse' {resources} -> resources) (\s@ListResourcesResponse' {} a -> s {resources = a} :: ListResourcesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listResourcesResponse_httpStatus :: Lens.Lens' ListResourcesResponse Prelude.Int
listResourcesResponse_httpStatus = Lens.lens (\ListResourcesResponse' {httpStatus} -> httpStatus) (\s@ListResourcesResponse' {} a -> s {httpStatus = a} :: ListResourcesResponse)

instance Prelude.NFData ListResourcesResponse where
  rnf ListResourcesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf resources
      `Prelude.seq` Prelude.rnf httpStatus
