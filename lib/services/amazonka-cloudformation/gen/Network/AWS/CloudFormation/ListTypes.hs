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
-- Module      : Network.AWS.CloudFormation.ListTypes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns summary information about extension that have been registered
-- with CloudFormation.
--
-- This operation returns paginated results.
module Network.AWS.CloudFormation.ListTypes
  ( -- * Creating a Request
    ListTypes (..),
    newListTypes,

    -- * Request Lenses
    listTypes_filters,
    listTypes_visibility,
    listTypes_nextToken,
    listTypes_deprecatedStatus,
    listTypes_type,
    listTypes_maxResults,
    listTypes_provisioningType,

    -- * Destructuring the Response
    ListTypesResponse (..),
    newListTypesResponse,

    -- * Response Lenses
    listTypesResponse_typeSummaries,
    listTypesResponse_nextToken,
    listTypesResponse_httpStatus,
  )
where

import Network.AWS.CloudFormation.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListTypes' smart constructor.
data ListTypes = ListTypes'
  { -- | Filter criteria to use in determining which extensions to return.
    --
    -- If you specify a filter, CloudFormation ignores any specified
    -- @Visibility@ value when returning the list of types.
    filters :: Prelude.Maybe TypeFilters,
    -- | The scope at which the extensions are visible and usable in
    -- CloudFormation operations.
    --
    -- Valid values include:
    --
    -- -   @PRIVATE@: Extensions that are visible and usable within this
    --     account and region. This includes:
    --
    --     -   Private extensions you have registered in this account and
    --         region.
    --
    --     -   Public extensions that you have activated in this account and
    --         region.
    --
    -- -   @PUBLIC@: Extensions that are publicly visible and available to be
    --     activated within any Amazon account. This includes extensions from
    --     Amazon, as well as third-party publishers.
    --
    -- The default is @PRIVATE@.
    visibility :: Prelude.Maybe Visibility,
    -- | If the previous paginated request didn\'t return all of the remaining
    -- results, the response object\'s @NextToken@ parameter value is set to a
    -- token. To retrieve the next set of results, call this action again and
    -- assign that token to the request object\'s @NextToken@ parameter. If
    -- there are no remaining results, the previous response object\'s
    -- @NextToken@ parameter is set to @null@.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The deprecation status of the extension that you want to get summary
    -- information about.
    --
    -- Valid values include:
    --
    -- -   @LIVE@: The extension is registered for use in CloudFormation
    --     operations.
    --
    -- -   @DEPRECATED@: The extension has been deregistered and can no longer
    --     be used in CloudFormation operations.
    deprecatedStatus :: Prelude.Maybe DeprecatedStatus,
    -- | The type of extension.
    type' :: Prelude.Maybe RegistryType,
    -- | The maximum number of results to be returned with a single call. If the
    -- number of available results exceeds this maximum, the response includes
    -- a @NextToken@ value that you can assign to the @NextToken@ request
    -- parameter to get the next set of results.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | For resource types, the provisioning behavior of the resource type.
    -- CloudFormation determines the provisioning type during registration,
    -- based on the types of handlers in the schema handler package submitted.
    --
    -- Valid values include:
    --
    -- -   @FULLY_MUTABLE@: The resource type includes an update handler to
    --     process updates to the type during stack update operations.
    --
    -- -   @IMMUTABLE@: The resource type does not include an update handler,
    --     so the type cannot be updated and must instead be replaced during
    --     stack update operations.
    --
    -- -   @NON_PROVISIONABLE@: The resource type does not include create,
    --     read, and delete handlers, and therefore cannot actually be
    --     provisioned.
    --
    -- The default is @FULLY_MUTABLE@.
    provisioningType :: Prelude.Maybe ProvisioningType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTypes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filters', 'listTypes_filters' - Filter criteria to use in determining which extensions to return.
--
-- If you specify a filter, CloudFormation ignores any specified
-- @Visibility@ value when returning the list of types.
--
-- 'visibility', 'listTypes_visibility' - The scope at which the extensions are visible and usable in
-- CloudFormation operations.
--
-- Valid values include:
--
-- -   @PRIVATE@: Extensions that are visible and usable within this
--     account and region. This includes:
--
--     -   Private extensions you have registered in this account and
--         region.
--
--     -   Public extensions that you have activated in this account and
--         region.
--
-- -   @PUBLIC@: Extensions that are publicly visible and available to be
--     activated within any Amazon account. This includes extensions from
--     Amazon, as well as third-party publishers.
--
-- The default is @PRIVATE@.
--
-- 'nextToken', 'listTypes_nextToken' - If the previous paginated request didn\'t return all of the remaining
-- results, the response object\'s @NextToken@ parameter value is set to a
-- token. To retrieve the next set of results, call this action again and
-- assign that token to the request object\'s @NextToken@ parameter. If
-- there are no remaining results, the previous response object\'s
-- @NextToken@ parameter is set to @null@.
--
-- 'deprecatedStatus', 'listTypes_deprecatedStatus' - The deprecation status of the extension that you want to get summary
-- information about.
--
-- Valid values include:
--
-- -   @LIVE@: The extension is registered for use in CloudFormation
--     operations.
--
-- -   @DEPRECATED@: The extension has been deregistered and can no longer
--     be used in CloudFormation operations.
--
-- 'type'', 'listTypes_type' - The type of extension.
--
-- 'maxResults', 'listTypes_maxResults' - The maximum number of results to be returned with a single call. If the
-- number of available results exceeds this maximum, the response includes
-- a @NextToken@ value that you can assign to the @NextToken@ request
-- parameter to get the next set of results.
--
-- 'provisioningType', 'listTypes_provisioningType' - For resource types, the provisioning behavior of the resource type.
-- CloudFormation determines the provisioning type during registration,
-- based on the types of handlers in the schema handler package submitted.
--
-- Valid values include:
--
-- -   @FULLY_MUTABLE@: The resource type includes an update handler to
--     process updates to the type during stack update operations.
--
-- -   @IMMUTABLE@: The resource type does not include an update handler,
--     so the type cannot be updated and must instead be replaced during
--     stack update operations.
--
-- -   @NON_PROVISIONABLE@: The resource type does not include create,
--     read, and delete handlers, and therefore cannot actually be
--     provisioned.
--
-- The default is @FULLY_MUTABLE@.
newListTypes ::
  ListTypes
newListTypes =
  ListTypes'
    { filters = Prelude.Nothing,
      visibility = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      deprecatedStatus = Prelude.Nothing,
      type' = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      provisioningType = Prelude.Nothing
    }

-- | Filter criteria to use in determining which extensions to return.
--
-- If you specify a filter, CloudFormation ignores any specified
-- @Visibility@ value when returning the list of types.
listTypes_filters :: Lens.Lens' ListTypes (Prelude.Maybe TypeFilters)
listTypes_filters = Lens.lens (\ListTypes' {filters} -> filters) (\s@ListTypes' {} a -> s {filters = a} :: ListTypes)

-- | The scope at which the extensions are visible and usable in
-- CloudFormation operations.
--
-- Valid values include:
--
-- -   @PRIVATE@: Extensions that are visible and usable within this
--     account and region. This includes:
--
--     -   Private extensions you have registered in this account and
--         region.
--
--     -   Public extensions that you have activated in this account and
--         region.
--
-- -   @PUBLIC@: Extensions that are publicly visible and available to be
--     activated within any Amazon account. This includes extensions from
--     Amazon, as well as third-party publishers.
--
-- The default is @PRIVATE@.
listTypes_visibility :: Lens.Lens' ListTypes (Prelude.Maybe Visibility)
listTypes_visibility = Lens.lens (\ListTypes' {visibility} -> visibility) (\s@ListTypes' {} a -> s {visibility = a} :: ListTypes)

-- | If the previous paginated request didn\'t return all of the remaining
-- results, the response object\'s @NextToken@ parameter value is set to a
-- token. To retrieve the next set of results, call this action again and
-- assign that token to the request object\'s @NextToken@ parameter. If
-- there are no remaining results, the previous response object\'s
-- @NextToken@ parameter is set to @null@.
listTypes_nextToken :: Lens.Lens' ListTypes (Prelude.Maybe Prelude.Text)
listTypes_nextToken = Lens.lens (\ListTypes' {nextToken} -> nextToken) (\s@ListTypes' {} a -> s {nextToken = a} :: ListTypes)

-- | The deprecation status of the extension that you want to get summary
-- information about.
--
-- Valid values include:
--
-- -   @LIVE@: The extension is registered for use in CloudFormation
--     operations.
--
-- -   @DEPRECATED@: The extension has been deregistered and can no longer
--     be used in CloudFormation operations.
listTypes_deprecatedStatus :: Lens.Lens' ListTypes (Prelude.Maybe DeprecatedStatus)
listTypes_deprecatedStatus = Lens.lens (\ListTypes' {deprecatedStatus} -> deprecatedStatus) (\s@ListTypes' {} a -> s {deprecatedStatus = a} :: ListTypes)

-- | The type of extension.
listTypes_type :: Lens.Lens' ListTypes (Prelude.Maybe RegistryType)
listTypes_type = Lens.lens (\ListTypes' {type'} -> type') (\s@ListTypes' {} a -> s {type' = a} :: ListTypes)

-- | The maximum number of results to be returned with a single call. If the
-- number of available results exceeds this maximum, the response includes
-- a @NextToken@ value that you can assign to the @NextToken@ request
-- parameter to get the next set of results.
listTypes_maxResults :: Lens.Lens' ListTypes (Prelude.Maybe Prelude.Natural)
listTypes_maxResults = Lens.lens (\ListTypes' {maxResults} -> maxResults) (\s@ListTypes' {} a -> s {maxResults = a} :: ListTypes)

-- | For resource types, the provisioning behavior of the resource type.
-- CloudFormation determines the provisioning type during registration,
-- based on the types of handlers in the schema handler package submitted.
--
-- Valid values include:
--
-- -   @FULLY_MUTABLE@: The resource type includes an update handler to
--     process updates to the type during stack update operations.
--
-- -   @IMMUTABLE@: The resource type does not include an update handler,
--     so the type cannot be updated and must instead be replaced during
--     stack update operations.
--
-- -   @NON_PROVISIONABLE@: The resource type does not include create,
--     read, and delete handlers, and therefore cannot actually be
--     provisioned.
--
-- The default is @FULLY_MUTABLE@.
listTypes_provisioningType :: Lens.Lens' ListTypes (Prelude.Maybe ProvisioningType)
listTypes_provisioningType = Lens.lens (\ListTypes' {provisioningType} -> provisioningType) (\s@ListTypes' {} a -> s {provisioningType = a} :: ListTypes)

instance Core.AWSPager ListTypes where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listTypesResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listTypesResponse_typeSummaries Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listTypes_nextToken
          Lens..~ rs
          Lens.^? listTypesResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListTypes where
  type AWSResponse ListTypes = ListTypesResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "ListTypesResult"
      ( \s h x ->
          ListTypesResponse'
            Prelude.<$> ( x Core..@? "TypeSummaries" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "member")
                        )
            Prelude.<*> (x Core..@? "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListTypes

instance Prelude.NFData ListTypes

instance Core.ToHeaders ListTypes where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ListTypes where
  toPath = Prelude.const "/"

instance Core.ToQuery ListTypes where
  toQuery ListTypes' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("ListTypes" :: Prelude.ByteString),
        "Version"
          Core.=: ("2010-05-15" :: Prelude.ByteString),
        "Filters" Core.=: filters,
        "Visibility" Core.=: visibility,
        "NextToken" Core.=: nextToken,
        "DeprecatedStatus" Core.=: deprecatedStatus,
        "Type" Core.=: type',
        "MaxResults" Core.=: maxResults,
        "ProvisioningType" Core.=: provisioningType
      ]

-- | /See:/ 'newListTypesResponse' smart constructor.
data ListTypesResponse = ListTypesResponse'
  { -- | A list of @TypeSummary@ structures that contain information about the
    -- specified extensions.
    typeSummaries :: Prelude.Maybe [TypeSummary],
    -- | If the request doesn\'t return all of the remaining results, @NextToken@
    -- is set to a token. To retrieve the next set of results, call this action
    -- again and assign that token to the request object\'s @NextToken@
    -- parameter. If the request returns all results, @NextToken@ is set to
    -- @null@.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTypesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'typeSummaries', 'listTypesResponse_typeSummaries' - A list of @TypeSummary@ structures that contain information about the
-- specified extensions.
--
-- 'nextToken', 'listTypesResponse_nextToken' - If the request doesn\'t return all of the remaining results, @NextToken@
-- is set to a token. To retrieve the next set of results, call this action
-- again and assign that token to the request object\'s @NextToken@
-- parameter. If the request returns all results, @NextToken@ is set to
-- @null@.
--
-- 'httpStatus', 'listTypesResponse_httpStatus' - The response's http status code.
newListTypesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListTypesResponse
newListTypesResponse pHttpStatus_ =
  ListTypesResponse'
    { typeSummaries = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of @TypeSummary@ structures that contain information about the
-- specified extensions.
listTypesResponse_typeSummaries :: Lens.Lens' ListTypesResponse (Prelude.Maybe [TypeSummary])
listTypesResponse_typeSummaries = Lens.lens (\ListTypesResponse' {typeSummaries} -> typeSummaries) (\s@ListTypesResponse' {} a -> s {typeSummaries = a} :: ListTypesResponse) Prelude.. Lens.mapping Lens.coerced

-- | If the request doesn\'t return all of the remaining results, @NextToken@
-- is set to a token. To retrieve the next set of results, call this action
-- again and assign that token to the request object\'s @NextToken@
-- parameter. If the request returns all results, @NextToken@ is set to
-- @null@.
listTypesResponse_nextToken :: Lens.Lens' ListTypesResponse (Prelude.Maybe Prelude.Text)
listTypesResponse_nextToken = Lens.lens (\ListTypesResponse' {nextToken} -> nextToken) (\s@ListTypesResponse' {} a -> s {nextToken = a} :: ListTypesResponse)

-- | The response's http status code.
listTypesResponse_httpStatus :: Lens.Lens' ListTypesResponse Prelude.Int
listTypesResponse_httpStatus = Lens.lens (\ListTypesResponse' {httpStatus} -> httpStatus) (\s@ListTypesResponse' {} a -> s {httpStatus = a} :: ListTypesResponse)

instance Prelude.NFData ListTypesResponse
