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
-- Module      : Amazonka.CloudFormation.ListTypes
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns summary information about extension that have been registered
-- with CloudFormation.
--
-- This operation returns paginated results.
module Amazonka.CloudFormation.ListTypes
  ( -- * Creating a Request
    ListTypes (..),
    newListTypes,

    -- * Request Lenses
    listTypes_deprecatedStatus,
    listTypes_filters,
    listTypes_maxResults,
    listTypes_nextToken,
    listTypes_provisioningType,
    listTypes_type,
    listTypes_visibility,

    -- * Destructuring the Response
    ListTypesResponse (..),
    newListTypesResponse,

    -- * Response Lenses
    listTypesResponse_nextToken,
    listTypesResponse_typeSummaries,
    listTypesResponse_httpStatus,
  )
where

import Amazonka.CloudFormation.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListTypes' smart constructor.
data ListTypes = ListTypes'
  { -- | The deprecation status of the extension that you want to get summary
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
    -- | Filter criteria to use in determining which extensions to return.
    --
    -- Filters must be compatible with @Visibility@ to return valid results.
    -- For example, specifying @AWS_TYPES@ for @Category@ and @PRIVATE@ for
    -- @Visibility@ returns an empty list of types, but specifying @PUBLIC@ for
    -- @Visibility@ returns the desired list.
    filters :: Prelude.Maybe TypeFilters,
    -- | The maximum number of results to be returned with a single call. If the
    -- number of available results exceeds this maximum, the response includes
    -- a @NextToken@ value that you can assign to the @NextToken@ request
    -- parameter to get the next set of results.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | If the previous paginated request didn\'t return all the remaining
    -- results, the response object\'s @NextToken@ parameter value is set to a
    -- token. To retrieve the next set of results, call this action again and
    -- assign that token to the request object\'s @NextToken@ parameter. If
    -- there are no remaining results, the previous response object\'s
    -- @NextToken@ parameter is set to @null@.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | For resource types, the provisioning behavior of the resource type.
    -- CloudFormation determines the provisioning type during registration,
    -- based on the types of handlers in the schema handler package submitted.
    --
    -- Valid values include:
    --
    -- -   @FULLY_MUTABLE@: The resource type includes an update handler to
    --     process updates to the type during stack update operations.
    --
    -- -   @IMMUTABLE@: The resource type doesn\'t include an update handler,
    --     so the type can\'t be updated and must instead be replaced during
    --     stack update operations.
    --
    -- -   @NON_PROVISIONABLE@: The resource type doesn\'t include create,
    --     read, and delete handlers, and therefore can\'t actually be
    --     provisioned.
    --
    -- The default is @FULLY_MUTABLE@.
    provisioningType :: Prelude.Maybe ProvisioningType,
    -- | The type of extension.
    type' :: Prelude.Maybe RegistryType,
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
    --     activated within any Amazon Web Services account. This includes
    --     extensions from Amazon Web Services, in addition to third-party
    --     publishers.
    --
    -- The default is @PRIVATE@.
    visibility :: Prelude.Maybe Visibility
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
-- 'filters', 'listTypes_filters' - Filter criteria to use in determining which extensions to return.
--
-- Filters must be compatible with @Visibility@ to return valid results.
-- For example, specifying @AWS_TYPES@ for @Category@ and @PRIVATE@ for
-- @Visibility@ returns an empty list of types, but specifying @PUBLIC@ for
-- @Visibility@ returns the desired list.
--
-- 'maxResults', 'listTypes_maxResults' - The maximum number of results to be returned with a single call. If the
-- number of available results exceeds this maximum, the response includes
-- a @NextToken@ value that you can assign to the @NextToken@ request
-- parameter to get the next set of results.
--
-- 'nextToken', 'listTypes_nextToken' - If the previous paginated request didn\'t return all the remaining
-- results, the response object\'s @NextToken@ parameter value is set to a
-- token. To retrieve the next set of results, call this action again and
-- assign that token to the request object\'s @NextToken@ parameter. If
-- there are no remaining results, the previous response object\'s
-- @NextToken@ parameter is set to @null@.
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
-- -   @IMMUTABLE@: The resource type doesn\'t include an update handler,
--     so the type can\'t be updated and must instead be replaced during
--     stack update operations.
--
-- -   @NON_PROVISIONABLE@: The resource type doesn\'t include create,
--     read, and delete handlers, and therefore can\'t actually be
--     provisioned.
--
-- The default is @FULLY_MUTABLE@.
--
-- 'type'', 'listTypes_type' - The type of extension.
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
--     activated within any Amazon Web Services account. This includes
--     extensions from Amazon Web Services, in addition to third-party
--     publishers.
--
-- The default is @PRIVATE@.
newListTypes ::
  ListTypes
newListTypes =
  ListTypes'
    { deprecatedStatus = Prelude.Nothing,
      filters = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      provisioningType = Prelude.Nothing,
      type' = Prelude.Nothing,
      visibility = Prelude.Nothing
    }

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

-- | Filter criteria to use in determining which extensions to return.
--
-- Filters must be compatible with @Visibility@ to return valid results.
-- For example, specifying @AWS_TYPES@ for @Category@ and @PRIVATE@ for
-- @Visibility@ returns an empty list of types, but specifying @PUBLIC@ for
-- @Visibility@ returns the desired list.
listTypes_filters :: Lens.Lens' ListTypes (Prelude.Maybe TypeFilters)
listTypes_filters = Lens.lens (\ListTypes' {filters} -> filters) (\s@ListTypes' {} a -> s {filters = a} :: ListTypes)

-- | The maximum number of results to be returned with a single call. If the
-- number of available results exceeds this maximum, the response includes
-- a @NextToken@ value that you can assign to the @NextToken@ request
-- parameter to get the next set of results.
listTypes_maxResults :: Lens.Lens' ListTypes (Prelude.Maybe Prelude.Natural)
listTypes_maxResults = Lens.lens (\ListTypes' {maxResults} -> maxResults) (\s@ListTypes' {} a -> s {maxResults = a} :: ListTypes)

-- | If the previous paginated request didn\'t return all the remaining
-- results, the response object\'s @NextToken@ parameter value is set to a
-- token. To retrieve the next set of results, call this action again and
-- assign that token to the request object\'s @NextToken@ parameter. If
-- there are no remaining results, the previous response object\'s
-- @NextToken@ parameter is set to @null@.
listTypes_nextToken :: Lens.Lens' ListTypes (Prelude.Maybe Prelude.Text)
listTypes_nextToken = Lens.lens (\ListTypes' {nextToken} -> nextToken) (\s@ListTypes' {} a -> s {nextToken = a} :: ListTypes)

-- | For resource types, the provisioning behavior of the resource type.
-- CloudFormation determines the provisioning type during registration,
-- based on the types of handlers in the schema handler package submitted.
--
-- Valid values include:
--
-- -   @FULLY_MUTABLE@: The resource type includes an update handler to
--     process updates to the type during stack update operations.
--
-- -   @IMMUTABLE@: The resource type doesn\'t include an update handler,
--     so the type can\'t be updated and must instead be replaced during
--     stack update operations.
--
-- -   @NON_PROVISIONABLE@: The resource type doesn\'t include create,
--     read, and delete handlers, and therefore can\'t actually be
--     provisioned.
--
-- The default is @FULLY_MUTABLE@.
listTypes_provisioningType :: Lens.Lens' ListTypes (Prelude.Maybe ProvisioningType)
listTypes_provisioningType = Lens.lens (\ListTypes' {provisioningType} -> provisioningType) (\s@ListTypes' {} a -> s {provisioningType = a} :: ListTypes)

-- | The type of extension.
listTypes_type :: Lens.Lens' ListTypes (Prelude.Maybe RegistryType)
listTypes_type = Lens.lens (\ListTypes' {type'} -> type') (\s@ListTypes' {} a -> s {type' = a} :: ListTypes)

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
--     activated within any Amazon Web Services account. This includes
--     extensions from Amazon Web Services, in addition to third-party
--     publishers.
--
-- The default is @PRIVATE@.
listTypes_visibility :: Lens.Lens' ListTypes (Prelude.Maybe Visibility)
listTypes_visibility = Lens.lens (\ListTypes' {visibility} -> visibility) (\s@ListTypes' {} a -> s {visibility = a} :: ListTypes)

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
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "ListTypesResult"
      ( \s h x ->
          ListTypesResponse'
            Prelude.<$> (x Data..@? "NextToken")
            Prelude.<*> ( x Data..@? "TypeSummaries" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "member")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListTypes where
  hashWithSalt _salt ListTypes' {..} =
    _salt `Prelude.hashWithSalt` deprecatedStatus
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` provisioningType
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` visibility

instance Prelude.NFData ListTypes where
  rnf ListTypes' {..} =
    Prelude.rnf deprecatedStatus
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf provisioningType
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf visibility

instance Data.ToHeaders ListTypes where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListTypes where
  toPath = Prelude.const "/"

instance Data.ToQuery ListTypes where
  toQuery ListTypes' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("ListTypes" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-05-15" :: Prelude.ByteString),
        "DeprecatedStatus" Data.=: deprecatedStatus,
        "Filters" Data.=: filters,
        "MaxResults" Data.=: maxResults,
        "NextToken" Data.=: nextToken,
        "ProvisioningType" Data.=: provisioningType,
        "Type" Data.=: type',
        "Visibility" Data.=: visibility
      ]

-- | /See:/ 'newListTypesResponse' smart constructor.
data ListTypesResponse = ListTypesResponse'
  { -- | If the request doesn\'t return all the remaining results, @NextToken@ is
    -- set to a token. To retrieve the next set of results, call this action
    -- again and assign that token to the request object\'s @NextToken@
    -- parameter. If the request returns all results, @NextToken@ is set to
    -- @null@.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of @TypeSummary@ structures that contain information about the
    -- specified extensions.
    typeSummaries :: Prelude.Maybe [TypeSummary],
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
-- 'nextToken', 'listTypesResponse_nextToken' - If the request doesn\'t return all the remaining results, @NextToken@ is
-- set to a token. To retrieve the next set of results, call this action
-- again and assign that token to the request object\'s @NextToken@
-- parameter. If the request returns all results, @NextToken@ is set to
-- @null@.
--
-- 'typeSummaries', 'listTypesResponse_typeSummaries' - A list of @TypeSummary@ structures that contain information about the
-- specified extensions.
--
-- 'httpStatus', 'listTypesResponse_httpStatus' - The response's http status code.
newListTypesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListTypesResponse
newListTypesResponse pHttpStatus_ =
  ListTypesResponse'
    { nextToken = Prelude.Nothing,
      typeSummaries = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If the request doesn\'t return all the remaining results, @NextToken@ is
-- set to a token. To retrieve the next set of results, call this action
-- again and assign that token to the request object\'s @NextToken@
-- parameter. If the request returns all results, @NextToken@ is set to
-- @null@.
listTypesResponse_nextToken :: Lens.Lens' ListTypesResponse (Prelude.Maybe Prelude.Text)
listTypesResponse_nextToken = Lens.lens (\ListTypesResponse' {nextToken} -> nextToken) (\s@ListTypesResponse' {} a -> s {nextToken = a} :: ListTypesResponse)

-- | A list of @TypeSummary@ structures that contain information about the
-- specified extensions.
listTypesResponse_typeSummaries :: Lens.Lens' ListTypesResponse (Prelude.Maybe [TypeSummary])
listTypesResponse_typeSummaries = Lens.lens (\ListTypesResponse' {typeSummaries} -> typeSummaries) (\s@ListTypesResponse' {} a -> s {typeSummaries = a} :: ListTypesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listTypesResponse_httpStatus :: Lens.Lens' ListTypesResponse Prelude.Int
listTypesResponse_httpStatus = Lens.lens (\ListTypesResponse' {httpStatus} -> httpStatus) (\s@ListTypesResponse' {} a -> s {httpStatus = a} :: ListTypesResponse)

instance Prelude.NFData ListTypesResponse where
  rnf ListTypesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf typeSummaries
      `Prelude.seq` Prelude.rnf httpStatus
