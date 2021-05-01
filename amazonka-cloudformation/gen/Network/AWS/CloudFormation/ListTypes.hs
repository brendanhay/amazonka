{-# LANGUAGE DeriveDataTypeable #-}
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
module Network.AWS.CloudFormation.ListTypes
  ( -- * Creating a Request
    ListTypes (..),
    newListTypes,

    -- * Request Lenses
    listTypes_nextToken,
    listTypes_maxResults,
    listTypes_deprecatedStatus,
    listTypes_provisioningType,
    listTypes_visibility,
    listTypes_type,

    -- * Destructuring the Response
    ListTypesResponse (..),
    newListTypesResponse,

    -- * Response Lenses
    listTypesResponse_nextToken,
    listTypesResponse_typeSummaries,
    listTypesResponse_httpStatus,
  )
where

import Network.AWS.CloudFormation.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListTypes' smart constructor.
data ListTypes = ListTypes'
  { -- | If the previous paginated request didn\'t return all of the remaining
    -- results, the response object\'s @NextToken@ parameter value is set to a
    -- token. To retrieve the next set of results, call this action again and
    -- assign that token to the request object\'s @NextToken@ parameter. If
    -- there are no remaining results, the previous response object\'s
    -- @NextToken@ parameter is set to @null@.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to be returned with a single call. If the
    -- number of available results exceeds this maximum, the response includes
    -- a @NextToken@ value that you can assign to the @NextToken@ request
    -- parameter to get the next set of results.
    maxResults :: Prelude.Maybe Prelude.Natural,
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
    -- | The provisioning behavior of the type. AWS CloudFormation determines the
    -- provisioning type during registration, based on the types of handlers in
    -- the schema handler package submitted.
    --
    -- Valid values include:
    --
    -- -   @FULLY_MUTABLE@: The extension includes an update handler to process
    --     updates to the extension during stack update operations.
    --
    -- -   @IMMUTABLE@: The extension does not include an update handler, so
    --     the extension cannot be updated and must instead be replaced during
    --     stack update operations.
    --
    -- -   @NON_PROVISIONABLE@: The extension does not include create, read,
    --     and delete handlers, and therefore cannot actually be provisioned.
    provisioningType :: Prelude.Maybe ProvisioningType,
    -- | The scope at which the extension is visible and usable in CloudFormation
    -- operations.
    --
    -- Valid values include:
    --
    -- -   @PRIVATE@: The extension is only visible and usable within the
    --     account in which it is registered. Currently, AWS CloudFormation
    --     marks any extension you create as @PRIVATE@.
    --
    -- -   @PUBLIC@: The extension is publically visible and usable within any
    --     Amazon account.
    --
    -- The default is @PRIVATE@.
    visibility :: Prelude.Maybe Visibility,
    -- | The type of extension.
    type' :: Prelude.Maybe RegistryType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ListTypes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listTypes_nextToken' - If the previous paginated request didn\'t return all of the remaining
-- results, the response object\'s @NextToken@ parameter value is set to a
-- token. To retrieve the next set of results, call this action again and
-- assign that token to the request object\'s @NextToken@ parameter. If
-- there are no remaining results, the previous response object\'s
-- @NextToken@ parameter is set to @null@.
--
-- 'maxResults', 'listTypes_maxResults' - The maximum number of results to be returned with a single call. If the
-- number of available results exceeds this maximum, the response includes
-- a @NextToken@ value that you can assign to the @NextToken@ request
-- parameter to get the next set of results.
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
-- 'provisioningType', 'listTypes_provisioningType' - The provisioning behavior of the type. AWS CloudFormation determines the
-- provisioning type during registration, based on the types of handlers in
-- the schema handler package submitted.
--
-- Valid values include:
--
-- -   @FULLY_MUTABLE@: The extension includes an update handler to process
--     updates to the extension during stack update operations.
--
-- -   @IMMUTABLE@: The extension does not include an update handler, so
--     the extension cannot be updated and must instead be replaced during
--     stack update operations.
--
-- -   @NON_PROVISIONABLE@: The extension does not include create, read,
--     and delete handlers, and therefore cannot actually be provisioned.
--
-- 'visibility', 'listTypes_visibility' - The scope at which the extension is visible and usable in CloudFormation
-- operations.
--
-- Valid values include:
--
-- -   @PRIVATE@: The extension is only visible and usable within the
--     account in which it is registered. Currently, AWS CloudFormation
--     marks any extension you create as @PRIVATE@.
--
-- -   @PUBLIC@: The extension is publically visible and usable within any
--     Amazon account.
--
-- The default is @PRIVATE@.
--
-- 'type'', 'listTypes_type' - The type of extension.
newListTypes ::
  ListTypes
newListTypes =
  ListTypes'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      deprecatedStatus = Prelude.Nothing,
      provisioningType = Prelude.Nothing,
      visibility = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | If the previous paginated request didn\'t return all of the remaining
-- results, the response object\'s @NextToken@ parameter value is set to a
-- token. To retrieve the next set of results, call this action again and
-- assign that token to the request object\'s @NextToken@ parameter. If
-- there are no remaining results, the previous response object\'s
-- @NextToken@ parameter is set to @null@.
listTypes_nextToken :: Lens.Lens' ListTypes (Prelude.Maybe Prelude.Text)
listTypes_nextToken = Lens.lens (\ListTypes' {nextToken} -> nextToken) (\s@ListTypes' {} a -> s {nextToken = a} :: ListTypes)

-- | The maximum number of results to be returned with a single call. If the
-- number of available results exceeds this maximum, the response includes
-- a @NextToken@ value that you can assign to the @NextToken@ request
-- parameter to get the next set of results.
listTypes_maxResults :: Lens.Lens' ListTypes (Prelude.Maybe Prelude.Natural)
listTypes_maxResults = Lens.lens (\ListTypes' {maxResults} -> maxResults) (\s@ListTypes' {} a -> s {maxResults = a} :: ListTypes)

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

-- | The provisioning behavior of the type. AWS CloudFormation determines the
-- provisioning type during registration, based on the types of handlers in
-- the schema handler package submitted.
--
-- Valid values include:
--
-- -   @FULLY_MUTABLE@: The extension includes an update handler to process
--     updates to the extension during stack update operations.
--
-- -   @IMMUTABLE@: The extension does not include an update handler, so
--     the extension cannot be updated and must instead be replaced during
--     stack update operations.
--
-- -   @NON_PROVISIONABLE@: The extension does not include create, read,
--     and delete handlers, and therefore cannot actually be provisioned.
listTypes_provisioningType :: Lens.Lens' ListTypes (Prelude.Maybe ProvisioningType)
listTypes_provisioningType = Lens.lens (\ListTypes' {provisioningType} -> provisioningType) (\s@ListTypes' {} a -> s {provisioningType = a} :: ListTypes)

-- | The scope at which the extension is visible and usable in CloudFormation
-- operations.
--
-- Valid values include:
--
-- -   @PRIVATE@: The extension is only visible and usable within the
--     account in which it is registered. Currently, AWS CloudFormation
--     marks any extension you create as @PRIVATE@.
--
-- -   @PUBLIC@: The extension is publically visible and usable within any
--     Amazon account.
--
-- The default is @PRIVATE@.
listTypes_visibility :: Lens.Lens' ListTypes (Prelude.Maybe Visibility)
listTypes_visibility = Lens.lens (\ListTypes' {visibility} -> visibility) (\s@ListTypes' {} a -> s {visibility = a} :: ListTypes)

-- | The type of extension.
listTypes_type :: Lens.Lens' ListTypes (Prelude.Maybe RegistryType)
listTypes_type = Lens.lens (\ListTypes' {type'} -> type') (\s@ListTypes' {} a -> s {type' = a} :: ListTypes)

instance Prelude.AWSRequest ListTypes where
  type Rs ListTypes = ListTypesResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "ListTypesResult"
      ( \s h x ->
          ListTypesResponse'
            Prelude.<$> (x Prelude..@? "NextToken")
            Prelude.<*> ( x Prelude..@? "TypeSummaries"
                            Prelude..!@ Prelude.mempty
                            Prelude.>>= Prelude.may (Prelude.parseXMLList "member")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListTypes

instance Prelude.NFData ListTypes

instance Prelude.ToHeaders ListTypes where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath ListTypes where
  toPath = Prelude.const "/"

instance Prelude.ToQuery ListTypes where
  toQuery ListTypes' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("ListTypes" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2010-05-15" :: Prelude.ByteString),
        "NextToken" Prelude.=: nextToken,
        "MaxResults" Prelude.=: maxResults,
        "DeprecatedStatus" Prelude.=: deprecatedStatus,
        "ProvisioningType" Prelude.=: provisioningType,
        "Visibility" Prelude.=: visibility,
        "Type" Prelude.=: type'
      ]

-- | /See:/ 'newListTypesResponse' smart constructor.
data ListTypesResponse = ListTypesResponse'
  { -- | If the request doesn\'t return all of the remaining results, @NextToken@
    -- is set to a token. To retrieve the next set of results, call this action
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ListTypesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listTypesResponse_nextToken' - If the request doesn\'t return all of the remaining results, @NextToken@
-- is set to a token. To retrieve the next set of results, call this action
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

-- | If the request doesn\'t return all of the remaining results, @NextToken@
-- is set to a token. To retrieve the next set of results, call this action
-- again and assign that token to the request object\'s @NextToken@
-- parameter. If the request returns all results, @NextToken@ is set to
-- @null@.
listTypesResponse_nextToken :: Lens.Lens' ListTypesResponse (Prelude.Maybe Prelude.Text)
listTypesResponse_nextToken = Lens.lens (\ListTypesResponse' {nextToken} -> nextToken) (\s@ListTypesResponse' {} a -> s {nextToken = a} :: ListTypesResponse)

-- | A list of @TypeSummary@ structures that contain information about the
-- specified extensions.
listTypesResponse_typeSummaries :: Lens.Lens' ListTypesResponse (Prelude.Maybe [TypeSummary])
listTypesResponse_typeSummaries = Lens.lens (\ListTypesResponse' {typeSummaries} -> typeSummaries) (\s@ListTypesResponse' {} a -> s {typeSummaries = a} :: ListTypesResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
listTypesResponse_httpStatus :: Lens.Lens' ListTypesResponse Prelude.Int
listTypesResponse_httpStatus = Lens.lens (\ListTypesResponse' {httpStatus} -> httpStatus) (\s@ListTypesResponse' {} a -> s {httpStatus = a} :: ListTypesResponse)

instance Prelude.NFData ListTypesResponse
