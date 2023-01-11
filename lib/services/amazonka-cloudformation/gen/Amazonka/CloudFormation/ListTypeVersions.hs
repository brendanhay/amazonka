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
-- Module      : Amazonka.CloudFormation.ListTypeVersions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns summary information about the versions of an extension.
module Amazonka.CloudFormation.ListTypeVersions
  ( -- * Creating a Request
    ListTypeVersions (..),
    newListTypeVersions,

    -- * Request Lenses
    listTypeVersions_arn,
    listTypeVersions_deprecatedStatus,
    listTypeVersions_maxResults,
    listTypeVersions_nextToken,
    listTypeVersions_publisherId,
    listTypeVersions_type,
    listTypeVersions_typeName,

    -- * Destructuring the Response
    ListTypeVersionsResponse (..),
    newListTypeVersionsResponse,

    -- * Response Lenses
    listTypeVersionsResponse_nextToken,
    listTypeVersionsResponse_typeVersionSummaries,
    listTypeVersionsResponse_httpStatus,
  )
where

import Amazonka.CloudFormation.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListTypeVersions' smart constructor.
data ListTypeVersions = ListTypeVersions'
  { -- | The Amazon Resource Name (ARN) of the extension for which you want
    -- version summary information.
    --
    -- Conditional: You must specify either @TypeName@ and @Type@, or @Arn@.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The deprecation status of the extension versions that you want to get
    -- summary information about.
    --
    -- Valid values include:
    --
    -- -   @LIVE@: The extension version is registered and can be used in
    --     CloudFormation operations, dependent on its provisioning behavior
    --     and visibility scope.
    --
    -- -   @DEPRECATED@: The extension version has been deregistered and can no
    --     longer be used in CloudFormation operations.
    --
    -- The default is @LIVE@.
    deprecatedStatus :: Prelude.Maybe DeprecatedStatus,
    -- | The maximum number of results to be returned with a single call. If the
    -- number of available results exceeds this maximum, the response includes
    -- a @NextToken@ value that you can assign to the @NextToken@ request
    -- parameter to get the next set of results.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | If the previous paginated request didn\'t return all of the remaining
    -- results, the response object\'s @NextToken@ parameter value is set to a
    -- token. To retrieve the next set of results, call this action again and
    -- assign that token to the request object\'s @NextToken@ parameter. If
    -- there are no remaining results, the previous response object\'s
    -- @NextToken@ parameter is set to @null@.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The publisher ID of the extension publisher.
    --
    -- Extensions published by Amazon aren\'t assigned a publisher ID.
    publisherId :: Prelude.Maybe Prelude.Text,
    -- | The kind of the extension.
    --
    -- Conditional: You must specify either @TypeName@ and @Type@, or @Arn@.
    type' :: Prelude.Maybe RegistryType,
    -- | The name of the extension for which you want version summary
    -- information.
    --
    -- Conditional: You must specify either @TypeName@ and @Type@, or @Arn@.
    typeName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTypeVersions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'listTypeVersions_arn' - The Amazon Resource Name (ARN) of the extension for which you want
-- version summary information.
--
-- Conditional: You must specify either @TypeName@ and @Type@, or @Arn@.
--
-- 'deprecatedStatus', 'listTypeVersions_deprecatedStatus' - The deprecation status of the extension versions that you want to get
-- summary information about.
--
-- Valid values include:
--
-- -   @LIVE@: The extension version is registered and can be used in
--     CloudFormation operations, dependent on its provisioning behavior
--     and visibility scope.
--
-- -   @DEPRECATED@: The extension version has been deregistered and can no
--     longer be used in CloudFormation operations.
--
-- The default is @LIVE@.
--
-- 'maxResults', 'listTypeVersions_maxResults' - The maximum number of results to be returned with a single call. If the
-- number of available results exceeds this maximum, the response includes
-- a @NextToken@ value that you can assign to the @NextToken@ request
-- parameter to get the next set of results.
--
-- 'nextToken', 'listTypeVersions_nextToken' - If the previous paginated request didn\'t return all of the remaining
-- results, the response object\'s @NextToken@ parameter value is set to a
-- token. To retrieve the next set of results, call this action again and
-- assign that token to the request object\'s @NextToken@ parameter. If
-- there are no remaining results, the previous response object\'s
-- @NextToken@ parameter is set to @null@.
--
-- 'publisherId', 'listTypeVersions_publisherId' - The publisher ID of the extension publisher.
--
-- Extensions published by Amazon aren\'t assigned a publisher ID.
--
-- 'type'', 'listTypeVersions_type' - The kind of the extension.
--
-- Conditional: You must specify either @TypeName@ and @Type@, or @Arn@.
--
-- 'typeName', 'listTypeVersions_typeName' - The name of the extension for which you want version summary
-- information.
--
-- Conditional: You must specify either @TypeName@ and @Type@, or @Arn@.
newListTypeVersions ::
  ListTypeVersions
newListTypeVersions =
  ListTypeVersions'
    { arn = Prelude.Nothing,
      deprecatedStatus = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      publisherId = Prelude.Nothing,
      type' = Prelude.Nothing,
      typeName = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the extension for which you want
-- version summary information.
--
-- Conditional: You must specify either @TypeName@ and @Type@, or @Arn@.
listTypeVersions_arn :: Lens.Lens' ListTypeVersions (Prelude.Maybe Prelude.Text)
listTypeVersions_arn = Lens.lens (\ListTypeVersions' {arn} -> arn) (\s@ListTypeVersions' {} a -> s {arn = a} :: ListTypeVersions)

-- | The deprecation status of the extension versions that you want to get
-- summary information about.
--
-- Valid values include:
--
-- -   @LIVE@: The extension version is registered and can be used in
--     CloudFormation operations, dependent on its provisioning behavior
--     and visibility scope.
--
-- -   @DEPRECATED@: The extension version has been deregistered and can no
--     longer be used in CloudFormation operations.
--
-- The default is @LIVE@.
listTypeVersions_deprecatedStatus :: Lens.Lens' ListTypeVersions (Prelude.Maybe DeprecatedStatus)
listTypeVersions_deprecatedStatus = Lens.lens (\ListTypeVersions' {deprecatedStatus} -> deprecatedStatus) (\s@ListTypeVersions' {} a -> s {deprecatedStatus = a} :: ListTypeVersions)

-- | The maximum number of results to be returned with a single call. If the
-- number of available results exceeds this maximum, the response includes
-- a @NextToken@ value that you can assign to the @NextToken@ request
-- parameter to get the next set of results.
listTypeVersions_maxResults :: Lens.Lens' ListTypeVersions (Prelude.Maybe Prelude.Natural)
listTypeVersions_maxResults = Lens.lens (\ListTypeVersions' {maxResults} -> maxResults) (\s@ListTypeVersions' {} a -> s {maxResults = a} :: ListTypeVersions)

-- | If the previous paginated request didn\'t return all of the remaining
-- results, the response object\'s @NextToken@ parameter value is set to a
-- token. To retrieve the next set of results, call this action again and
-- assign that token to the request object\'s @NextToken@ parameter. If
-- there are no remaining results, the previous response object\'s
-- @NextToken@ parameter is set to @null@.
listTypeVersions_nextToken :: Lens.Lens' ListTypeVersions (Prelude.Maybe Prelude.Text)
listTypeVersions_nextToken = Lens.lens (\ListTypeVersions' {nextToken} -> nextToken) (\s@ListTypeVersions' {} a -> s {nextToken = a} :: ListTypeVersions)

-- | The publisher ID of the extension publisher.
--
-- Extensions published by Amazon aren\'t assigned a publisher ID.
listTypeVersions_publisherId :: Lens.Lens' ListTypeVersions (Prelude.Maybe Prelude.Text)
listTypeVersions_publisherId = Lens.lens (\ListTypeVersions' {publisherId} -> publisherId) (\s@ListTypeVersions' {} a -> s {publisherId = a} :: ListTypeVersions)

-- | The kind of the extension.
--
-- Conditional: You must specify either @TypeName@ and @Type@, or @Arn@.
listTypeVersions_type :: Lens.Lens' ListTypeVersions (Prelude.Maybe RegistryType)
listTypeVersions_type = Lens.lens (\ListTypeVersions' {type'} -> type') (\s@ListTypeVersions' {} a -> s {type' = a} :: ListTypeVersions)

-- | The name of the extension for which you want version summary
-- information.
--
-- Conditional: You must specify either @TypeName@ and @Type@, or @Arn@.
listTypeVersions_typeName :: Lens.Lens' ListTypeVersions (Prelude.Maybe Prelude.Text)
listTypeVersions_typeName = Lens.lens (\ListTypeVersions' {typeName} -> typeName) (\s@ListTypeVersions' {} a -> s {typeName = a} :: ListTypeVersions)

instance Core.AWSRequest ListTypeVersions where
  type
    AWSResponse ListTypeVersions =
      ListTypeVersionsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "ListTypeVersionsResult"
      ( \s h x ->
          ListTypeVersionsResponse'
            Prelude.<$> (x Data..@? "NextToken")
            Prelude.<*> ( x Data..@? "TypeVersionSummaries"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "member")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListTypeVersions where
  hashWithSalt _salt ListTypeVersions' {..} =
    _salt `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` deprecatedStatus
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` publisherId
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` typeName

instance Prelude.NFData ListTypeVersions where
  rnf ListTypeVersions' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf deprecatedStatus
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf publisherId
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf typeName

instance Data.ToHeaders ListTypeVersions where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListTypeVersions where
  toPath = Prelude.const "/"

instance Data.ToQuery ListTypeVersions where
  toQuery ListTypeVersions' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("ListTypeVersions" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-05-15" :: Prelude.ByteString),
        "Arn" Data.=: arn,
        "DeprecatedStatus" Data.=: deprecatedStatus,
        "MaxResults" Data.=: maxResults,
        "NextToken" Data.=: nextToken,
        "PublisherId" Data.=: publisherId,
        "Type" Data.=: type',
        "TypeName" Data.=: typeName
      ]

-- | /See:/ 'newListTypeVersionsResponse' smart constructor.
data ListTypeVersionsResponse = ListTypeVersionsResponse'
  { -- | If the request doesn\'t return all of the remaining results, @NextToken@
    -- is set to a token. To retrieve the next set of results, call this action
    -- again and assign that token to the request object\'s @NextToken@
    -- parameter. If the request returns all results, @NextToken@ is set to
    -- @null@.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of @TypeVersionSummary@ structures that contain information about
    -- the specified extension\'s versions.
    typeVersionSummaries :: Prelude.Maybe [TypeVersionSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTypeVersionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listTypeVersionsResponse_nextToken' - If the request doesn\'t return all of the remaining results, @NextToken@
-- is set to a token. To retrieve the next set of results, call this action
-- again and assign that token to the request object\'s @NextToken@
-- parameter. If the request returns all results, @NextToken@ is set to
-- @null@.
--
-- 'typeVersionSummaries', 'listTypeVersionsResponse_typeVersionSummaries' - A list of @TypeVersionSummary@ structures that contain information about
-- the specified extension\'s versions.
--
-- 'httpStatus', 'listTypeVersionsResponse_httpStatus' - The response's http status code.
newListTypeVersionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListTypeVersionsResponse
newListTypeVersionsResponse pHttpStatus_ =
  ListTypeVersionsResponse'
    { nextToken =
        Prelude.Nothing,
      typeVersionSummaries = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If the request doesn\'t return all of the remaining results, @NextToken@
-- is set to a token. To retrieve the next set of results, call this action
-- again and assign that token to the request object\'s @NextToken@
-- parameter. If the request returns all results, @NextToken@ is set to
-- @null@.
listTypeVersionsResponse_nextToken :: Lens.Lens' ListTypeVersionsResponse (Prelude.Maybe Prelude.Text)
listTypeVersionsResponse_nextToken = Lens.lens (\ListTypeVersionsResponse' {nextToken} -> nextToken) (\s@ListTypeVersionsResponse' {} a -> s {nextToken = a} :: ListTypeVersionsResponse)

-- | A list of @TypeVersionSummary@ structures that contain information about
-- the specified extension\'s versions.
listTypeVersionsResponse_typeVersionSummaries :: Lens.Lens' ListTypeVersionsResponse (Prelude.Maybe [TypeVersionSummary])
listTypeVersionsResponse_typeVersionSummaries = Lens.lens (\ListTypeVersionsResponse' {typeVersionSummaries} -> typeVersionSummaries) (\s@ListTypeVersionsResponse' {} a -> s {typeVersionSummaries = a} :: ListTypeVersionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listTypeVersionsResponse_httpStatus :: Lens.Lens' ListTypeVersionsResponse Prelude.Int
listTypeVersionsResponse_httpStatus = Lens.lens (\ListTypeVersionsResponse' {httpStatus} -> httpStatus) (\s@ListTypeVersionsResponse' {} a -> s {httpStatus = a} :: ListTypeVersionsResponse)

instance Prelude.NFData ListTypeVersionsResponse where
  rnf ListTypeVersionsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf typeVersionSummaries
      `Prelude.seq` Prelude.rnf httpStatus
