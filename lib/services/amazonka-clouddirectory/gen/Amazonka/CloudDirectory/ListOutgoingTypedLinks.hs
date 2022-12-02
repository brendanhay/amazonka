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
-- Module      : Amazonka.CloudDirectory.ListOutgoingTypedLinks
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a paginated list of all the outgoing TypedLinkSpecifier
-- information for an object. It also supports filtering by typed link
-- facet and identity attributes. For more information, see
-- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links>.
--
-- This operation returns paginated results.
module Amazonka.CloudDirectory.ListOutgoingTypedLinks
  ( -- * Creating a Request
    ListOutgoingTypedLinks (..),
    newListOutgoingTypedLinks,

    -- * Request Lenses
    listOutgoingTypedLinks_nextToken,
    listOutgoingTypedLinks_consistencyLevel,
    listOutgoingTypedLinks_maxResults,
    listOutgoingTypedLinks_filterAttributeRanges,
    listOutgoingTypedLinks_filterTypedLink,
    listOutgoingTypedLinks_directoryArn,
    listOutgoingTypedLinks_objectReference,

    -- * Destructuring the Response
    ListOutgoingTypedLinksResponse (..),
    newListOutgoingTypedLinksResponse,

    -- * Response Lenses
    listOutgoingTypedLinksResponse_nextToken,
    listOutgoingTypedLinksResponse_typedLinkSpecifiers,
    listOutgoingTypedLinksResponse_httpStatus,
  )
where

import Amazonka.CloudDirectory.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListOutgoingTypedLinks' smart constructor.
data ListOutgoingTypedLinks = ListOutgoingTypedLinks'
  { -- | The pagination token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The consistency level to execute the request at.
    consistencyLevel :: Prelude.Maybe ConsistencyLevel,
    -- | The maximum number of results to retrieve.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Provides range filters for multiple attributes. When providing ranges to
    -- typed link selection, any inexact ranges must be specified at the end.
    -- Any attributes that do not have a range specified are presumed to match
    -- the entire range.
    filterAttributeRanges :: Prelude.Maybe [TypedLinkAttributeRange],
    -- | Filters are interpreted in the order of the attributes defined on the
    -- typed link facet, not the order they are supplied to any API calls.
    filterTypedLink :: Prelude.Maybe TypedLinkSchemaAndFacetName,
    -- | The Amazon Resource Name (ARN) of the directory where you want to list
    -- the typed links.
    directoryArn :: Prelude.Text,
    -- | A reference that identifies the object whose attributes will be listed.
    objectReference :: ObjectReference
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListOutgoingTypedLinks' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listOutgoingTypedLinks_nextToken' - The pagination token.
--
-- 'consistencyLevel', 'listOutgoingTypedLinks_consistencyLevel' - The consistency level to execute the request at.
--
-- 'maxResults', 'listOutgoingTypedLinks_maxResults' - The maximum number of results to retrieve.
--
-- 'filterAttributeRanges', 'listOutgoingTypedLinks_filterAttributeRanges' - Provides range filters for multiple attributes. When providing ranges to
-- typed link selection, any inexact ranges must be specified at the end.
-- Any attributes that do not have a range specified are presumed to match
-- the entire range.
--
-- 'filterTypedLink', 'listOutgoingTypedLinks_filterTypedLink' - Filters are interpreted in the order of the attributes defined on the
-- typed link facet, not the order they are supplied to any API calls.
--
-- 'directoryArn', 'listOutgoingTypedLinks_directoryArn' - The Amazon Resource Name (ARN) of the directory where you want to list
-- the typed links.
--
-- 'objectReference', 'listOutgoingTypedLinks_objectReference' - A reference that identifies the object whose attributes will be listed.
newListOutgoingTypedLinks ::
  -- | 'directoryArn'
  Prelude.Text ->
  -- | 'objectReference'
  ObjectReference ->
  ListOutgoingTypedLinks
newListOutgoingTypedLinks
  pDirectoryArn_
  pObjectReference_ =
    ListOutgoingTypedLinks'
      { nextToken =
          Prelude.Nothing,
        consistencyLevel = Prelude.Nothing,
        maxResults = Prelude.Nothing,
        filterAttributeRanges = Prelude.Nothing,
        filterTypedLink = Prelude.Nothing,
        directoryArn = pDirectoryArn_,
        objectReference = pObjectReference_
      }

-- | The pagination token.
listOutgoingTypedLinks_nextToken :: Lens.Lens' ListOutgoingTypedLinks (Prelude.Maybe Prelude.Text)
listOutgoingTypedLinks_nextToken = Lens.lens (\ListOutgoingTypedLinks' {nextToken} -> nextToken) (\s@ListOutgoingTypedLinks' {} a -> s {nextToken = a} :: ListOutgoingTypedLinks)

-- | The consistency level to execute the request at.
listOutgoingTypedLinks_consistencyLevel :: Lens.Lens' ListOutgoingTypedLinks (Prelude.Maybe ConsistencyLevel)
listOutgoingTypedLinks_consistencyLevel = Lens.lens (\ListOutgoingTypedLinks' {consistencyLevel} -> consistencyLevel) (\s@ListOutgoingTypedLinks' {} a -> s {consistencyLevel = a} :: ListOutgoingTypedLinks)

-- | The maximum number of results to retrieve.
listOutgoingTypedLinks_maxResults :: Lens.Lens' ListOutgoingTypedLinks (Prelude.Maybe Prelude.Natural)
listOutgoingTypedLinks_maxResults = Lens.lens (\ListOutgoingTypedLinks' {maxResults} -> maxResults) (\s@ListOutgoingTypedLinks' {} a -> s {maxResults = a} :: ListOutgoingTypedLinks)

-- | Provides range filters for multiple attributes. When providing ranges to
-- typed link selection, any inexact ranges must be specified at the end.
-- Any attributes that do not have a range specified are presumed to match
-- the entire range.
listOutgoingTypedLinks_filterAttributeRanges :: Lens.Lens' ListOutgoingTypedLinks (Prelude.Maybe [TypedLinkAttributeRange])
listOutgoingTypedLinks_filterAttributeRanges = Lens.lens (\ListOutgoingTypedLinks' {filterAttributeRanges} -> filterAttributeRanges) (\s@ListOutgoingTypedLinks' {} a -> s {filterAttributeRanges = a} :: ListOutgoingTypedLinks) Prelude.. Lens.mapping Lens.coerced

-- | Filters are interpreted in the order of the attributes defined on the
-- typed link facet, not the order they are supplied to any API calls.
listOutgoingTypedLinks_filterTypedLink :: Lens.Lens' ListOutgoingTypedLinks (Prelude.Maybe TypedLinkSchemaAndFacetName)
listOutgoingTypedLinks_filterTypedLink = Lens.lens (\ListOutgoingTypedLinks' {filterTypedLink} -> filterTypedLink) (\s@ListOutgoingTypedLinks' {} a -> s {filterTypedLink = a} :: ListOutgoingTypedLinks)

-- | The Amazon Resource Name (ARN) of the directory where you want to list
-- the typed links.
listOutgoingTypedLinks_directoryArn :: Lens.Lens' ListOutgoingTypedLinks Prelude.Text
listOutgoingTypedLinks_directoryArn = Lens.lens (\ListOutgoingTypedLinks' {directoryArn} -> directoryArn) (\s@ListOutgoingTypedLinks' {} a -> s {directoryArn = a} :: ListOutgoingTypedLinks)

-- | A reference that identifies the object whose attributes will be listed.
listOutgoingTypedLinks_objectReference :: Lens.Lens' ListOutgoingTypedLinks ObjectReference
listOutgoingTypedLinks_objectReference = Lens.lens (\ListOutgoingTypedLinks' {objectReference} -> objectReference) (\s@ListOutgoingTypedLinks' {} a -> s {objectReference = a} :: ListOutgoingTypedLinks)

instance Core.AWSPager ListOutgoingTypedLinks where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listOutgoingTypedLinksResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listOutgoingTypedLinksResponse_typedLinkSpecifiers
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listOutgoingTypedLinks_nextToken
          Lens..~ rs
          Lens.^? listOutgoingTypedLinksResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListOutgoingTypedLinks where
  type
    AWSResponse ListOutgoingTypedLinks =
      ListOutgoingTypedLinksResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListOutgoingTypedLinksResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> ( x Data..?> "TypedLinkSpecifiers"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListOutgoingTypedLinks where
  hashWithSalt _salt ListOutgoingTypedLinks' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` consistencyLevel
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` filterAttributeRanges
      `Prelude.hashWithSalt` filterTypedLink
      `Prelude.hashWithSalt` directoryArn
      `Prelude.hashWithSalt` objectReference

instance Prelude.NFData ListOutgoingTypedLinks where
  rnf ListOutgoingTypedLinks' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf consistencyLevel
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf filterAttributeRanges
      `Prelude.seq` Prelude.rnf filterTypedLink
      `Prelude.seq` Prelude.rnf directoryArn
      `Prelude.seq` Prelude.rnf objectReference

instance Data.ToHeaders ListOutgoingTypedLinks where
  toHeaders ListOutgoingTypedLinks' {..} =
    Prelude.mconcat
      ["x-amz-data-partition" Data.=# directoryArn]

instance Data.ToJSON ListOutgoingTypedLinks where
  toJSON ListOutgoingTypedLinks' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NextToken" Data..=) Prelude.<$> nextToken,
            ("ConsistencyLevel" Data..=)
              Prelude.<$> consistencyLevel,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("FilterAttributeRanges" Data..=)
              Prelude.<$> filterAttributeRanges,
            ("FilterTypedLink" Data..=)
              Prelude.<$> filterTypedLink,
            Prelude.Just
              ("ObjectReference" Data..= objectReference)
          ]
      )

instance Data.ToPath ListOutgoingTypedLinks where
  toPath =
    Prelude.const
      "/amazonclouddirectory/2017-01-11/typedlink/outgoing"

instance Data.ToQuery ListOutgoingTypedLinks where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListOutgoingTypedLinksResponse' smart constructor.
data ListOutgoingTypedLinksResponse = ListOutgoingTypedLinksResponse'
  { -- | The pagination token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Returns a typed link specifier as output.
    typedLinkSpecifiers :: Prelude.Maybe [TypedLinkSpecifier],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListOutgoingTypedLinksResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listOutgoingTypedLinksResponse_nextToken' - The pagination token.
--
-- 'typedLinkSpecifiers', 'listOutgoingTypedLinksResponse_typedLinkSpecifiers' - Returns a typed link specifier as output.
--
-- 'httpStatus', 'listOutgoingTypedLinksResponse_httpStatus' - The response's http status code.
newListOutgoingTypedLinksResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListOutgoingTypedLinksResponse
newListOutgoingTypedLinksResponse pHttpStatus_ =
  ListOutgoingTypedLinksResponse'
    { nextToken =
        Prelude.Nothing,
      typedLinkSpecifiers = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The pagination token.
listOutgoingTypedLinksResponse_nextToken :: Lens.Lens' ListOutgoingTypedLinksResponse (Prelude.Maybe Prelude.Text)
listOutgoingTypedLinksResponse_nextToken = Lens.lens (\ListOutgoingTypedLinksResponse' {nextToken} -> nextToken) (\s@ListOutgoingTypedLinksResponse' {} a -> s {nextToken = a} :: ListOutgoingTypedLinksResponse)

-- | Returns a typed link specifier as output.
listOutgoingTypedLinksResponse_typedLinkSpecifiers :: Lens.Lens' ListOutgoingTypedLinksResponse (Prelude.Maybe [TypedLinkSpecifier])
listOutgoingTypedLinksResponse_typedLinkSpecifiers = Lens.lens (\ListOutgoingTypedLinksResponse' {typedLinkSpecifiers} -> typedLinkSpecifiers) (\s@ListOutgoingTypedLinksResponse' {} a -> s {typedLinkSpecifiers = a} :: ListOutgoingTypedLinksResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listOutgoingTypedLinksResponse_httpStatus :: Lens.Lens' ListOutgoingTypedLinksResponse Prelude.Int
listOutgoingTypedLinksResponse_httpStatus = Lens.lens (\ListOutgoingTypedLinksResponse' {httpStatus} -> httpStatus) (\s@ListOutgoingTypedLinksResponse' {} a -> s {httpStatus = a} :: ListOutgoingTypedLinksResponse)

instance
  Prelude.NFData
    ListOutgoingTypedLinksResponse
  where
  rnf ListOutgoingTypedLinksResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf typedLinkSpecifiers
      `Prelude.seq` Prelude.rnf httpStatus
