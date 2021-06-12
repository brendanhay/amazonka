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
-- Module      : Network.AWS.CloudDirectory.ListOutgoingTypedLinks
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.CloudDirectory.ListOutgoingTypedLinks
  ( -- * Creating a Request
    ListOutgoingTypedLinks (..),
    newListOutgoingTypedLinks,

    -- * Request Lenses
    listOutgoingTypedLinks_nextToken,
    listOutgoingTypedLinks_filterTypedLink,
    listOutgoingTypedLinks_maxResults,
    listOutgoingTypedLinks_consistencyLevel,
    listOutgoingTypedLinks_filterAttributeRanges,
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

import Network.AWS.CloudDirectory.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListOutgoingTypedLinks' smart constructor.
data ListOutgoingTypedLinks = ListOutgoingTypedLinks'
  { -- | The pagination token.
    nextToken :: Core.Maybe Core.Text,
    -- | Filters are interpreted in the order of the attributes defined on the
    -- typed link facet, not the order they are supplied to any API calls.
    filterTypedLink :: Core.Maybe TypedLinkSchemaAndFacetName,
    -- | The maximum number of results to retrieve.
    maxResults :: Core.Maybe Core.Natural,
    -- | The consistency level to execute the request at.
    consistencyLevel :: Core.Maybe ConsistencyLevel,
    -- | Provides range filters for multiple attributes. When providing ranges to
    -- typed link selection, any inexact ranges must be specified at the end.
    -- Any attributes that do not have a range specified are presumed to match
    -- the entire range.
    filterAttributeRanges :: Core.Maybe [TypedLinkAttributeRange],
    -- | The Amazon Resource Name (ARN) of the directory where you want to list
    -- the typed links.
    directoryArn :: Core.Text,
    -- | A reference that identifies the object whose attributes will be listed.
    objectReference :: ObjectReference
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- 'filterTypedLink', 'listOutgoingTypedLinks_filterTypedLink' - Filters are interpreted in the order of the attributes defined on the
-- typed link facet, not the order they are supplied to any API calls.
--
-- 'maxResults', 'listOutgoingTypedLinks_maxResults' - The maximum number of results to retrieve.
--
-- 'consistencyLevel', 'listOutgoingTypedLinks_consistencyLevel' - The consistency level to execute the request at.
--
-- 'filterAttributeRanges', 'listOutgoingTypedLinks_filterAttributeRanges' - Provides range filters for multiple attributes. When providing ranges to
-- typed link selection, any inexact ranges must be specified at the end.
-- Any attributes that do not have a range specified are presumed to match
-- the entire range.
--
-- 'directoryArn', 'listOutgoingTypedLinks_directoryArn' - The Amazon Resource Name (ARN) of the directory where you want to list
-- the typed links.
--
-- 'objectReference', 'listOutgoingTypedLinks_objectReference' - A reference that identifies the object whose attributes will be listed.
newListOutgoingTypedLinks ::
  -- | 'directoryArn'
  Core.Text ->
  -- | 'objectReference'
  ObjectReference ->
  ListOutgoingTypedLinks
newListOutgoingTypedLinks
  pDirectoryArn_
  pObjectReference_ =
    ListOutgoingTypedLinks'
      { nextToken = Core.Nothing,
        filterTypedLink = Core.Nothing,
        maxResults = Core.Nothing,
        consistencyLevel = Core.Nothing,
        filterAttributeRanges = Core.Nothing,
        directoryArn = pDirectoryArn_,
        objectReference = pObjectReference_
      }

-- | The pagination token.
listOutgoingTypedLinks_nextToken :: Lens.Lens' ListOutgoingTypedLinks (Core.Maybe Core.Text)
listOutgoingTypedLinks_nextToken = Lens.lens (\ListOutgoingTypedLinks' {nextToken} -> nextToken) (\s@ListOutgoingTypedLinks' {} a -> s {nextToken = a} :: ListOutgoingTypedLinks)

-- | Filters are interpreted in the order of the attributes defined on the
-- typed link facet, not the order they are supplied to any API calls.
listOutgoingTypedLinks_filterTypedLink :: Lens.Lens' ListOutgoingTypedLinks (Core.Maybe TypedLinkSchemaAndFacetName)
listOutgoingTypedLinks_filterTypedLink = Lens.lens (\ListOutgoingTypedLinks' {filterTypedLink} -> filterTypedLink) (\s@ListOutgoingTypedLinks' {} a -> s {filterTypedLink = a} :: ListOutgoingTypedLinks)

-- | The maximum number of results to retrieve.
listOutgoingTypedLinks_maxResults :: Lens.Lens' ListOutgoingTypedLinks (Core.Maybe Core.Natural)
listOutgoingTypedLinks_maxResults = Lens.lens (\ListOutgoingTypedLinks' {maxResults} -> maxResults) (\s@ListOutgoingTypedLinks' {} a -> s {maxResults = a} :: ListOutgoingTypedLinks)

-- | The consistency level to execute the request at.
listOutgoingTypedLinks_consistencyLevel :: Lens.Lens' ListOutgoingTypedLinks (Core.Maybe ConsistencyLevel)
listOutgoingTypedLinks_consistencyLevel = Lens.lens (\ListOutgoingTypedLinks' {consistencyLevel} -> consistencyLevel) (\s@ListOutgoingTypedLinks' {} a -> s {consistencyLevel = a} :: ListOutgoingTypedLinks)

-- | Provides range filters for multiple attributes. When providing ranges to
-- typed link selection, any inexact ranges must be specified at the end.
-- Any attributes that do not have a range specified are presumed to match
-- the entire range.
listOutgoingTypedLinks_filterAttributeRanges :: Lens.Lens' ListOutgoingTypedLinks (Core.Maybe [TypedLinkAttributeRange])
listOutgoingTypedLinks_filterAttributeRanges = Lens.lens (\ListOutgoingTypedLinks' {filterAttributeRanges} -> filterAttributeRanges) (\s@ListOutgoingTypedLinks' {} a -> s {filterAttributeRanges = a} :: ListOutgoingTypedLinks) Core.. Lens.mapping Lens._Coerce

-- | The Amazon Resource Name (ARN) of the directory where you want to list
-- the typed links.
listOutgoingTypedLinks_directoryArn :: Lens.Lens' ListOutgoingTypedLinks Core.Text
listOutgoingTypedLinks_directoryArn = Lens.lens (\ListOutgoingTypedLinks' {directoryArn} -> directoryArn) (\s@ListOutgoingTypedLinks' {} a -> s {directoryArn = a} :: ListOutgoingTypedLinks)

-- | A reference that identifies the object whose attributes will be listed.
listOutgoingTypedLinks_objectReference :: Lens.Lens' ListOutgoingTypedLinks ObjectReference
listOutgoingTypedLinks_objectReference = Lens.lens (\ListOutgoingTypedLinks' {objectReference} -> objectReference) (\s@ListOutgoingTypedLinks' {} a -> s {objectReference = a} :: ListOutgoingTypedLinks)

instance Core.AWSPager ListOutgoingTypedLinks where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listOutgoingTypedLinksResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listOutgoingTypedLinksResponse_typedLinkSpecifiers
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listOutgoingTypedLinks_nextToken
          Lens..~ rs
          Lens.^? listOutgoingTypedLinksResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest ListOutgoingTypedLinks where
  type
    AWSResponse ListOutgoingTypedLinks =
      ListOutgoingTypedLinksResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListOutgoingTypedLinksResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> ( x Core..?> "TypedLinkSpecifiers"
                         Core..!@ Core.mempty
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListOutgoingTypedLinks

instance Core.NFData ListOutgoingTypedLinks

instance Core.ToHeaders ListOutgoingTypedLinks where
  toHeaders ListOutgoingTypedLinks' {..} =
    Core.mconcat
      ["x-amz-data-partition" Core.=# directoryArn]

instance Core.ToJSON ListOutgoingTypedLinks where
  toJSON ListOutgoingTypedLinks' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("FilterTypedLink" Core..=) Core.<$> filterTypedLink,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("ConsistencyLevel" Core..=)
              Core.<$> consistencyLevel,
            ("FilterAttributeRanges" Core..=)
              Core.<$> filterAttributeRanges,
            Core.Just
              ("ObjectReference" Core..= objectReference)
          ]
      )

instance Core.ToPath ListOutgoingTypedLinks where
  toPath =
    Core.const
      "/amazonclouddirectory/2017-01-11/typedlink/outgoing"

instance Core.ToQuery ListOutgoingTypedLinks where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListOutgoingTypedLinksResponse' smart constructor.
data ListOutgoingTypedLinksResponse = ListOutgoingTypedLinksResponse'
  { -- | The pagination token.
    nextToken :: Core.Maybe Core.Text,
    -- | Returns a typed link specifier as output.
    typedLinkSpecifiers :: Core.Maybe [TypedLinkSpecifier],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  ListOutgoingTypedLinksResponse
newListOutgoingTypedLinksResponse pHttpStatus_ =
  ListOutgoingTypedLinksResponse'
    { nextToken =
        Core.Nothing,
      typedLinkSpecifiers = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The pagination token.
listOutgoingTypedLinksResponse_nextToken :: Lens.Lens' ListOutgoingTypedLinksResponse (Core.Maybe Core.Text)
listOutgoingTypedLinksResponse_nextToken = Lens.lens (\ListOutgoingTypedLinksResponse' {nextToken} -> nextToken) (\s@ListOutgoingTypedLinksResponse' {} a -> s {nextToken = a} :: ListOutgoingTypedLinksResponse)

-- | Returns a typed link specifier as output.
listOutgoingTypedLinksResponse_typedLinkSpecifiers :: Lens.Lens' ListOutgoingTypedLinksResponse (Core.Maybe [TypedLinkSpecifier])
listOutgoingTypedLinksResponse_typedLinkSpecifiers = Lens.lens (\ListOutgoingTypedLinksResponse' {typedLinkSpecifiers} -> typedLinkSpecifiers) (\s@ListOutgoingTypedLinksResponse' {} a -> s {typedLinkSpecifiers = a} :: ListOutgoingTypedLinksResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listOutgoingTypedLinksResponse_httpStatus :: Lens.Lens' ListOutgoingTypedLinksResponse Core.Int
listOutgoingTypedLinksResponse_httpStatus = Lens.lens (\ListOutgoingTypedLinksResponse' {httpStatus} -> httpStatus) (\s@ListOutgoingTypedLinksResponse' {} a -> s {httpStatus = a} :: ListOutgoingTypedLinksResponse)

instance Core.NFData ListOutgoingTypedLinksResponse
