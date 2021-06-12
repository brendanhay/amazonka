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
-- Module      : Network.AWS.CloudDirectory.ListIncomingTypedLinks
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a paginated list of all the incoming TypedLinkSpecifier
-- information for an object. It also supports filtering by typed link
-- facet and identity attributes. For more information, see
-- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links>.
--
-- This operation returns paginated results.
module Network.AWS.CloudDirectory.ListIncomingTypedLinks
  ( -- * Creating a Request
    ListIncomingTypedLinks (..),
    newListIncomingTypedLinks,

    -- * Request Lenses
    listIncomingTypedLinks_nextToken,
    listIncomingTypedLinks_filterTypedLink,
    listIncomingTypedLinks_maxResults,
    listIncomingTypedLinks_consistencyLevel,
    listIncomingTypedLinks_filterAttributeRanges,
    listIncomingTypedLinks_directoryArn,
    listIncomingTypedLinks_objectReference,

    -- * Destructuring the Response
    ListIncomingTypedLinksResponse (..),
    newListIncomingTypedLinksResponse,

    -- * Response Lenses
    listIncomingTypedLinksResponse_linkSpecifiers,
    listIncomingTypedLinksResponse_nextToken,
    listIncomingTypedLinksResponse_httpStatus,
  )
where

import Network.AWS.CloudDirectory.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListIncomingTypedLinks' smart constructor.
data ListIncomingTypedLinks = ListIncomingTypedLinks'
  { -- | The pagination token.
    nextToken :: Core.Maybe Core.Text,
    -- | Filters are interpreted in the order of the attributes on the typed link
    -- facet, not the order in which they are supplied to any API calls.
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
    -- | Reference that identifies the object whose attributes will be listed.
    objectReference :: ObjectReference
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListIncomingTypedLinks' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listIncomingTypedLinks_nextToken' - The pagination token.
--
-- 'filterTypedLink', 'listIncomingTypedLinks_filterTypedLink' - Filters are interpreted in the order of the attributes on the typed link
-- facet, not the order in which they are supplied to any API calls.
--
-- 'maxResults', 'listIncomingTypedLinks_maxResults' - The maximum number of results to retrieve.
--
-- 'consistencyLevel', 'listIncomingTypedLinks_consistencyLevel' - The consistency level to execute the request at.
--
-- 'filterAttributeRanges', 'listIncomingTypedLinks_filterAttributeRanges' - Provides range filters for multiple attributes. When providing ranges to
-- typed link selection, any inexact ranges must be specified at the end.
-- Any attributes that do not have a range specified are presumed to match
-- the entire range.
--
-- 'directoryArn', 'listIncomingTypedLinks_directoryArn' - The Amazon Resource Name (ARN) of the directory where you want to list
-- the typed links.
--
-- 'objectReference', 'listIncomingTypedLinks_objectReference' - Reference that identifies the object whose attributes will be listed.
newListIncomingTypedLinks ::
  -- | 'directoryArn'
  Core.Text ->
  -- | 'objectReference'
  ObjectReference ->
  ListIncomingTypedLinks
newListIncomingTypedLinks
  pDirectoryArn_
  pObjectReference_ =
    ListIncomingTypedLinks'
      { nextToken = Core.Nothing,
        filterTypedLink = Core.Nothing,
        maxResults = Core.Nothing,
        consistencyLevel = Core.Nothing,
        filterAttributeRanges = Core.Nothing,
        directoryArn = pDirectoryArn_,
        objectReference = pObjectReference_
      }

-- | The pagination token.
listIncomingTypedLinks_nextToken :: Lens.Lens' ListIncomingTypedLinks (Core.Maybe Core.Text)
listIncomingTypedLinks_nextToken = Lens.lens (\ListIncomingTypedLinks' {nextToken} -> nextToken) (\s@ListIncomingTypedLinks' {} a -> s {nextToken = a} :: ListIncomingTypedLinks)

-- | Filters are interpreted in the order of the attributes on the typed link
-- facet, not the order in which they are supplied to any API calls.
listIncomingTypedLinks_filterTypedLink :: Lens.Lens' ListIncomingTypedLinks (Core.Maybe TypedLinkSchemaAndFacetName)
listIncomingTypedLinks_filterTypedLink = Lens.lens (\ListIncomingTypedLinks' {filterTypedLink} -> filterTypedLink) (\s@ListIncomingTypedLinks' {} a -> s {filterTypedLink = a} :: ListIncomingTypedLinks)

-- | The maximum number of results to retrieve.
listIncomingTypedLinks_maxResults :: Lens.Lens' ListIncomingTypedLinks (Core.Maybe Core.Natural)
listIncomingTypedLinks_maxResults = Lens.lens (\ListIncomingTypedLinks' {maxResults} -> maxResults) (\s@ListIncomingTypedLinks' {} a -> s {maxResults = a} :: ListIncomingTypedLinks)

-- | The consistency level to execute the request at.
listIncomingTypedLinks_consistencyLevel :: Lens.Lens' ListIncomingTypedLinks (Core.Maybe ConsistencyLevel)
listIncomingTypedLinks_consistencyLevel = Lens.lens (\ListIncomingTypedLinks' {consistencyLevel} -> consistencyLevel) (\s@ListIncomingTypedLinks' {} a -> s {consistencyLevel = a} :: ListIncomingTypedLinks)

-- | Provides range filters for multiple attributes. When providing ranges to
-- typed link selection, any inexact ranges must be specified at the end.
-- Any attributes that do not have a range specified are presumed to match
-- the entire range.
listIncomingTypedLinks_filterAttributeRanges :: Lens.Lens' ListIncomingTypedLinks (Core.Maybe [TypedLinkAttributeRange])
listIncomingTypedLinks_filterAttributeRanges = Lens.lens (\ListIncomingTypedLinks' {filterAttributeRanges} -> filterAttributeRanges) (\s@ListIncomingTypedLinks' {} a -> s {filterAttributeRanges = a} :: ListIncomingTypedLinks) Core.. Lens.mapping Lens._Coerce

-- | The Amazon Resource Name (ARN) of the directory where you want to list
-- the typed links.
listIncomingTypedLinks_directoryArn :: Lens.Lens' ListIncomingTypedLinks Core.Text
listIncomingTypedLinks_directoryArn = Lens.lens (\ListIncomingTypedLinks' {directoryArn} -> directoryArn) (\s@ListIncomingTypedLinks' {} a -> s {directoryArn = a} :: ListIncomingTypedLinks)

-- | Reference that identifies the object whose attributes will be listed.
listIncomingTypedLinks_objectReference :: Lens.Lens' ListIncomingTypedLinks ObjectReference
listIncomingTypedLinks_objectReference = Lens.lens (\ListIncomingTypedLinks' {objectReference} -> objectReference) (\s@ListIncomingTypedLinks' {} a -> s {objectReference = a} :: ListIncomingTypedLinks)

instance Core.AWSPager ListIncomingTypedLinks where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listIncomingTypedLinksResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listIncomingTypedLinksResponse_linkSpecifiers
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listIncomingTypedLinks_nextToken
          Lens..~ rs
          Lens.^? listIncomingTypedLinksResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest ListIncomingTypedLinks where
  type
    AWSResponse ListIncomingTypedLinks =
      ListIncomingTypedLinksResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListIncomingTypedLinksResponse'
            Core.<$> (x Core..?> "LinkSpecifiers" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListIncomingTypedLinks

instance Core.NFData ListIncomingTypedLinks

instance Core.ToHeaders ListIncomingTypedLinks where
  toHeaders ListIncomingTypedLinks' {..} =
    Core.mconcat
      ["x-amz-data-partition" Core.=# directoryArn]

instance Core.ToJSON ListIncomingTypedLinks where
  toJSON ListIncomingTypedLinks' {..} =
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

instance Core.ToPath ListIncomingTypedLinks where
  toPath =
    Core.const
      "/amazonclouddirectory/2017-01-11/typedlink/incoming"

instance Core.ToQuery ListIncomingTypedLinks where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListIncomingTypedLinksResponse' smart constructor.
data ListIncomingTypedLinksResponse = ListIncomingTypedLinksResponse'
  { -- | Returns one or more typed link specifiers as output.
    linkSpecifiers :: Core.Maybe [TypedLinkSpecifier],
    -- | The pagination token.
    nextToken :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListIncomingTypedLinksResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'linkSpecifiers', 'listIncomingTypedLinksResponse_linkSpecifiers' - Returns one or more typed link specifiers as output.
--
-- 'nextToken', 'listIncomingTypedLinksResponse_nextToken' - The pagination token.
--
-- 'httpStatus', 'listIncomingTypedLinksResponse_httpStatus' - The response's http status code.
newListIncomingTypedLinksResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListIncomingTypedLinksResponse
newListIncomingTypedLinksResponse pHttpStatus_ =
  ListIncomingTypedLinksResponse'
    { linkSpecifiers =
        Core.Nothing,
      nextToken = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Returns one or more typed link specifiers as output.
listIncomingTypedLinksResponse_linkSpecifiers :: Lens.Lens' ListIncomingTypedLinksResponse (Core.Maybe [TypedLinkSpecifier])
listIncomingTypedLinksResponse_linkSpecifiers = Lens.lens (\ListIncomingTypedLinksResponse' {linkSpecifiers} -> linkSpecifiers) (\s@ListIncomingTypedLinksResponse' {} a -> s {linkSpecifiers = a} :: ListIncomingTypedLinksResponse) Core.. Lens.mapping Lens._Coerce

-- | The pagination token.
listIncomingTypedLinksResponse_nextToken :: Lens.Lens' ListIncomingTypedLinksResponse (Core.Maybe Core.Text)
listIncomingTypedLinksResponse_nextToken = Lens.lens (\ListIncomingTypedLinksResponse' {nextToken} -> nextToken) (\s@ListIncomingTypedLinksResponse' {} a -> s {nextToken = a} :: ListIncomingTypedLinksResponse)

-- | The response's http status code.
listIncomingTypedLinksResponse_httpStatus :: Lens.Lens' ListIncomingTypedLinksResponse Core.Int
listIncomingTypedLinksResponse_httpStatus = Lens.lens (\ListIncomingTypedLinksResponse' {httpStatus} -> httpStatus) (\s@ListIncomingTypedLinksResponse' {} a -> s {httpStatus = a} :: ListIncomingTypedLinksResponse)

instance Core.NFData ListIncomingTypedLinksResponse
