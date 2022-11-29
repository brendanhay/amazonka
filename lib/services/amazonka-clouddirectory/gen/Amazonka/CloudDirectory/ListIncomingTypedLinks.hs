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
-- Module      : Amazonka.CloudDirectory.ListIncomingTypedLinks
-- Copyright   : (c) 2013-2022 Brendan Hay
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
module Amazonka.CloudDirectory.ListIncomingTypedLinks
  ( -- * Creating a Request
    ListIncomingTypedLinks (..),
    newListIncomingTypedLinks,

    -- * Request Lenses
    listIncomingTypedLinks_nextToken,
    listIncomingTypedLinks_consistencyLevel,
    listIncomingTypedLinks_maxResults,
    listIncomingTypedLinks_filterAttributeRanges,
    listIncomingTypedLinks_filterTypedLink,
    listIncomingTypedLinks_directoryArn,
    listIncomingTypedLinks_objectReference,

    -- * Destructuring the Response
    ListIncomingTypedLinksResponse (..),
    newListIncomingTypedLinksResponse,

    -- * Response Lenses
    listIncomingTypedLinksResponse_nextToken,
    listIncomingTypedLinksResponse_linkSpecifiers,
    listIncomingTypedLinksResponse_httpStatus,
  )
where

import Amazonka.CloudDirectory.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListIncomingTypedLinks' smart constructor.
data ListIncomingTypedLinks = ListIncomingTypedLinks'
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
    -- | Filters are interpreted in the order of the attributes on the typed link
    -- facet, not the order in which they are supplied to any API calls.
    filterTypedLink :: Prelude.Maybe TypedLinkSchemaAndFacetName,
    -- | The Amazon Resource Name (ARN) of the directory where you want to list
    -- the typed links.
    directoryArn :: Prelude.Text,
    -- | Reference that identifies the object whose attributes will be listed.
    objectReference :: ObjectReference
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'consistencyLevel', 'listIncomingTypedLinks_consistencyLevel' - The consistency level to execute the request at.
--
-- 'maxResults', 'listIncomingTypedLinks_maxResults' - The maximum number of results to retrieve.
--
-- 'filterAttributeRanges', 'listIncomingTypedLinks_filterAttributeRanges' - Provides range filters for multiple attributes. When providing ranges to
-- typed link selection, any inexact ranges must be specified at the end.
-- Any attributes that do not have a range specified are presumed to match
-- the entire range.
--
-- 'filterTypedLink', 'listIncomingTypedLinks_filterTypedLink' - Filters are interpreted in the order of the attributes on the typed link
-- facet, not the order in which they are supplied to any API calls.
--
-- 'directoryArn', 'listIncomingTypedLinks_directoryArn' - The Amazon Resource Name (ARN) of the directory where you want to list
-- the typed links.
--
-- 'objectReference', 'listIncomingTypedLinks_objectReference' - Reference that identifies the object whose attributes will be listed.
newListIncomingTypedLinks ::
  -- | 'directoryArn'
  Prelude.Text ->
  -- | 'objectReference'
  ObjectReference ->
  ListIncomingTypedLinks
newListIncomingTypedLinks
  pDirectoryArn_
  pObjectReference_ =
    ListIncomingTypedLinks'
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
listIncomingTypedLinks_nextToken :: Lens.Lens' ListIncomingTypedLinks (Prelude.Maybe Prelude.Text)
listIncomingTypedLinks_nextToken = Lens.lens (\ListIncomingTypedLinks' {nextToken} -> nextToken) (\s@ListIncomingTypedLinks' {} a -> s {nextToken = a} :: ListIncomingTypedLinks)

-- | The consistency level to execute the request at.
listIncomingTypedLinks_consistencyLevel :: Lens.Lens' ListIncomingTypedLinks (Prelude.Maybe ConsistencyLevel)
listIncomingTypedLinks_consistencyLevel = Lens.lens (\ListIncomingTypedLinks' {consistencyLevel} -> consistencyLevel) (\s@ListIncomingTypedLinks' {} a -> s {consistencyLevel = a} :: ListIncomingTypedLinks)

-- | The maximum number of results to retrieve.
listIncomingTypedLinks_maxResults :: Lens.Lens' ListIncomingTypedLinks (Prelude.Maybe Prelude.Natural)
listIncomingTypedLinks_maxResults = Lens.lens (\ListIncomingTypedLinks' {maxResults} -> maxResults) (\s@ListIncomingTypedLinks' {} a -> s {maxResults = a} :: ListIncomingTypedLinks)

-- | Provides range filters for multiple attributes. When providing ranges to
-- typed link selection, any inexact ranges must be specified at the end.
-- Any attributes that do not have a range specified are presumed to match
-- the entire range.
listIncomingTypedLinks_filterAttributeRanges :: Lens.Lens' ListIncomingTypedLinks (Prelude.Maybe [TypedLinkAttributeRange])
listIncomingTypedLinks_filterAttributeRanges = Lens.lens (\ListIncomingTypedLinks' {filterAttributeRanges} -> filterAttributeRanges) (\s@ListIncomingTypedLinks' {} a -> s {filterAttributeRanges = a} :: ListIncomingTypedLinks) Prelude.. Lens.mapping Lens.coerced

-- | Filters are interpreted in the order of the attributes on the typed link
-- facet, not the order in which they are supplied to any API calls.
listIncomingTypedLinks_filterTypedLink :: Lens.Lens' ListIncomingTypedLinks (Prelude.Maybe TypedLinkSchemaAndFacetName)
listIncomingTypedLinks_filterTypedLink = Lens.lens (\ListIncomingTypedLinks' {filterTypedLink} -> filterTypedLink) (\s@ListIncomingTypedLinks' {} a -> s {filterTypedLink = a} :: ListIncomingTypedLinks)

-- | The Amazon Resource Name (ARN) of the directory where you want to list
-- the typed links.
listIncomingTypedLinks_directoryArn :: Lens.Lens' ListIncomingTypedLinks Prelude.Text
listIncomingTypedLinks_directoryArn = Lens.lens (\ListIncomingTypedLinks' {directoryArn} -> directoryArn) (\s@ListIncomingTypedLinks' {} a -> s {directoryArn = a} :: ListIncomingTypedLinks)

-- | Reference that identifies the object whose attributes will be listed.
listIncomingTypedLinks_objectReference :: Lens.Lens' ListIncomingTypedLinks ObjectReference
listIncomingTypedLinks_objectReference = Lens.lens (\ListIncomingTypedLinks' {objectReference} -> objectReference) (\s@ListIncomingTypedLinks' {} a -> s {objectReference = a} :: ListIncomingTypedLinks)

instance Core.AWSPager ListIncomingTypedLinks where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listIncomingTypedLinksResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listIncomingTypedLinksResponse_linkSpecifiers
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listIncomingTypedLinks_nextToken
          Lens..~ rs
          Lens.^? listIncomingTypedLinksResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListIncomingTypedLinks where
  type
    AWSResponse ListIncomingTypedLinks =
      ListIncomingTypedLinksResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListIncomingTypedLinksResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "LinkSpecifiers" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListIncomingTypedLinks where
  hashWithSalt _salt ListIncomingTypedLinks' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` consistencyLevel
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` filterAttributeRanges
      `Prelude.hashWithSalt` filterTypedLink
      `Prelude.hashWithSalt` directoryArn
      `Prelude.hashWithSalt` objectReference

instance Prelude.NFData ListIncomingTypedLinks where
  rnf ListIncomingTypedLinks' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf consistencyLevel
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf filterAttributeRanges
      `Prelude.seq` Prelude.rnf filterTypedLink
      `Prelude.seq` Prelude.rnf directoryArn
      `Prelude.seq` Prelude.rnf objectReference

instance Core.ToHeaders ListIncomingTypedLinks where
  toHeaders ListIncomingTypedLinks' {..} =
    Prelude.mconcat
      ["x-amz-data-partition" Core.=# directoryArn]

instance Core.ToJSON ListIncomingTypedLinks where
  toJSON ListIncomingTypedLinks' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("ConsistencyLevel" Core..=)
              Prelude.<$> consistencyLevel,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            ("FilterAttributeRanges" Core..=)
              Prelude.<$> filterAttributeRanges,
            ("FilterTypedLink" Core..=)
              Prelude.<$> filterTypedLink,
            Prelude.Just
              ("ObjectReference" Core..= objectReference)
          ]
      )

instance Core.ToPath ListIncomingTypedLinks where
  toPath =
    Prelude.const
      "/amazonclouddirectory/2017-01-11/typedlink/incoming"

instance Core.ToQuery ListIncomingTypedLinks where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListIncomingTypedLinksResponse' smart constructor.
data ListIncomingTypedLinksResponse = ListIncomingTypedLinksResponse'
  { -- | The pagination token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Returns one or more typed link specifiers as output.
    linkSpecifiers :: Prelude.Maybe [TypedLinkSpecifier],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListIncomingTypedLinksResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listIncomingTypedLinksResponse_nextToken' - The pagination token.
--
-- 'linkSpecifiers', 'listIncomingTypedLinksResponse_linkSpecifiers' - Returns one or more typed link specifiers as output.
--
-- 'httpStatus', 'listIncomingTypedLinksResponse_httpStatus' - The response's http status code.
newListIncomingTypedLinksResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListIncomingTypedLinksResponse
newListIncomingTypedLinksResponse pHttpStatus_ =
  ListIncomingTypedLinksResponse'
    { nextToken =
        Prelude.Nothing,
      linkSpecifiers = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The pagination token.
listIncomingTypedLinksResponse_nextToken :: Lens.Lens' ListIncomingTypedLinksResponse (Prelude.Maybe Prelude.Text)
listIncomingTypedLinksResponse_nextToken = Lens.lens (\ListIncomingTypedLinksResponse' {nextToken} -> nextToken) (\s@ListIncomingTypedLinksResponse' {} a -> s {nextToken = a} :: ListIncomingTypedLinksResponse)

-- | Returns one or more typed link specifiers as output.
listIncomingTypedLinksResponse_linkSpecifiers :: Lens.Lens' ListIncomingTypedLinksResponse (Prelude.Maybe [TypedLinkSpecifier])
listIncomingTypedLinksResponse_linkSpecifiers = Lens.lens (\ListIncomingTypedLinksResponse' {linkSpecifiers} -> linkSpecifiers) (\s@ListIncomingTypedLinksResponse' {} a -> s {linkSpecifiers = a} :: ListIncomingTypedLinksResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listIncomingTypedLinksResponse_httpStatus :: Lens.Lens' ListIncomingTypedLinksResponse Prelude.Int
listIncomingTypedLinksResponse_httpStatus = Lens.lens (\ListIncomingTypedLinksResponse' {httpStatus} -> httpStatus) (\s@ListIncomingTypedLinksResponse' {} a -> s {httpStatus = a} :: ListIncomingTypedLinksResponse)

instance
  Prelude.NFData
    ListIncomingTypedLinksResponse
  where
  rnf ListIncomingTypedLinksResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf linkSpecifiers
      `Prelude.seq` Prelude.rnf httpStatus
