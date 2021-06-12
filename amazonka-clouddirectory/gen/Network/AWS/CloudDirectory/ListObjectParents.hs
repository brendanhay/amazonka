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
-- Module      : Network.AWS.CloudDirectory.ListObjectParents
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists parent objects that are associated with a given object in
-- pagination fashion.
module Network.AWS.CloudDirectory.ListObjectParents
  ( -- * Creating a Request
    ListObjectParents (..),
    newListObjectParents,

    -- * Request Lenses
    listObjectParents_nextToken,
    listObjectParents_maxResults,
    listObjectParents_includeAllLinksToEachParent,
    listObjectParents_consistencyLevel,
    listObjectParents_directoryArn,
    listObjectParents_objectReference,

    -- * Destructuring the Response
    ListObjectParentsResponse (..),
    newListObjectParentsResponse,

    -- * Response Lenses
    listObjectParentsResponse_parents,
    listObjectParentsResponse_parentLinks,
    listObjectParentsResponse_nextToken,
    listObjectParentsResponse_httpStatus,
  )
where

import Network.AWS.CloudDirectory.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListObjectParents' smart constructor.
data ListObjectParents = ListObjectParents'
  { -- | The pagination token.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of items to be retrieved in a single call. This is an
    -- approximate number.
    maxResults :: Core.Maybe Core.Natural,
    -- | When set to True, returns all ListObjectParentsResponse$ParentLinks.
    -- There could be multiple links between a parent-child pair.
    includeAllLinksToEachParent :: Core.Maybe Core.Bool,
    -- | Represents the manner and timing in which the successful write or update
    -- of an object is reflected in a subsequent read operation of that same
    -- object.
    consistencyLevel :: Core.Maybe ConsistencyLevel,
    -- | The Amazon Resource Name (ARN) that is associated with the Directory
    -- where the object resides. For more information, see arns.
    directoryArn :: Core.Text,
    -- | The reference that identifies the object for which parent objects are
    -- being listed.
    objectReference :: ObjectReference
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListObjectParents' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listObjectParents_nextToken' - The pagination token.
--
-- 'maxResults', 'listObjectParents_maxResults' - The maximum number of items to be retrieved in a single call. This is an
-- approximate number.
--
-- 'includeAllLinksToEachParent', 'listObjectParents_includeAllLinksToEachParent' - When set to True, returns all ListObjectParentsResponse$ParentLinks.
-- There could be multiple links between a parent-child pair.
--
-- 'consistencyLevel', 'listObjectParents_consistencyLevel' - Represents the manner and timing in which the successful write or update
-- of an object is reflected in a subsequent read operation of that same
-- object.
--
-- 'directoryArn', 'listObjectParents_directoryArn' - The Amazon Resource Name (ARN) that is associated with the Directory
-- where the object resides. For more information, see arns.
--
-- 'objectReference', 'listObjectParents_objectReference' - The reference that identifies the object for which parent objects are
-- being listed.
newListObjectParents ::
  -- | 'directoryArn'
  Core.Text ->
  -- | 'objectReference'
  ObjectReference ->
  ListObjectParents
newListObjectParents pDirectoryArn_ pObjectReference_ =
  ListObjectParents'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      includeAllLinksToEachParent = Core.Nothing,
      consistencyLevel = Core.Nothing,
      directoryArn = pDirectoryArn_,
      objectReference = pObjectReference_
    }

-- | The pagination token.
listObjectParents_nextToken :: Lens.Lens' ListObjectParents (Core.Maybe Core.Text)
listObjectParents_nextToken = Lens.lens (\ListObjectParents' {nextToken} -> nextToken) (\s@ListObjectParents' {} a -> s {nextToken = a} :: ListObjectParents)

-- | The maximum number of items to be retrieved in a single call. This is an
-- approximate number.
listObjectParents_maxResults :: Lens.Lens' ListObjectParents (Core.Maybe Core.Natural)
listObjectParents_maxResults = Lens.lens (\ListObjectParents' {maxResults} -> maxResults) (\s@ListObjectParents' {} a -> s {maxResults = a} :: ListObjectParents)

-- | When set to True, returns all ListObjectParentsResponse$ParentLinks.
-- There could be multiple links between a parent-child pair.
listObjectParents_includeAllLinksToEachParent :: Lens.Lens' ListObjectParents (Core.Maybe Core.Bool)
listObjectParents_includeAllLinksToEachParent = Lens.lens (\ListObjectParents' {includeAllLinksToEachParent} -> includeAllLinksToEachParent) (\s@ListObjectParents' {} a -> s {includeAllLinksToEachParent = a} :: ListObjectParents)

-- | Represents the manner and timing in which the successful write or update
-- of an object is reflected in a subsequent read operation of that same
-- object.
listObjectParents_consistencyLevel :: Lens.Lens' ListObjectParents (Core.Maybe ConsistencyLevel)
listObjectParents_consistencyLevel = Lens.lens (\ListObjectParents' {consistencyLevel} -> consistencyLevel) (\s@ListObjectParents' {} a -> s {consistencyLevel = a} :: ListObjectParents)

-- | The Amazon Resource Name (ARN) that is associated with the Directory
-- where the object resides. For more information, see arns.
listObjectParents_directoryArn :: Lens.Lens' ListObjectParents Core.Text
listObjectParents_directoryArn = Lens.lens (\ListObjectParents' {directoryArn} -> directoryArn) (\s@ListObjectParents' {} a -> s {directoryArn = a} :: ListObjectParents)

-- | The reference that identifies the object for which parent objects are
-- being listed.
listObjectParents_objectReference :: Lens.Lens' ListObjectParents ObjectReference
listObjectParents_objectReference = Lens.lens (\ListObjectParents' {objectReference} -> objectReference) (\s@ListObjectParents' {} a -> s {objectReference = a} :: ListObjectParents)

instance Core.AWSRequest ListObjectParents where
  type
    AWSResponse ListObjectParents =
      ListObjectParentsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListObjectParentsResponse'
            Core.<$> (x Core..?> "Parents" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "ParentLinks" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListObjectParents

instance Core.NFData ListObjectParents

instance Core.ToHeaders ListObjectParents where
  toHeaders ListObjectParents' {..} =
    Core.mconcat
      [ "x-amz-consistency-level" Core.=# consistencyLevel,
        "x-amz-data-partition" Core.=# directoryArn
      ]

instance Core.ToJSON ListObjectParents where
  toJSON ListObjectParents' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("IncludeAllLinksToEachParent" Core..=)
              Core.<$> includeAllLinksToEachParent,
            Core.Just
              ("ObjectReference" Core..= objectReference)
          ]
      )

instance Core.ToPath ListObjectParents where
  toPath =
    Core.const
      "/amazonclouddirectory/2017-01-11/object/parent"

instance Core.ToQuery ListObjectParents where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListObjectParentsResponse' smart constructor.
data ListObjectParentsResponse = ListObjectParentsResponse'
  { -- | The parent structure, which is a map with key as the @ObjectIdentifier@
    -- and LinkName as the value.
    parents :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | Returns a list of parent reference and LinkName Tuples.
    parentLinks :: Core.Maybe [ObjectIdentifierAndLinkNameTuple],
    -- | The pagination token.
    nextToken :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListObjectParentsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'parents', 'listObjectParentsResponse_parents' - The parent structure, which is a map with key as the @ObjectIdentifier@
-- and LinkName as the value.
--
-- 'parentLinks', 'listObjectParentsResponse_parentLinks' - Returns a list of parent reference and LinkName Tuples.
--
-- 'nextToken', 'listObjectParentsResponse_nextToken' - The pagination token.
--
-- 'httpStatus', 'listObjectParentsResponse_httpStatus' - The response's http status code.
newListObjectParentsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListObjectParentsResponse
newListObjectParentsResponse pHttpStatus_ =
  ListObjectParentsResponse'
    { parents = Core.Nothing,
      parentLinks = Core.Nothing,
      nextToken = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The parent structure, which is a map with key as the @ObjectIdentifier@
-- and LinkName as the value.
listObjectParentsResponse_parents :: Lens.Lens' ListObjectParentsResponse (Core.Maybe (Core.HashMap Core.Text Core.Text))
listObjectParentsResponse_parents = Lens.lens (\ListObjectParentsResponse' {parents} -> parents) (\s@ListObjectParentsResponse' {} a -> s {parents = a} :: ListObjectParentsResponse) Core.. Lens.mapping Lens._Coerce

-- | Returns a list of parent reference and LinkName Tuples.
listObjectParentsResponse_parentLinks :: Lens.Lens' ListObjectParentsResponse (Core.Maybe [ObjectIdentifierAndLinkNameTuple])
listObjectParentsResponse_parentLinks = Lens.lens (\ListObjectParentsResponse' {parentLinks} -> parentLinks) (\s@ListObjectParentsResponse' {} a -> s {parentLinks = a} :: ListObjectParentsResponse) Core.. Lens.mapping Lens._Coerce

-- | The pagination token.
listObjectParentsResponse_nextToken :: Lens.Lens' ListObjectParentsResponse (Core.Maybe Core.Text)
listObjectParentsResponse_nextToken = Lens.lens (\ListObjectParentsResponse' {nextToken} -> nextToken) (\s@ListObjectParentsResponse' {} a -> s {nextToken = a} :: ListObjectParentsResponse)

-- | The response's http status code.
listObjectParentsResponse_httpStatus :: Lens.Lens' ListObjectParentsResponse Core.Int
listObjectParentsResponse_httpStatus = Lens.lens (\ListObjectParentsResponse' {httpStatus} -> httpStatus) (\s@ListObjectParentsResponse' {} a -> s {httpStatus = a} :: ListObjectParentsResponse)

instance Core.NFData ListObjectParentsResponse
