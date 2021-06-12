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
-- Module      : Network.AWS.CloudDirectory.ListObjectParentPaths
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves all available parent paths for any object type such as node,
-- leaf node, policy node, and index node objects. For more information
-- about objects, see
-- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/key_concepts_directorystructure.html Directory Structure>.
--
-- Use this API to evaluate all parents for an object. The call returns all
-- objects from the root of the directory up to the requested object. The
-- API returns the number of paths based on user-defined @MaxResults@, in
-- case there are multiple paths to the parent. The order of the paths and
-- nodes returned is consistent among multiple API calls unless the objects
-- are deleted or moved. Paths not leading to the directory root are
-- ignored from the target object.
--
-- This operation returns paginated results.
module Network.AWS.CloudDirectory.ListObjectParentPaths
  ( -- * Creating a Request
    ListObjectParentPaths (..),
    newListObjectParentPaths,

    -- * Request Lenses
    listObjectParentPaths_nextToken,
    listObjectParentPaths_maxResults,
    listObjectParentPaths_directoryArn,
    listObjectParentPaths_objectReference,

    -- * Destructuring the Response
    ListObjectParentPathsResponse (..),
    newListObjectParentPathsResponse,

    -- * Response Lenses
    listObjectParentPathsResponse_nextToken,
    listObjectParentPathsResponse_pathToObjectIdentifiersList,
    listObjectParentPathsResponse_httpStatus,
  )
where

import Network.AWS.CloudDirectory.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListObjectParentPaths' smart constructor.
data ListObjectParentPaths = ListObjectParentPaths'
  { -- | The pagination token.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of items to be retrieved in a single call. This is an
    -- approximate number.
    maxResults :: Core.Maybe Core.Natural,
    -- | The ARN of the directory to which the parent path applies.
    directoryArn :: Core.Text,
    -- | The reference that identifies the object whose parent paths are listed.
    objectReference :: ObjectReference
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListObjectParentPaths' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listObjectParentPaths_nextToken' - The pagination token.
--
-- 'maxResults', 'listObjectParentPaths_maxResults' - The maximum number of items to be retrieved in a single call. This is an
-- approximate number.
--
-- 'directoryArn', 'listObjectParentPaths_directoryArn' - The ARN of the directory to which the parent path applies.
--
-- 'objectReference', 'listObjectParentPaths_objectReference' - The reference that identifies the object whose parent paths are listed.
newListObjectParentPaths ::
  -- | 'directoryArn'
  Core.Text ->
  -- | 'objectReference'
  ObjectReference ->
  ListObjectParentPaths
newListObjectParentPaths
  pDirectoryArn_
  pObjectReference_ =
    ListObjectParentPaths'
      { nextToken = Core.Nothing,
        maxResults = Core.Nothing,
        directoryArn = pDirectoryArn_,
        objectReference = pObjectReference_
      }

-- | The pagination token.
listObjectParentPaths_nextToken :: Lens.Lens' ListObjectParentPaths (Core.Maybe Core.Text)
listObjectParentPaths_nextToken = Lens.lens (\ListObjectParentPaths' {nextToken} -> nextToken) (\s@ListObjectParentPaths' {} a -> s {nextToken = a} :: ListObjectParentPaths)

-- | The maximum number of items to be retrieved in a single call. This is an
-- approximate number.
listObjectParentPaths_maxResults :: Lens.Lens' ListObjectParentPaths (Core.Maybe Core.Natural)
listObjectParentPaths_maxResults = Lens.lens (\ListObjectParentPaths' {maxResults} -> maxResults) (\s@ListObjectParentPaths' {} a -> s {maxResults = a} :: ListObjectParentPaths)

-- | The ARN of the directory to which the parent path applies.
listObjectParentPaths_directoryArn :: Lens.Lens' ListObjectParentPaths Core.Text
listObjectParentPaths_directoryArn = Lens.lens (\ListObjectParentPaths' {directoryArn} -> directoryArn) (\s@ListObjectParentPaths' {} a -> s {directoryArn = a} :: ListObjectParentPaths)

-- | The reference that identifies the object whose parent paths are listed.
listObjectParentPaths_objectReference :: Lens.Lens' ListObjectParentPaths ObjectReference
listObjectParentPaths_objectReference = Lens.lens (\ListObjectParentPaths' {objectReference} -> objectReference) (\s@ListObjectParentPaths' {} a -> s {objectReference = a} :: ListObjectParentPaths)

instance Core.AWSPager ListObjectParentPaths where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listObjectParentPathsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listObjectParentPathsResponse_pathToObjectIdentifiersList
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listObjectParentPaths_nextToken
          Lens..~ rs
          Lens.^? listObjectParentPathsResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest ListObjectParentPaths where
  type
    AWSResponse ListObjectParentPaths =
      ListObjectParentPathsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListObjectParentPathsResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> ( x Core..?> "PathToObjectIdentifiersList"
                         Core..!@ Core.mempty
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListObjectParentPaths

instance Core.NFData ListObjectParentPaths

instance Core.ToHeaders ListObjectParentPaths where
  toHeaders ListObjectParentPaths' {..} =
    Core.mconcat
      ["x-amz-data-partition" Core.=# directoryArn]

instance Core.ToJSON ListObjectParentPaths where
  toJSON ListObjectParentPaths' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults,
            Core.Just
              ("ObjectReference" Core..= objectReference)
          ]
      )

instance Core.ToPath ListObjectParentPaths where
  toPath =
    Core.const
      "/amazonclouddirectory/2017-01-11/object/parentpaths"

instance Core.ToQuery ListObjectParentPaths where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListObjectParentPathsResponse' smart constructor.
data ListObjectParentPathsResponse = ListObjectParentPathsResponse'
  { -- | The pagination token.
    nextToken :: Core.Maybe Core.Text,
    -- | Returns the path to the @ObjectIdentifiers@ that are associated with the
    -- directory.
    pathToObjectIdentifiersList :: Core.Maybe [PathToObjectIdentifiers],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListObjectParentPathsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listObjectParentPathsResponse_nextToken' - The pagination token.
--
-- 'pathToObjectIdentifiersList', 'listObjectParentPathsResponse_pathToObjectIdentifiersList' - Returns the path to the @ObjectIdentifiers@ that are associated with the
-- directory.
--
-- 'httpStatus', 'listObjectParentPathsResponse_httpStatus' - The response's http status code.
newListObjectParentPathsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListObjectParentPathsResponse
newListObjectParentPathsResponse pHttpStatus_ =
  ListObjectParentPathsResponse'
    { nextToken =
        Core.Nothing,
      pathToObjectIdentifiersList = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The pagination token.
listObjectParentPathsResponse_nextToken :: Lens.Lens' ListObjectParentPathsResponse (Core.Maybe Core.Text)
listObjectParentPathsResponse_nextToken = Lens.lens (\ListObjectParentPathsResponse' {nextToken} -> nextToken) (\s@ListObjectParentPathsResponse' {} a -> s {nextToken = a} :: ListObjectParentPathsResponse)

-- | Returns the path to the @ObjectIdentifiers@ that are associated with the
-- directory.
listObjectParentPathsResponse_pathToObjectIdentifiersList :: Lens.Lens' ListObjectParentPathsResponse (Core.Maybe [PathToObjectIdentifiers])
listObjectParentPathsResponse_pathToObjectIdentifiersList = Lens.lens (\ListObjectParentPathsResponse' {pathToObjectIdentifiersList} -> pathToObjectIdentifiersList) (\s@ListObjectParentPathsResponse' {} a -> s {pathToObjectIdentifiersList = a} :: ListObjectParentPathsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listObjectParentPathsResponse_httpStatus :: Lens.Lens' ListObjectParentPathsResponse Core.Int
listObjectParentPathsResponse_httpStatus = Lens.lens (\ListObjectParentPathsResponse' {httpStatus} -> httpStatus) (\s@ListObjectParentPathsResponse' {} a -> s {httpStatus = a} :: ListObjectParentPathsResponse)

instance Core.NFData ListObjectParentPathsResponse
