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
-- Module      : Network.AWS.CloudDirectory.ListObjectChildren
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a paginated list of child objects that are associated with a
-- given object.
module Network.AWS.CloudDirectory.ListObjectChildren
  ( -- * Creating a Request
    ListObjectChildren (..),
    newListObjectChildren,

    -- * Request Lenses
    listObjectChildren_nextToken,
    listObjectChildren_maxResults,
    listObjectChildren_consistencyLevel,
    listObjectChildren_directoryArn,
    listObjectChildren_objectReference,

    -- * Destructuring the Response
    ListObjectChildrenResponse (..),
    newListObjectChildrenResponse,

    -- * Response Lenses
    listObjectChildrenResponse_nextToken,
    listObjectChildrenResponse_children,
    listObjectChildrenResponse_httpStatus,
  )
where

import Network.AWS.CloudDirectory.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListObjectChildren' smart constructor.
data ListObjectChildren = ListObjectChildren'
  { -- | The pagination token.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of items to be retrieved in a single call. This is an
    -- approximate number.
    maxResults :: Core.Maybe Core.Natural,
    -- | Represents the manner and timing in which the successful write or update
    -- of an object is reflected in a subsequent read operation of that same
    -- object.
    consistencyLevel :: Core.Maybe ConsistencyLevel,
    -- | The Amazon Resource Name (ARN) that is associated with the Directory
    -- where the object resides. For more information, see arns.
    directoryArn :: Core.Text,
    -- | The reference that identifies the object for which child objects are
    -- being listed.
    objectReference :: ObjectReference
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListObjectChildren' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listObjectChildren_nextToken' - The pagination token.
--
-- 'maxResults', 'listObjectChildren_maxResults' - The maximum number of items to be retrieved in a single call. This is an
-- approximate number.
--
-- 'consistencyLevel', 'listObjectChildren_consistencyLevel' - Represents the manner and timing in which the successful write or update
-- of an object is reflected in a subsequent read operation of that same
-- object.
--
-- 'directoryArn', 'listObjectChildren_directoryArn' - The Amazon Resource Name (ARN) that is associated with the Directory
-- where the object resides. For more information, see arns.
--
-- 'objectReference', 'listObjectChildren_objectReference' - The reference that identifies the object for which child objects are
-- being listed.
newListObjectChildren ::
  -- | 'directoryArn'
  Core.Text ->
  -- | 'objectReference'
  ObjectReference ->
  ListObjectChildren
newListObjectChildren
  pDirectoryArn_
  pObjectReference_ =
    ListObjectChildren'
      { nextToken = Core.Nothing,
        maxResults = Core.Nothing,
        consistencyLevel = Core.Nothing,
        directoryArn = pDirectoryArn_,
        objectReference = pObjectReference_
      }

-- | The pagination token.
listObjectChildren_nextToken :: Lens.Lens' ListObjectChildren (Core.Maybe Core.Text)
listObjectChildren_nextToken = Lens.lens (\ListObjectChildren' {nextToken} -> nextToken) (\s@ListObjectChildren' {} a -> s {nextToken = a} :: ListObjectChildren)

-- | The maximum number of items to be retrieved in a single call. This is an
-- approximate number.
listObjectChildren_maxResults :: Lens.Lens' ListObjectChildren (Core.Maybe Core.Natural)
listObjectChildren_maxResults = Lens.lens (\ListObjectChildren' {maxResults} -> maxResults) (\s@ListObjectChildren' {} a -> s {maxResults = a} :: ListObjectChildren)

-- | Represents the manner and timing in which the successful write or update
-- of an object is reflected in a subsequent read operation of that same
-- object.
listObjectChildren_consistencyLevel :: Lens.Lens' ListObjectChildren (Core.Maybe ConsistencyLevel)
listObjectChildren_consistencyLevel = Lens.lens (\ListObjectChildren' {consistencyLevel} -> consistencyLevel) (\s@ListObjectChildren' {} a -> s {consistencyLevel = a} :: ListObjectChildren)

-- | The Amazon Resource Name (ARN) that is associated with the Directory
-- where the object resides. For more information, see arns.
listObjectChildren_directoryArn :: Lens.Lens' ListObjectChildren Core.Text
listObjectChildren_directoryArn = Lens.lens (\ListObjectChildren' {directoryArn} -> directoryArn) (\s@ListObjectChildren' {} a -> s {directoryArn = a} :: ListObjectChildren)

-- | The reference that identifies the object for which child objects are
-- being listed.
listObjectChildren_objectReference :: Lens.Lens' ListObjectChildren ObjectReference
listObjectChildren_objectReference = Lens.lens (\ListObjectChildren' {objectReference} -> objectReference) (\s@ListObjectChildren' {} a -> s {objectReference = a} :: ListObjectChildren)

instance Core.AWSRequest ListObjectChildren where
  type
    AWSResponse ListObjectChildren =
      ListObjectChildrenResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListObjectChildrenResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "Children" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListObjectChildren

instance Core.NFData ListObjectChildren

instance Core.ToHeaders ListObjectChildren where
  toHeaders ListObjectChildren' {..} =
    Core.mconcat
      [ "x-amz-consistency-level" Core.=# consistencyLevel,
        "x-amz-data-partition" Core.=# directoryArn
      ]

instance Core.ToJSON ListObjectChildren where
  toJSON ListObjectChildren' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults,
            Core.Just
              ("ObjectReference" Core..= objectReference)
          ]
      )

instance Core.ToPath ListObjectChildren where
  toPath =
    Core.const
      "/amazonclouddirectory/2017-01-11/object/children"

instance Core.ToQuery ListObjectChildren where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListObjectChildrenResponse' smart constructor.
data ListObjectChildrenResponse = ListObjectChildrenResponse'
  { -- | The pagination token.
    nextToken :: Core.Maybe Core.Text,
    -- | Children structure, which is a map with key as the @LinkName@ and
    -- @ObjectIdentifier@ as the value.
    children :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListObjectChildrenResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listObjectChildrenResponse_nextToken' - The pagination token.
--
-- 'children', 'listObjectChildrenResponse_children' - Children structure, which is a map with key as the @LinkName@ and
-- @ObjectIdentifier@ as the value.
--
-- 'httpStatus', 'listObjectChildrenResponse_httpStatus' - The response's http status code.
newListObjectChildrenResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListObjectChildrenResponse
newListObjectChildrenResponse pHttpStatus_ =
  ListObjectChildrenResponse'
    { nextToken =
        Core.Nothing,
      children = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The pagination token.
listObjectChildrenResponse_nextToken :: Lens.Lens' ListObjectChildrenResponse (Core.Maybe Core.Text)
listObjectChildrenResponse_nextToken = Lens.lens (\ListObjectChildrenResponse' {nextToken} -> nextToken) (\s@ListObjectChildrenResponse' {} a -> s {nextToken = a} :: ListObjectChildrenResponse)

-- | Children structure, which is a map with key as the @LinkName@ and
-- @ObjectIdentifier@ as the value.
listObjectChildrenResponse_children :: Lens.Lens' ListObjectChildrenResponse (Core.Maybe (Core.HashMap Core.Text Core.Text))
listObjectChildrenResponse_children = Lens.lens (\ListObjectChildrenResponse' {children} -> children) (\s@ListObjectChildrenResponse' {} a -> s {children = a} :: ListObjectChildrenResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listObjectChildrenResponse_httpStatus :: Lens.Lens' ListObjectChildrenResponse Core.Int
listObjectChildrenResponse_httpStatus = Lens.lens (\ListObjectChildrenResponse' {httpStatus} -> httpStatus) (\s@ListObjectChildrenResponse' {} a -> s {httpStatus = a} :: ListObjectChildrenResponse)

instance Core.NFData ListObjectChildrenResponse
