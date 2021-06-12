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
-- Module      : Network.AWS.CloudDirectory.ListObjectPolicies
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns policies attached to an object in pagination fashion.
--
-- This operation returns paginated results.
module Network.AWS.CloudDirectory.ListObjectPolicies
  ( -- * Creating a Request
    ListObjectPolicies (..),
    newListObjectPolicies,

    -- * Request Lenses
    listObjectPolicies_nextToken,
    listObjectPolicies_maxResults,
    listObjectPolicies_consistencyLevel,
    listObjectPolicies_directoryArn,
    listObjectPolicies_objectReference,

    -- * Destructuring the Response
    ListObjectPoliciesResponse (..),
    newListObjectPoliciesResponse,

    -- * Response Lenses
    listObjectPoliciesResponse_nextToken,
    listObjectPoliciesResponse_attachedPolicyIds,
    listObjectPoliciesResponse_httpStatus,
  )
where

import Network.AWS.CloudDirectory.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListObjectPolicies' smart constructor.
data ListObjectPolicies = ListObjectPolicies'
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
    -- where objects reside. For more information, see arns.
    directoryArn :: Core.Text,
    -- | Reference that identifies the object for which policies will be listed.
    objectReference :: ObjectReference
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListObjectPolicies' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listObjectPolicies_nextToken' - The pagination token.
--
-- 'maxResults', 'listObjectPolicies_maxResults' - The maximum number of items to be retrieved in a single call. This is an
-- approximate number.
--
-- 'consistencyLevel', 'listObjectPolicies_consistencyLevel' - Represents the manner and timing in which the successful write or update
-- of an object is reflected in a subsequent read operation of that same
-- object.
--
-- 'directoryArn', 'listObjectPolicies_directoryArn' - The Amazon Resource Name (ARN) that is associated with the Directory
-- where objects reside. For more information, see arns.
--
-- 'objectReference', 'listObjectPolicies_objectReference' - Reference that identifies the object for which policies will be listed.
newListObjectPolicies ::
  -- | 'directoryArn'
  Core.Text ->
  -- | 'objectReference'
  ObjectReference ->
  ListObjectPolicies
newListObjectPolicies
  pDirectoryArn_
  pObjectReference_ =
    ListObjectPolicies'
      { nextToken = Core.Nothing,
        maxResults = Core.Nothing,
        consistencyLevel = Core.Nothing,
        directoryArn = pDirectoryArn_,
        objectReference = pObjectReference_
      }

-- | The pagination token.
listObjectPolicies_nextToken :: Lens.Lens' ListObjectPolicies (Core.Maybe Core.Text)
listObjectPolicies_nextToken = Lens.lens (\ListObjectPolicies' {nextToken} -> nextToken) (\s@ListObjectPolicies' {} a -> s {nextToken = a} :: ListObjectPolicies)

-- | The maximum number of items to be retrieved in a single call. This is an
-- approximate number.
listObjectPolicies_maxResults :: Lens.Lens' ListObjectPolicies (Core.Maybe Core.Natural)
listObjectPolicies_maxResults = Lens.lens (\ListObjectPolicies' {maxResults} -> maxResults) (\s@ListObjectPolicies' {} a -> s {maxResults = a} :: ListObjectPolicies)

-- | Represents the manner and timing in which the successful write or update
-- of an object is reflected in a subsequent read operation of that same
-- object.
listObjectPolicies_consistencyLevel :: Lens.Lens' ListObjectPolicies (Core.Maybe ConsistencyLevel)
listObjectPolicies_consistencyLevel = Lens.lens (\ListObjectPolicies' {consistencyLevel} -> consistencyLevel) (\s@ListObjectPolicies' {} a -> s {consistencyLevel = a} :: ListObjectPolicies)

-- | The Amazon Resource Name (ARN) that is associated with the Directory
-- where objects reside. For more information, see arns.
listObjectPolicies_directoryArn :: Lens.Lens' ListObjectPolicies Core.Text
listObjectPolicies_directoryArn = Lens.lens (\ListObjectPolicies' {directoryArn} -> directoryArn) (\s@ListObjectPolicies' {} a -> s {directoryArn = a} :: ListObjectPolicies)

-- | Reference that identifies the object for which policies will be listed.
listObjectPolicies_objectReference :: Lens.Lens' ListObjectPolicies ObjectReference
listObjectPolicies_objectReference = Lens.lens (\ListObjectPolicies' {objectReference} -> objectReference) (\s@ListObjectPolicies' {} a -> s {objectReference = a} :: ListObjectPolicies)

instance Core.AWSPager ListObjectPolicies where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listObjectPoliciesResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listObjectPoliciesResponse_attachedPolicyIds
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listObjectPolicies_nextToken
          Lens..~ rs
          Lens.^? listObjectPoliciesResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest ListObjectPolicies where
  type
    AWSResponse ListObjectPolicies =
      ListObjectPoliciesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListObjectPoliciesResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "AttachedPolicyIds" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListObjectPolicies

instance Core.NFData ListObjectPolicies

instance Core.ToHeaders ListObjectPolicies where
  toHeaders ListObjectPolicies' {..} =
    Core.mconcat
      [ "x-amz-consistency-level" Core.=# consistencyLevel,
        "x-amz-data-partition" Core.=# directoryArn
      ]

instance Core.ToJSON ListObjectPolicies where
  toJSON ListObjectPolicies' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults,
            Core.Just
              ("ObjectReference" Core..= objectReference)
          ]
      )

instance Core.ToPath ListObjectPolicies where
  toPath =
    Core.const
      "/amazonclouddirectory/2017-01-11/object/policy"

instance Core.ToQuery ListObjectPolicies where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListObjectPoliciesResponse' smart constructor.
data ListObjectPoliciesResponse = ListObjectPoliciesResponse'
  { -- | The pagination token.
    nextToken :: Core.Maybe Core.Text,
    -- | A list of policy @ObjectIdentifiers@, that are attached to the object.
    attachedPolicyIds :: Core.Maybe [Core.Text],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListObjectPoliciesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listObjectPoliciesResponse_nextToken' - The pagination token.
--
-- 'attachedPolicyIds', 'listObjectPoliciesResponse_attachedPolicyIds' - A list of policy @ObjectIdentifiers@, that are attached to the object.
--
-- 'httpStatus', 'listObjectPoliciesResponse_httpStatus' - The response's http status code.
newListObjectPoliciesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListObjectPoliciesResponse
newListObjectPoliciesResponse pHttpStatus_ =
  ListObjectPoliciesResponse'
    { nextToken =
        Core.Nothing,
      attachedPolicyIds = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The pagination token.
listObjectPoliciesResponse_nextToken :: Lens.Lens' ListObjectPoliciesResponse (Core.Maybe Core.Text)
listObjectPoliciesResponse_nextToken = Lens.lens (\ListObjectPoliciesResponse' {nextToken} -> nextToken) (\s@ListObjectPoliciesResponse' {} a -> s {nextToken = a} :: ListObjectPoliciesResponse)

-- | A list of policy @ObjectIdentifiers@, that are attached to the object.
listObjectPoliciesResponse_attachedPolicyIds :: Lens.Lens' ListObjectPoliciesResponse (Core.Maybe [Core.Text])
listObjectPoliciesResponse_attachedPolicyIds = Lens.lens (\ListObjectPoliciesResponse' {attachedPolicyIds} -> attachedPolicyIds) (\s@ListObjectPoliciesResponse' {} a -> s {attachedPolicyIds = a} :: ListObjectPoliciesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listObjectPoliciesResponse_httpStatus :: Lens.Lens' ListObjectPoliciesResponse Core.Int
listObjectPoliciesResponse_httpStatus = Lens.lens (\ListObjectPoliciesResponse' {httpStatus} -> httpStatus) (\s@ListObjectPoliciesResponse' {} a -> s {httpStatus = a} :: ListObjectPoliciesResponse)

instance Core.NFData ListObjectPoliciesResponse
