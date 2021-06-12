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
-- Module      : Network.AWS.CloudDirectory.ListPolicyAttachments
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns all of the @ObjectIdentifiers@ to which a given policy is
-- attached.
--
-- This operation returns paginated results.
module Network.AWS.CloudDirectory.ListPolicyAttachments
  ( -- * Creating a Request
    ListPolicyAttachments (..),
    newListPolicyAttachments,

    -- * Request Lenses
    listPolicyAttachments_nextToken,
    listPolicyAttachments_maxResults,
    listPolicyAttachments_consistencyLevel,
    listPolicyAttachments_directoryArn,
    listPolicyAttachments_policyReference,

    -- * Destructuring the Response
    ListPolicyAttachmentsResponse (..),
    newListPolicyAttachmentsResponse,

    -- * Response Lenses
    listPolicyAttachmentsResponse_nextToken,
    listPolicyAttachmentsResponse_objectIdentifiers,
    listPolicyAttachmentsResponse_httpStatus,
  )
where

import Network.AWS.CloudDirectory.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListPolicyAttachments' smart constructor.
data ListPolicyAttachments = ListPolicyAttachments'
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
    -- | The reference that identifies the policy object.
    policyReference :: ObjectReference
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListPolicyAttachments' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listPolicyAttachments_nextToken' - The pagination token.
--
-- 'maxResults', 'listPolicyAttachments_maxResults' - The maximum number of items to be retrieved in a single call. This is an
-- approximate number.
--
-- 'consistencyLevel', 'listPolicyAttachments_consistencyLevel' - Represents the manner and timing in which the successful write or update
-- of an object is reflected in a subsequent read operation of that same
-- object.
--
-- 'directoryArn', 'listPolicyAttachments_directoryArn' - The Amazon Resource Name (ARN) that is associated with the Directory
-- where objects reside. For more information, see arns.
--
-- 'policyReference', 'listPolicyAttachments_policyReference' - The reference that identifies the policy object.
newListPolicyAttachments ::
  -- | 'directoryArn'
  Core.Text ->
  -- | 'policyReference'
  ObjectReference ->
  ListPolicyAttachments
newListPolicyAttachments
  pDirectoryArn_
  pPolicyReference_ =
    ListPolicyAttachments'
      { nextToken = Core.Nothing,
        maxResults = Core.Nothing,
        consistencyLevel = Core.Nothing,
        directoryArn = pDirectoryArn_,
        policyReference = pPolicyReference_
      }

-- | The pagination token.
listPolicyAttachments_nextToken :: Lens.Lens' ListPolicyAttachments (Core.Maybe Core.Text)
listPolicyAttachments_nextToken = Lens.lens (\ListPolicyAttachments' {nextToken} -> nextToken) (\s@ListPolicyAttachments' {} a -> s {nextToken = a} :: ListPolicyAttachments)

-- | The maximum number of items to be retrieved in a single call. This is an
-- approximate number.
listPolicyAttachments_maxResults :: Lens.Lens' ListPolicyAttachments (Core.Maybe Core.Natural)
listPolicyAttachments_maxResults = Lens.lens (\ListPolicyAttachments' {maxResults} -> maxResults) (\s@ListPolicyAttachments' {} a -> s {maxResults = a} :: ListPolicyAttachments)

-- | Represents the manner and timing in which the successful write or update
-- of an object is reflected in a subsequent read operation of that same
-- object.
listPolicyAttachments_consistencyLevel :: Lens.Lens' ListPolicyAttachments (Core.Maybe ConsistencyLevel)
listPolicyAttachments_consistencyLevel = Lens.lens (\ListPolicyAttachments' {consistencyLevel} -> consistencyLevel) (\s@ListPolicyAttachments' {} a -> s {consistencyLevel = a} :: ListPolicyAttachments)

-- | The Amazon Resource Name (ARN) that is associated with the Directory
-- where objects reside. For more information, see arns.
listPolicyAttachments_directoryArn :: Lens.Lens' ListPolicyAttachments Core.Text
listPolicyAttachments_directoryArn = Lens.lens (\ListPolicyAttachments' {directoryArn} -> directoryArn) (\s@ListPolicyAttachments' {} a -> s {directoryArn = a} :: ListPolicyAttachments)

-- | The reference that identifies the policy object.
listPolicyAttachments_policyReference :: Lens.Lens' ListPolicyAttachments ObjectReference
listPolicyAttachments_policyReference = Lens.lens (\ListPolicyAttachments' {policyReference} -> policyReference) (\s@ListPolicyAttachments' {} a -> s {policyReference = a} :: ListPolicyAttachments)

instance Core.AWSPager ListPolicyAttachments where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listPolicyAttachmentsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listPolicyAttachmentsResponse_objectIdentifiers
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listPolicyAttachments_nextToken
          Lens..~ rs
          Lens.^? listPolicyAttachmentsResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest ListPolicyAttachments where
  type
    AWSResponse ListPolicyAttachments =
      ListPolicyAttachmentsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListPolicyAttachmentsResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "ObjectIdentifiers" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListPolicyAttachments

instance Core.NFData ListPolicyAttachments

instance Core.ToHeaders ListPolicyAttachments where
  toHeaders ListPolicyAttachments' {..} =
    Core.mconcat
      [ "x-amz-consistency-level" Core.=# consistencyLevel,
        "x-amz-data-partition" Core.=# directoryArn
      ]

instance Core.ToJSON ListPolicyAttachments where
  toJSON ListPolicyAttachments' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults,
            Core.Just
              ("PolicyReference" Core..= policyReference)
          ]
      )

instance Core.ToPath ListPolicyAttachments where
  toPath =
    Core.const
      "/amazonclouddirectory/2017-01-11/policy/attachment"

instance Core.ToQuery ListPolicyAttachments where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListPolicyAttachmentsResponse' smart constructor.
data ListPolicyAttachmentsResponse = ListPolicyAttachmentsResponse'
  { -- | The pagination token.
    nextToken :: Core.Maybe Core.Text,
    -- | A list of @ObjectIdentifiers@ to which the policy is attached.
    objectIdentifiers :: Core.Maybe [Core.Text],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListPolicyAttachmentsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listPolicyAttachmentsResponse_nextToken' - The pagination token.
--
-- 'objectIdentifiers', 'listPolicyAttachmentsResponse_objectIdentifiers' - A list of @ObjectIdentifiers@ to which the policy is attached.
--
-- 'httpStatus', 'listPolicyAttachmentsResponse_httpStatus' - The response's http status code.
newListPolicyAttachmentsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListPolicyAttachmentsResponse
newListPolicyAttachmentsResponse pHttpStatus_ =
  ListPolicyAttachmentsResponse'
    { nextToken =
        Core.Nothing,
      objectIdentifiers = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The pagination token.
listPolicyAttachmentsResponse_nextToken :: Lens.Lens' ListPolicyAttachmentsResponse (Core.Maybe Core.Text)
listPolicyAttachmentsResponse_nextToken = Lens.lens (\ListPolicyAttachmentsResponse' {nextToken} -> nextToken) (\s@ListPolicyAttachmentsResponse' {} a -> s {nextToken = a} :: ListPolicyAttachmentsResponse)

-- | A list of @ObjectIdentifiers@ to which the policy is attached.
listPolicyAttachmentsResponse_objectIdentifiers :: Lens.Lens' ListPolicyAttachmentsResponse (Core.Maybe [Core.Text])
listPolicyAttachmentsResponse_objectIdentifiers = Lens.lens (\ListPolicyAttachmentsResponse' {objectIdentifiers} -> objectIdentifiers) (\s@ListPolicyAttachmentsResponse' {} a -> s {objectIdentifiers = a} :: ListPolicyAttachmentsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listPolicyAttachmentsResponse_httpStatus :: Lens.Lens' ListPolicyAttachmentsResponse Core.Int
listPolicyAttachmentsResponse_httpStatus = Lens.lens (\ListPolicyAttachmentsResponse' {httpStatus} -> httpStatus) (\s@ListPolicyAttachmentsResponse' {} a -> s {httpStatus = a} :: ListPolicyAttachmentsResponse)

instance Core.NFData ListPolicyAttachmentsResponse
