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
-- Module      : Network.AWS.CloudDirectory.ListAttachedIndices
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists indices attached to the specified object.
--
-- This operation returns paginated results.
module Network.AWS.CloudDirectory.ListAttachedIndices
  ( -- * Creating a Request
    ListAttachedIndices (..),
    newListAttachedIndices,

    -- * Request Lenses
    listAttachedIndices_nextToken,
    listAttachedIndices_maxResults,
    listAttachedIndices_consistencyLevel,
    listAttachedIndices_directoryArn,
    listAttachedIndices_targetReference,

    -- * Destructuring the Response
    ListAttachedIndicesResponse (..),
    newListAttachedIndicesResponse,

    -- * Response Lenses
    listAttachedIndicesResponse_nextToken,
    listAttachedIndicesResponse_indexAttachments,
    listAttachedIndicesResponse_httpStatus,
  )
where

import Network.AWS.CloudDirectory.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListAttachedIndices' smart constructor.
data ListAttachedIndices = ListAttachedIndices'
  { -- | The pagination token.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of results to retrieve.
    maxResults :: Core.Maybe Core.Natural,
    -- | The consistency level to use for this operation.
    consistencyLevel :: Core.Maybe ConsistencyLevel,
    -- | The ARN of the directory.
    directoryArn :: Core.Text,
    -- | A reference to the object that has indices attached.
    targetReference :: ObjectReference
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListAttachedIndices' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listAttachedIndices_nextToken' - The pagination token.
--
-- 'maxResults', 'listAttachedIndices_maxResults' - The maximum number of results to retrieve.
--
-- 'consistencyLevel', 'listAttachedIndices_consistencyLevel' - The consistency level to use for this operation.
--
-- 'directoryArn', 'listAttachedIndices_directoryArn' - The ARN of the directory.
--
-- 'targetReference', 'listAttachedIndices_targetReference' - A reference to the object that has indices attached.
newListAttachedIndices ::
  -- | 'directoryArn'
  Core.Text ->
  -- | 'targetReference'
  ObjectReference ->
  ListAttachedIndices
newListAttachedIndices
  pDirectoryArn_
  pTargetReference_ =
    ListAttachedIndices'
      { nextToken = Core.Nothing,
        maxResults = Core.Nothing,
        consistencyLevel = Core.Nothing,
        directoryArn = pDirectoryArn_,
        targetReference = pTargetReference_
      }

-- | The pagination token.
listAttachedIndices_nextToken :: Lens.Lens' ListAttachedIndices (Core.Maybe Core.Text)
listAttachedIndices_nextToken = Lens.lens (\ListAttachedIndices' {nextToken} -> nextToken) (\s@ListAttachedIndices' {} a -> s {nextToken = a} :: ListAttachedIndices)

-- | The maximum number of results to retrieve.
listAttachedIndices_maxResults :: Lens.Lens' ListAttachedIndices (Core.Maybe Core.Natural)
listAttachedIndices_maxResults = Lens.lens (\ListAttachedIndices' {maxResults} -> maxResults) (\s@ListAttachedIndices' {} a -> s {maxResults = a} :: ListAttachedIndices)

-- | The consistency level to use for this operation.
listAttachedIndices_consistencyLevel :: Lens.Lens' ListAttachedIndices (Core.Maybe ConsistencyLevel)
listAttachedIndices_consistencyLevel = Lens.lens (\ListAttachedIndices' {consistencyLevel} -> consistencyLevel) (\s@ListAttachedIndices' {} a -> s {consistencyLevel = a} :: ListAttachedIndices)

-- | The ARN of the directory.
listAttachedIndices_directoryArn :: Lens.Lens' ListAttachedIndices Core.Text
listAttachedIndices_directoryArn = Lens.lens (\ListAttachedIndices' {directoryArn} -> directoryArn) (\s@ListAttachedIndices' {} a -> s {directoryArn = a} :: ListAttachedIndices)

-- | A reference to the object that has indices attached.
listAttachedIndices_targetReference :: Lens.Lens' ListAttachedIndices ObjectReference
listAttachedIndices_targetReference = Lens.lens (\ListAttachedIndices' {targetReference} -> targetReference) (\s@ListAttachedIndices' {} a -> s {targetReference = a} :: ListAttachedIndices)

instance Core.AWSPager ListAttachedIndices where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listAttachedIndicesResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listAttachedIndicesResponse_indexAttachments
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listAttachedIndices_nextToken
          Lens..~ rs
          Lens.^? listAttachedIndicesResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest ListAttachedIndices where
  type
    AWSResponse ListAttachedIndices =
      ListAttachedIndicesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAttachedIndicesResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "IndexAttachments" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListAttachedIndices

instance Core.NFData ListAttachedIndices

instance Core.ToHeaders ListAttachedIndices where
  toHeaders ListAttachedIndices' {..} =
    Core.mconcat
      [ "x-amz-consistency-level" Core.=# consistencyLevel,
        "x-amz-data-partition" Core.=# directoryArn
      ]

instance Core.ToJSON ListAttachedIndices where
  toJSON ListAttachedIndices' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults,
            Core.Just
              ("TargetReference" Core..= targetReference)
          ]
      )

instance Core.ToPath ListAttachedIndices where
  toPath =
    Core.const
      "/amazonclouddirectory/2017-01-11/object/indices"

instance Core.ToQuery ListAttachedIndices where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListAttachedIndicesResponse' smart constructor.
data ListAttachedIndicesResponse = ListAttachedIndicesResponse'
  { -- | The pagination token.
    nextToken :: Core.Maybe Core.Text,
    -- | The indices attached to the specified object.
    indexAttachments :: Core.Maybe [IndexAttachment],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListAttachedIndicesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listAttachedIndicesResponse_nextToken' - The pagination token.
--
-- 'indexAttachments', 'listAttachedIndicesResponse_indexAttachments' - The indices attached to the specified object.
--
-- 'httpStatus', 'listAttachedIndicesResponse_httpStatus' - The response's http status code.
newListAttachedIndicesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListAttachedIndicesResponse
newListAttachedIndicesResponse pHttpStatus_ =
  ListAttachedIndicesResponse'
    { nextToken =
        Core.Nothing,
      indexAttachments = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The pagination token.
listAttachedIndicesResponse_nextToken :: Lens.Lens' ListAttachedIndicesResponse (Core.Maybe Core.Text)
listAttachedIndicesResponse_nextToken = Lens.lens (\ListAttachedIndicesResponse' {nextToken} -> nextToken) (\s@ListAttachedIndicesResponse' {} a -> s {nextToken = a} :: ListAttachedIndicesResponse)

-- | The indices attached to the specified object.
listAttachedIndicesResponse_indexAttachments :: Lens.Lens' ListAttachedIndicesResponse (Core.Maybe [IndexAttachment])
listAttachedIndicesResponse_indexAttachments = Lens.lens (\ListAttachedIndicesResponse' {indexAttachments} -> indexAttachments) (\s@ListAttachedIndicesResponse' {} a -> s {indexAttachments = a} :: ListAttachedIndicesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listAttachedIndicesResponse_httpStatus :: Lens.Lens' ListAttachedIndicesResponse Core.Int
listAttachedIndicesResponse_httpStatus = Lens.lens (\ListAttachedIndicesResponse' {httpStatus} -> httpStatus) (\s@ListAttachedIndicesResponse' {} a -> s {httpStatus = a} :: ListAttachedIndicesResponse)

instance Core.NFData ListAttachedIndicesResponse
