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
-- Module      : Network.AWS.CloudDirectory.ListIndex
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists objects attached to the specified index.
--
-- This operation returns paginated results.
module Network.AWS.CloudDirectory.ListIndex
  ( -- * Creating a Request
    ListIndex (..),
    newListIndex,

    -- * Request Lenses
    listIndex_nextToken,
    listIndex_maxResults,
    listIndex_consistencyLevel,
    listIndex_rangesOnIndexedValues,
    listIndex_directoryArn,
    listIndex_indexReference,

    -- * Destructuring the Response
    ListIndexResponse (..),
    newListIndexResponse,

    -- * Response Lenses
    listIndexResponse_nextToken,
    listIndexResponse_indexAttachments,
    listIndexResponse_httpStatus,
  )
where

import Network.AWS.CloudDirectory.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListIndex' smart constructor.
data ListIndex = ListIndex'
  { -- | The pagination token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of objects in a single page to retrieve from the
    -- index during a request. For more information, see
    -- <http://docs.aws.amazon.com/clouddirectory/latest/developerguide/limits.html Amazon Cloud Directory Limits>.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The consistency level to execute the request at.
    consistencyLevel :: Prelude.Maybe ConsistencyLevel,
    -- | Specifies the ranges of indexed values that you want to query.
    rangesOnIndexedValues :: Prelude.Maybe [ObjectAttributeRange],
    -- | The ARN of the directory that the index exists in.
    directoryArn :: Prelude.Text,
    -- | The reference to the index to list.
    indexReference :: ObjectReference
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListIndex' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listIndex_nextToken' - The pagination token.
--
-- 'maxResults', 'listIndex_maxResults' - The maximum number of objects in a single page to retrieve from the
-- index during a request. For more information, see
-- <http://docs.aws.amazon.com/clouddirectory/latest/developerguide/limits.html Amazon Cloud Directory Limits>.
--
-- 'consistencyLevel', 'listIndex_consistencyLevel' - The consistency level to execute the request at.
--
-- 'rangesOnIndexedValues', 'listIndex_rangesOnIndexedValues' - Specifies the ranges of indexed values that you want to query.
--
-- 'directoryArn', 'listIndex_directoryArn' - The ARN of the directory that the index exists in.
--
-- 'indexReference', 'listIndex_indexReference' - The reference to the index to list.
newListIndex ::
  -- | 'directoryArn'
  Prelude.Text ->
  -- | 'indexReference'
  ObjectReference ->
  ListIndex
newListIndex pDirectoryArn_ pIndexReference_ =
  ListIndex'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      consistencyLevel = Prelude.Nothing,
      rangesOnIndexedValues = Prelude.Nothing,
      directoryArn = pDirectoryArn_,
      indexReference = pIndexReference_
    }

-- | The pagination token.
listIndex_nextToken :: Lens.Lens' ListIndex (Prelude.Maybe Prelude.Text)
listIndex_nextToken = Lens.lens (\ListIndex' {nextToken} -> nextToken) (\s@ListIndex' {} a -> s {nextToken = a} :: ListIndex)

-- | The maximum number of objects in a single page to retrieve from the
-- index during a request. For more information, see
-- <http://docs.aws.amazon.com/clouddirectory/latest/developerguide/limits.html Amazon Cloud Directory Limits>.
listIndex_maxResults :: Lens.Lens' ListIndex (Prelude.Maybe Prelude.Natural)
listIndex_maxResults = Lens.lens (\ListIndex' {maxResults} -> maxResults) (\s@ListIndex' {} a -> s {maxResults = a} :: ListIndex)

-- | The consistency level to execute the request at.
listIndex_consistencyLevel :: Lens.Lens' ListIndex (Prelude.Maybe ConsistencyLevel)
listIndex_consistencyLevel = Lens.lens (\ListIndex' {consistencyLevel} -> consistencyLevel) (\s@ListIndex' {} a -> s {consistencyLevel = a} :: ListIndex)

-- | Specifies the ranges of indexed values that you want to query.
listIndex_rangesOnIndexedValues :: Lens.Lens' ListIndex (Prelude.Maybe [ObjectAttributeRange])
listIndex_rangesOnIndexedValues = Lens.lens (\ListIndex' {rangesOnIndexedValues} -> rangesOnIndexedValues) (\s@ListIndex' {} a -> s {rangesOnIndexedValues = a} :: ListIndex) Prelude.. Lens.mapping Lens._Coerce

-- | The ARN of the directory that the index exists in.
listIndex_directoryArn :: Lens.Lens' ListIndex Prelude.Text
listIndex_directoryArn = Lens.lens (\ListIndex' {directoryArn} -> directoryArn) (\s@ListIndex' {} a -> s {directoryArn = a} :: ListIndex)

-- | The reference to the index to list.
listIndex_indexReference :: Lens.Lens' ListIndex ObjectReference
listIndex_indexReference = Lens.lens (\ListIndex' {indexReference} -> indexReference) (\s@ListIndex' {} a -> s {indexReference = a} :: ListIndex)

instance Core.AWSPager ListIndex where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listIndexResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listIndexResponse_indexAttachments
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listIndex_nextToken
          Lens..~ rs
          Lens.^? listIndexResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListIndex where
  type AWSResponse ListIndex = ListIndexResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListIndexResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> ( x Core..?> "IndexAttachments"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListIndex

instance Prelude.NFData ListIndex

instance Core.ToHeaders ListIndex where
  toHeaders ListIndex' {..} =
    Prelude.mconcat
      [ "x-amz-consistency-level" Core.=# consistencyLevel,
        "x-amz-data-partition" Core.=# directoryArn
      ]

instance Core.ToJSON ListIndex where
  toJSON ListIndex' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            ("RangesOnIndexedValues" Core..=)
              Prelude.<$> rangesOnIndexedValues,
            Prelude.Just
              ("IndexReference" Core..= indexReference)
          ]
      )

instance Core.ToPath ListIndex where
  toPath =
    Prelude.const
      "/amazonclouddirectory/2017-01-11/index/targets"

instance Core.ToQuery ListIndex where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListIndexResponse' smart constructor.
data ListIndexResponse = ListIndexResponse'
  { -- | The pagination token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The objects and indexed values attached to the index.
    indexAttachments :: Prelude.Maybe [IndexAttachment],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListIndexResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listIndexResponse_nextToken' - The pagination token.
--
-- 'indexAttachments', 'listIndexResponse_indexAttachments' - The objects and indexed values attached to the index.
--
-- 'httpStatus', 'listIndexResponse_httpStatus' - The response's http status code.
newListIndexResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListIndexResponse
newListIndexResponse pHttpStatus_ =
  ListIndexResponse'
    { nextToken = Prelude.Nothing,
      indexAttachments = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The pagination token.
listIndexResponse_nextToken :: Lens.Lens' ListIndexResponse (Prelude.Maybe Prelude.Text)
listIndexResponse_nextToken = Lens.lens (\ListIndexResponse' {nextToken} -> nextToken) (\s@ListIndexResponse' {} a -> s {nextToken = a} :: ListIndexResponse)

-- | The objects and indexed values attached to the index.
listIndexResponse_indexAttachments :: Lens.Lens' ListIndexResponse (Prelude.Maybe [IndexAttachment])
listIndexResponse_indexAttachments = Lens.lens (\ListIndexResponse' {indexAttachments} -> indexAttachments) (\s@ListIndexResponse' {} a -> s {indexAttachments = a} :: ListIndexResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listIndexResponse_httpStatus :: Lens.Lens' ListIndexResponse Prelude.Int
listIndexResponse_httpStatus = Lens.lens (\ListIndexResponse' {httpStatus} -> httpStatus) (\s@ListIndexResponse' {} a -> s {httpStatus = a} :: ListIndexResponse)

instance Prelude.NFData ListIndexResponse
