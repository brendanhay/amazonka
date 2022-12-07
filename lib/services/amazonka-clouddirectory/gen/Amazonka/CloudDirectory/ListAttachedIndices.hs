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
-- Module      : Amazonka.CloudDirectory.ListAttachedIndices
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists indices attached to the specified object.
--
-- This operation returns paginated results.
module Amazonka.CloudDirectory.ListAttachedIndices
  ( -- * Creating a Request
    ListAttachedIndices (..),
    newListAttachedIndices,

    -- * Request Lenses
    listAttachedIndices_nextToken,
    listAttachedIndices_consistencyLevel,
    listAttachedIndices_maxResults,
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

import Amazonka.CloudDirectory.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListAttachedIndices' smart constructor.
data ListAttachedIndices = ListAttachedIndices'
  { -- | The pagination token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The consistency level to use for this operation.
    consistencyLevel :: Prelude.Maybe ConsistencyLevel,
    -- | The maximum number of results to retrieve.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The ARN of the directory.
    directoryArn :: Prelude.Text,
    -- | A reference to the object that has indices attached.
    targetReference :: ObjectReference
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'consistencyLevel', 'listAttachedIndices_consistencyLevel' - The consistency level to use for this operation.
--
-- 'maxResults', 'listAttachedIndices_maxResults' - The maximum number of results to retrieve.
--
-- 'directoryArn', 'listAttachedIndices_directoryArn' - The ARN of the directory.
--
-- 'targetReference', 'listAttachedIndices_targetReference' - A reference to the object that has indices attached.
newListAttachedIndices ::
  -- | 'directoryArn'
  Prelude.Text ->
  -- | 'targetReference'
  ObjectReference ->
  ListAttachedIndices
newListAttachedIndices
  pDirectoryArn_
  pTargetReference_ =
    ListAttachedIndices'
      { nextToken = Prelude.Nothing,
        consistencyLevel = Prelude.Nothing,
        maxResults = Prelude.Nothing,
        directoryArn = pDirectoryArn_,
        targetReference = pTargetReference_
      }

-- | The pagination token.
listAttachedIndices_nextToken :: Lens.Lens' ListAttachedIndices (Prelude.Maybe Prelude.Text)
listAttachedIndices_nextToken = Lens.lens (\ListAttachedIndices' {nextToken} -> nextToken) (\s@ListAttachedIndices' {} a -> s {nextToken = a} :: ListAttachedIndices)

-- | The consistency level to use for this operation.
listAttachedIndices_consistencyLevel :: Lens.Lens' ListAttachedIndices (Prelude.Maybe ConsistencyLevel)
listAttachedIndices_consistencyLevel = Lens.lens (\ListAttachedIndices' {consistencyLevel} -> consistencyLevel) (\s@ListAttachedIndices' {} a -> s {consistencyLevel = a} :: ListAttachedIndices)

-- | The maximum number of results to retrieve.
listAttachedIndices_maxResults :: Lens.Lens' ListAttachedIndices (Prelude.Maybe Prelude.Natural)
listAttachedIndices_maxResults = Lens.lens (\ListAttachedIndices' {maxResults} -> maxResults) (\s@ListAttachedIndices' {} a -> s {maxResults = a} :: ListAttachedIndices)

-- | The ARN of the directory.
listAttachedIndices_directoryArn :: Lens.Lens' ListAttachedIndices Prelude.Text
listAttachedIndices_directoryArn = Lens.lens (\ListAttachedIndices' {directoryArn} -> directoryArn) (\s@ListAttachedIndices' {} a -> s {directoryArn = a} :: ListAttachedIndices)

-- | A reference to the object that has indices attached.
listAttachedIndices_targetReference :: Lens.Lens' ListAttachedIndices ObjectReference
listAttachedIndices_targetReference = Lens.lens (\ListAttachedIndices' {targetReference} -> targetReference) (\s@ListAttachedIndices' {} a -> s {targetReference = a} :: ListAttachedIndices)

instance Core.AWSPager ListAttachedIndices where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listAttachedIndicesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listAttachedIndicesResponse_indexAttachments
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listAttachedIndices_nextToken
          Lens..~ rs
          Lens.^? listAttachedIndicesResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListAttachedIndices where
  type
    AWSResponse ListAttachedIndices =
      ListAttachedIndicesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAttachedIndicesResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> ( x Data..?> "IndexAttachments"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListAttachedIndices where
  hashWithSalt _salt ListAttachedIndices' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` consistencyLevel
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` directoryArn
      `Prelude.hashWithSalt` targetReference

instance Prelude.NFData ListAttachedIndices where
  rnf ListAttachedIndices' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf consistencyLevel
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf directoryArn
      `Prelude.seq` Prelude.rnf targetReference

instance Data.ToHeaders ListAttachedIndices where
  toHeaders ListAttachedIndices' {..} =
    Prelude.mconcat
      [ "x-amz-consistency-level" Data.=# consistencyLevel,
        "x-amz-data-partition" Data.=# directoryArn
      ]

instance Data.ToJSON ListAttachedIndices where
  toJSON ListAttachedIndices' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NextToken" Data..=) Prelude.<$> nextToken,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            Prelude.Just
              ("TargetReference" Data..= targetReference)
          ]
      )

instance Data.ToPath ListAttachedIndices where
  toPath =
    Prelude.const
      "/amazonclouddirectory/2017-01-11/object/indices"

instance Data.ToQuery ListAttachedIndices where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListAttachedIndicesResponse' smart constructor.
data ListAttachedIndicesResponse = ListAttachedIndicesResponse'
  { -- | The pagination token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The indices attached to the specified object.
    indexAttachments :: Prelude.Maybe [IndexAttachment],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  ListAttachedIndicesResponse
newListAttachedIndicesResponse pHttpStatus_ =
  ListAttachedIndicesResponse'
    { nextToken =
        Prelude.Nothing,
      indexAttachments = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The pagination token.
listAttachedIndicesResponse_nextToken :: Lens.Lens' ListAttachedIndicesResponse (Prelude.Maybe Prelude.Text)
listAttachedIndicesResponse_nextToken = Lens.lens (\ListAttachedIndicesResponse' {nextToken} -> nextToken) (\s@ListAttachedIndicesResponse' {} a -> s {nextToken = a} :: ListAttachedIndicesResponse)

-- | The indices attached to the specified object.
listAttachedIndicesResponse_indexAttachments :: Lens.Lens' ListAttachedIndicesResponse (Prelude.Maybe [IndexAttachment])
listAttachedIndicesResponse_indexAttachments = Lens.lens (\ListAttachedIndicesResponse' {indexAttachments} -> indexAttachments) (\s@ListAttachedIndicesResponse' {} a -> s {indexAttachments = a} :: ListAttachedIndicesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listAttachedIndicesResponse_httpStatus :: Lens.Lens' ListAttachedIndicesResponse Prelude.Int
listAttachedIndicesResponse_httpStatus = Lens.lens (\ListAttachedIndicesResponse' {httpStatus} -> httpStatus) (\s@ListAttachedIndicesResponse' {} a -> s {httpStatus = a} :: ListAttachedIndicesResponse)

instance Prelude.NFData ListAttachedIndicesResponse where
  rnf ListAttachedIndicesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf indexAttachments
      `Prelude.seq` Prelude.rnf httpStatus
