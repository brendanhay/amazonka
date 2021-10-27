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
-- Module      : Network.AWS.Kendra.DeleteQuerySuggestionsBlockList
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a block list used for query suggestions for an index.
--
-- A deleted block list might not take effect right away. Amazon Kendra
-- needs to refresh the entire suggestions list to add back the queries
-- that were previously blocked.
module Network.AWS.Kendra.DeleteQuerySuggestionsBlockList
  ( -- * Creating a Request
    DeleteQuerySuggestionsBlockList (..),
    newDeleteQuerySuggestionsBlockList,

    -- * Request Lenses
    deleteQuerySuggestionsBlockList_indexId,
    deleteQuerySuggestionsBlockList_id,

    -- * Destructuring the Response
    DeleteQuerySuggestionsBlockListResponse (..),
    newDeleteQuerySuggestionsBlockListResponse,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Kendra.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteQuerySuggestionsBlockList' smart constructor.
data DeleteQuerySuggestionsBlockList = DeleteQuerySuggestionsBlockList'
  { -- | The identifier of the you want to delete a block list from.
    indexId :: Prelude.Text,
    -- | The unique identifier of the block list that needs to be deleted.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteQuerySuggestionsBlockList' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'indexId', 'deleteQuerySuggestionsBlockList_indexId' - The identifier of the you want to delete a block list from.
--
-- 'id', 'deleteQuerySuggestionsBlockList_id' - The unique identifier of the block list that needs to be deleted.
newDeleteQuerySuggestionsBlockList ::
  -- | 'indexId'
  Prelude.Text ->
  -- | 'id'
  Prelude.Text ->
  DeleteQuerySuggestionsBlockList
newDeleteQuerySuggestionsBlockList pIndexId_ pId_ =
  DeleteQuerySuggestionsBlockList'
    { indexId =
        pIndexId_,
      id = pId_
    }

-- | The identifier of the you want to delete a block list from.
deleteQuerySuggestionsBlockList_indexId :: Lens.Lens' DeleteQuerySuggestionsBlockList Prelude.Text
deleteQuerySuggestionsBlockList_indexId = Lens.lens (\DeleteQuerySuggestionsBlockList' {indexId} -> indexId) (\s@DeleteQuerySuggestionsBlockList' {} a -> s {indexId = a} :: DeleteQuerySuggestionsBlockList)

-- | The unique identifier of the block list that needs to be deleted.
deleteQuerySuggestionsBlockList_id :: Lens.Lens' DeleteQuerySuggestionsBlockList Prelude.Text
deleteQuerySuggestionsBlockList_id = Lens.lens (\DeleteQuerySuggestionsBlockList' {id} -> id) (\s@DeleteQuerySuggestionsBlockList' {} a -> s {id = a} :: DeleteQuerySuggestionsBlockList)

instance
  Core.AWSRequest
    DeleteQuerySuggestionsBlockList
  where
  type
    AWSResponse DeleteQuerySuggestionsBlockList =
      DeleteQuerySuggestionsBlockListResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull
      DeleteQuerySuggestionsBlockListResponse'

instance
  Prelude.Hashable
    DeleteQuerySuggestionsBlockList

instance
  Prelude.NFData
    DeleteQuerySuggestionsBlockList

instance
  Core.ToHeaders
    DeleteQuerySuggestionsBlockList
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSKendraFrontendService.DeleteQuerySuggestionsBlockList" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DeleteQuerySuggestionsBlockList where
  toJSON DeleteQuerySuggestionsBlockList' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("IndexId" Core..= indexId),
            Prelude.Just ("Id" Core..= id)
          ]
      )

instance Core.ToPath DeleteQuerySuggestionsBlockList where
  toPath = Prelude.const "/"

instance Core.ToQuery DeleteQuerySuggestionsBlockList where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteQuerySuggestionsBlockListResponse' smart constructor.
data DeleteQuerySuggestionsBlockListResponse = DeleteQuerySuggestionsBlockListResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteQuerySuggestionsBlockListResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteQuerySuggestionsBlockListResponse ::
  DeleteQuerySuggestionsBlockListResponse
newDeleteQuerySuggestionsBlockListResponse =
  DeleteQuerySuggestionsBlockListResponse'

instance
  Prelude.NFData
    DeleteQuerySuggestionsBlockListResponse
