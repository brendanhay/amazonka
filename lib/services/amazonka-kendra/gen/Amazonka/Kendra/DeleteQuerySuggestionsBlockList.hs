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
-- Module      : Amazonka.Kendra.DeleteQuerySuggestionsBlockList
-- Copyright   : (c) 2013-2023 Brendan Hay
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
--
-- @DeleteQuerySuggestionsBlockList@ is currently not supported in the
-- Amazon Web Services GovCloud (US-West) region.
module Amazonka.Kendra.DeleteQuerySuggestionsBlockList
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kendra.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteQuerySuggestionsBlockList' smart constructor.
data DeleteQuerySuggestionsBlockList = DeleteQuerySuggestionsBlockList'
  { -- | The identifier of the index for the block list.
    indexId :: Prelude.Text,
    -- | The identifier of the block list you want to delete.
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
-- 'indexId', 'deleteQuerySuggestionsBlockList_indexId' - The identifier of the index for the block list.
--
-- 'id', 'deleteQuerySuggestionsBlockList_id' - The identifier of the block list you want to delete.
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

-- | The identifier of the index for the block list.
deleteQuerySuggestionsBlockList_indexId :: Lens.Lens' DeleteQuerySuggestionsBlockList Prelude.Text
deleteQuerySuggestionsBlockList_indexId = Lens.lens (\DeleteQuerySuggestionsBlockList' {indexId} -> indexId) (\s@DeleteQuerySuggestionsBlockList' {} a -> s {indexId = a} :: DeleteQuerySuggestionsBlockList)

-- | The identifier of the block list you want to delete.
deleteQuerySuggestionsBlockList_id :: Lens.Lens' DeleteQuerySuggestionsBlockList Prelude.Text
deleteQuerySuggestionsBlockList_id = Lens.lens (\DeleteQuerySuggestionsBlockList' {id} -> id) (\s@DeleteQuerySuggestionsBlockList' {} a -> s {id = a} :: DeleteQuerySuggestionsBlockList)

instance
  Core.AWSRequest
    DeleteQuerySuggestionsBlockList
  where
  type
    AWSResponse DeleteQuerySuggestionsBlockList =
      DeleteQuerySuggestionsBlockListResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull
      DeleteQuerySuggestionsBlockListResponse'

instance
  Prelude.Hashable
    DeleteQuerySuggestionsBlockList
  where
  hashWithSalt
    _salt
    DeleteQuerySuggestionsBlockList' {..} =
      _salt `Prelude.hashWithSalt` indexId
        `Prelude.hashWithSalt` id

instance
  Prelude.NFData
    DeleteQuerySuggestionsBlockList
  where
  rnf DeleteQuerySuggestionsBlockList' {..} =
    Prelude.rnf indexId `Prelude.seq` Prelude.rnf id

instance
  Data.ToHeaders
    DeleteQuerySuggestionsBlockList
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSKendraFrontendService.DeleteQuerySuggestionsBlockList" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteQuerySuggestionsBlockList where
  toJSON DeleteQuerySuggestionsBlockList' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("IndexId" Data..= indexId),
            Prelude.Just ("Id" Data..= id)
          ]
      )

instance Data.ToPath DeleteQuerySuggestionsBlockList where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteQuerySuggestionsBlockList where
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
  where
  rnf _ = ()
