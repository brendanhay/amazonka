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
-- Module      : Network.AWS.Kendra.DeleteThesaurus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an existing Amazon Kendra thesaurus.
module Network.AWS.Kendra.DeleteThesaurus
  ( -- * Creating a Request
    DeleteThesaurus (..),
    newDeleteThesaurus,

    -- * Request Lenses
    deleteThesaurus_id,
    deleteThesaurus_indexId,

    -- * Destructuring the Response
    DeleteThesaurusResponse (..),
    newDeleteThesaurusResponse,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Kendra.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteThesaurus' smart constructor.
data DeleteThesaurus = DeleteThesaurus'
  { -- | The identifier of the thesaurus to delete.
    id :: Prelude.Text,
    -- | The identifier of the index associated with the thesaurus to delete.
    indexId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteThesaurus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'deleteThesaurus_id' - The identifier of the thesaurus to delete.
--
-- 'indexId', 'deleteThesaurus_indexId' - The identifier of the index associated with the thesaurus to delete.
newDeleteThesaurus ::
  -- | 'id'
  Prelude.Text ->
  -- | 'indexId'
  Prelude.Text ->
  DeleteThesaurus
newDeleteThesaurus pId_ pIndexId_ =
  DeleteThesaurus' {id = pId_, indexId = pIndexId_}

-- | The identifier of the thesaurus to delete.
deleteThesaurus_id :: Lens.Lens' DeleteThesaurus Prelude.Text
deleteThesaurus_id = Lens.lens (\DeleteThesaurus' {id} -> id) (\s@DeleteThesaurus' {} a -> s {id = a} :: DeleteThesaurus)

-- | The identifier of the index associated with the thesaurus to delete.
deleteThesaurus_indexId :: Lens.Lens' DeleteThesaurus Prelude.Text
deleteThesaurus_indexId = Lens.lens (\DeleteThesaurus' {indexId} -> indexId) (\s@DeleteThesaurus' {} a -> s {indexId = a} :: DeleteThesaurus)

instance Core.AWSRequest DeleteThesaurus where
  type
    AWSResponse DeleteThesaurus =
      DeleteThesaurusResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull DeleteThesaurusResponse'

instance Prelude.Hashable DeleteThesaurus

instance Prelude.NFData DeleteThesaurus

instance Core.ToHeaders DeleteThesaurus where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSKendraFrontendService.DeleteThesaurus" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DeleteThesaurus where
  toJSON DeleteThesaurus' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Id" Core..= id),
            Prelude.Just ("IndexId" Core..= indexId)
          ]
      )

instance Core.ToPath DeleteThesaurus where
  toPath = Prelude.const "/"

instance Core.ToQuery DeleteThesaurus where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteThesaurusResponse' smart constructor.
data DeleteThesaurusResponse = DeleteThesaurusResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteThesaurusResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteThesaurusResponse ::
  DeleteThesaurusResponse
newDeleteThesaurusResponse = DeleteThesaurusResponse'

instance Prelude.NFData DeleteThesaurusResponse
