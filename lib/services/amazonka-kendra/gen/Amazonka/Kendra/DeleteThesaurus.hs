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
-- Module      : Amazonka.Kendra.DeleteThesaurus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an existing Amazon Kendra thesaurus.
module Amazonka.Kendra.DeleteThesaurus
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kendra.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteThesaurus' smart constructor.
data DeleteThesaurus = DeleteThesaurus'
  { -- | The identifier of the thesaurus you want to delete.
    id :: Prelude.Text,
    -- | The identifier of the index for the thesaurus.
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
-- 'id', 'deleteThesaurus_id' - The identifier of the thesaurus you want to delete.
--
-- 'indexId', 'deleteThesaurus_indexId' - The identifier of the index for the thesaurus.
newDeleteThesaurus ::
  -- | 'id'
  Prelude.Text ->
  -- | 'indexId'
  Prelude.Text ->
  DeleteThesaurus
newDeleteThesaurus pId_ pIndexId_ =
  DeleteThesaurus' {id = pId_, indexId = pIndexId_}

-- | The identifier of the thesaurus you want to delete.
deleteThesaurus_id :: Lens.Lens' DeleteThesaurus Prelude.Text
deleteThesaurus_id = Lens.lens (\DeleteThesaurus' {id} -> id) (\s@DeleteThesaurus' {} a -> s {id = a} :: DeleteThesaurus)

-- | The identifier of the index for the thesaurus.
deleteThesaurus_indexId :: Lens.Lens' DeleteThesaurus Prelude.Text
deleteThesaurus_indexId = Lens.lens (\DeleteThesaurus' {indexId} -> indexId) (\s@DeleteThesaurus' {} a -> s {indexId = a} :: DeleteThesaurus)

instance Core.AWSRequest DeleteThesaurus where
  type
    AWSResponse DeleteThesaurus =
      DeleteThesaurusResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull DeleteThesaurusResponse'

instance Prelude.Hashable DeleteThesaurus where
  hashWithSalt _salt DeleteThesaurus' {..} =
    _salt
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` indexId

instance Prelude.NFData DeleteThesaurus where
  rnf DeleteThesaurus' {..} =
    Prelude.rnf id `Prelude.seq` Prelude.rnf indexId

instance Data.ToHeaders DeleteThesaurus where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSKendraFrontendService.DeleteThesaurus" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteThesaurus where
  toJSON DeleteThesaurus' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Id" Data..= id),
            Prelude.Just ("IndexId" Data..= indexId)
          ]
      )

instance Data.ToPath DeleteThesaurus where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteThesaurus where
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

instance Prelude.NFData DeleteThesaurusResponse where
  rnf _ = ()
