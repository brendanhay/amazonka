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
-- Module      : Amazonka.Kendra.DeleteFaq
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes an FAQ from an index.
module Amazonka.Kendra.DeleteFaq
  ( -- * Creating a Request
    DeleteFaq (..),
    newDeleteFaq,

    -- * Request Lenses
    deleteFaq_id,
    deleteFaq_indexId,

    -- * Destructuring the Response
    DeleteFaqResponse (..),
    newDeleteFaqResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kendra.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteFaq' smart constructor.
data DeleteFaq = DeleteFaq'
  { -- | The identifier of the FAQ you want to remove.
    id :: Prelude.Text,
    -- | The identifier of the index for the FAQ.
    indexId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteFaq' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'deleteFaq_id' - The identifier of the FAQ you want to remove.
--
-- 'indexId', 'deleteFaq_indexId' - The identifier of the index for the FAQ.
newDeleteFaq ::
  -- | 'id'
  Prelude.Text ->
  -- | 'indexId'
  Prelude.Text ->
  DeleteFaq
newDeleteFaq pId_ pIndexId_ =
  DeleteFaq' {id = pId_, indexId = pIndexId_}

-- | The identifier of the FAQ you want to remove.
deleteFaq_id :: Lens.Lens' DeleteFaq Prelude.Text
deleteFaq_id = Lens.lens (\DeleteFaq' {id} -> id) (\s@DeleteFaq' {} a -> s {id = a} :: DeleteFaq)

-- | The identifier of the index for the FAQ.
deleteFaq_indexId :: Lens.Lens' DeleteFaq Prelude.Text
deleteFaq_indexId = Lens.lens (\DeleteFaq' {indexId} -> indexId) (\s@DeleteFaq' {} a -> s {indexId = a} :: DeleteFaq)

instance Core.AWSRequest DeleteFaq where
  type AWSResponse DeleteFaq = DeleteFaqResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response = Response.receiveNull DeleteFaqResponse'

instance Prelude.Hashable DeleteFaq where
  hashWithSalt _salt DeleteFaq' {..} =
    _salt
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` indexId

instance Prelude.NFData DeleteFaq where
  rnf DeleteFaq' {..} =
    Prelude.rnf id `Prelude.seq` Prelude.rnf indexId

instance Data.ToHeaders DeleteFaq where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSKendraFrontendService.DeleteFaq" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteFaq where
  toJSON DeleteFaq' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Id" Data..= id),
            Prelude.Just ("IndexId" Data..= indexId)
          ]
      )

instance Data.ToPath DeleteFaq where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteFaq where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteFaqResponse' smart constructor.
data DeleteFaqResponse = DeleteFaqResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteFaqResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteFaqResponse ::
  DeleteFaqResponse
newDeleteFaqResponse = DeleteFaqResponse'

instance Prelude.NFData DeleteFaqResponse where
  rnf _ = ()
