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
-- Module      : Amazonka.Omics.DeleteRun
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a workflow run.
module Amazonka.Omics.DeleteRun
  ( -- * Creating a Request
    DeleteRun (..),
    newDeleteRun,

    -- * Request Lenses
    deleteRun_id,

    -- * Destructuring the Response
    DeleteRunResponse (..),
    newDeleteRunResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Omics.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteRun' smart constructor.
data DeleteRun = DeleteRun'
  { -- | The run\'s ID.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteRun' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'deleteRun_id' - The run\'s ID.
newDeleteRun ::
  -- | 'id'
  Prelude.Text ->
  DeleteRun
newDeleteRun pId_ = DeleteRun' {id = pId_}

-- | The run\'s ID.
deleteRun_id :: Lens.Lens' DeleteRun Prelude.Text
deleteRun_id = Lens.lens (\DeleteRun' {id} -> id) (\s@DeleteRun' {} a -> s {id = a} :: DeleteRun)

instance Core.AWSRequest DeleteRun where
  type AWSResponse DeleteRun = DeleteRunResponse
  request overrides =
    Request.delete (overrides defaultService)
  response = Response.receiveNull DeleteRunResponse'

instance Prelude.Hashable DeleteRun where
  hashWithSalt _salt DeleteRun' {..} =
    _salt `Prelude.hashWithSalt` id

instance Prelude.NFData DeleteRun where
  rnf DeleteRun' {..} = Prelude.rnf id

instance Data.ToHeaders DeleteRun where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteRun where
  toPath DeleteRun' {..} =
    Prelude.mconcat ["/run/", Data.toBS id]

instance Data.ToQuery DeleteRun where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteRunResponse' smart constructor.
data DeleteRunResponse = DeleteRunResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteRunResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteRunResponse ::
  DeleteRunResponse
newDeleteRunResponse = DeleteRunResponse'

instance Prelude.NFData DeleteRunResponse where
  rnf _ = ()
