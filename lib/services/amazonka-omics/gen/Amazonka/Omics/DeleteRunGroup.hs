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
-- Module      : Amazonka.Omics.DeleteRunGroup
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a workflow run group.
module Amazonka.Omics.DeleteRunGroup
  ( -- * Creating a Request
    DeleteRunGroup (..),
    newDeleteRunGroup,

    -- * Request Lenses
    deleteRunGroup_id,

    -- * Destructuring the Response
    DeleteRunGroupResponse (..),
    newDeleteRunGroupResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Omics.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteRunGroup' smart constructor.
data DeleteRunGroup = DeleteRunGroup'
  { -- | The run group\'s ID.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteRunGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'deleteRunGroup_id' - The run group\'s ID.
newDeleteRunGroup ::
  -- | 'id'
  Prelude.Text ->
  DeleteRunGroup
newDeleteRunGroup pId_ = DeleteRunGroup' {id = pId_}

-- | The run group\'s ID.
deleteRunGroup_id :: Lens.Lens' DeleteRunGroup Prelude.Text
deleteRunGroup_id = Lens.lens (\DeleteRunGroup' {id} -> id) (\s@DeleteRunGroup' {} a -> s {id = a} :: DeleteRunGroup)

instance Core.AWSRequest DeleteRunGroup where
  type
    AWSResponse DeleteRunGroup =
      DeleteRunGroupResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull DeleteRunGroupResponse'

instance Prelude.Hashable DeleteRunGroup where
  hashWithSalt _salt DeleteRunGroup' {..} =
    _salt `Prelude.hashWithSalt` id

instance Prelude.NFData DeleteRunGroup where
  rnf DeleteRunGroup' {..} = Prelude.rnf id

instance Data.ToHeaders DeleteRunGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteRunGroup where
  toPath DeleteRunGroup' {..} =
    Prelude.mconcat ["/runGroup/", Data.toBS id]

instance Data.ToQuery DeleteRunGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteRunGroupResponse' smart constructor.
data DeleteRunGroupResponse = DeleteRunGroupResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteRunGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteRunGroupResponse ::
  DeleteRunGroupResponse
newDeleteRunGroupResponse = DeleteRunGroupResponse'

instance Prelude.NFData DeleteRunGroupResponse where
  rnf _ = ()
