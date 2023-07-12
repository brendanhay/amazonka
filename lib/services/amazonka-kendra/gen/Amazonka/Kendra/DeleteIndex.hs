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
-- Module      : Amazonka.Kendra.DeleteIndex
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an existing Amazon Kendra index. An exception is not thrown if
-- the index is already being deleted. While the index is being deleted,
-- the @Status@ field returned by a call to the @DescribeIndex@ API is set
-- to @DELETING@.
module Amazonka.Kendra.DeleteIndex
  ( -- * Creating a Request
    DeleteIndex (..),
    newDeleteIndex,

    -- * Request Lenses
    deleteIndex_id,

    -- * Destructuring the Response
    DeleteIndexResponse (..),
    newDeleteIndexResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kendra.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteIndex' smart constructor.
data DeleteIndex = DeleteIndex'
  { -- | The identifier of the index you want to delete.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteIndex' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'deleteIndex_id' - The identifier of the index you want to delete.
newDeleteIndex ::
  -- | 'id'
  Prelude.Text ->
  DeleteIndex
newDeleteIndex pId_ = DeleteIndex' {id = pId_}

-- | The identifier of the index you want to delete.
deleteIndex_id :: Lens.Lens' DeleteIndex Prelude.Text
deleteIndex_id = Lens.lens (\DeleteIndex' {id} -> id) (\s@DeleteIndex' {} a -> s {id = a} :: DeleteIndex)

instance Core.AWSRequest DeleteIndex where
  type AWSResponse DeleteIndex = DeleteIndexResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response = Response.receiveNull DeleteIndexResponse'

instance Prelude.Hashable DeleteIndex where
  hashWithSalt _salt DeleteIndex' {..} =
    _salt `Prelude.hashWithSalt` id

instance Prelude.NFData DeleteIndex where
  rnf DeleteIndex' {..} = Prelude.rnf id

instance Data.ToHeaders DeleteIndex where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSKendraFrontendService.DeleteIndex" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteIndex where
  toJSON DeleteIndex' {..} =
    Data.object
      (Prelude.catMaybes [Prelude.Just ("Id" Data..= id)])

instance Data.ToPath DeleteIndex where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteIndex where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteIndexResponse' smart constructor.
data DeleteIndexResponse = DeleteIndexResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteIndexResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteIndexResponse ::
  DeleteIndexResponse
newDeleteIndexResponse = DeleteIndexResponse'

instance Prelude.NFData DeleteIndexResponse where
  rnf _ = ()
