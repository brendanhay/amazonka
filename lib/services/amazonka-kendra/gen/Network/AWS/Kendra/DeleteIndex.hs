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
-- Module      : Network.AWS.Kendra.DeleteIndex
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an existing Amazon Kendra index. An exception is not thrown if
-- the index is already being deleted. While the index is being deleted,
-- the @Status@ field returned by a call to the @DescribeIndex@ operation
-- is set to @DELETING@.
module Network.AWS.Kendra.DeleteIndex
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

import qualified Network.AWS.Core as Core
import Network.AWS.Kendra.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteIndex' smart constructor.
data DeleteIndex = DeleteIndex'
  { -- | The identifier of the index to delete.
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
-- 'id', 'deleteIndex_id' - The identifier of the index to delete.
newDeleteIndex ::
  -- | 'id'
  Prelude.Text ->
  DeleteIndex
newDeleteIndex pId_ = DeleteIndex' {id = pId_}

-- | The identifier of the index to delete.
deleteIndex_id :: Lens.Lens' DeleteIndex Prelude.Text
deleteIndex_id = Lens.lens (\DeleteIndex' {id} -> id) (\s@DeleteIndex' {} a -> s {id = a} :: DeleteIndex)

instance Core.AWSRequest DeleteIndex where
  type AWSResponse DeleteIndex = DeleteIndexResponse
  request = Request.postJSON defaultService
  response = Response.receiveNull DeleteIndexResponse'

instance Prelude.Hashable DeleteIndex

instance Prelude.NFData DeleteIndex

instance Core.ToHeaders DeleteIndex where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSKendraFrontendService.DeleteIndex" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DeleteIndex where
  toJSON DeleteIndex' {..} =
    Core.object
      (Prelude.catMaybes [Prelude.Just ("Id" Core..= id)])

instance Core.ToPath DeleteIndex where
  toPath = Prelude.const "/"

instance Core.ToQuery DeleteIndex where
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

instance Prelude.NFData DeleteIndexResponse
