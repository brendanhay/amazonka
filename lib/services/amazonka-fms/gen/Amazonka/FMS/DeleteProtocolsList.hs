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
-- Module      : Amazonka.FMS.DeleteProtocolsList
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Permanently deletes an Firewall Manager protocols list.
module Amazonka.FMS.DeleteProtocolsList
  ( -- * Creating a Request
    DeleteProtocolsList (..),
    newDeleteProtocolsList,

    -- * Request Lenses
    deleteProtocolsList_listId,

    -- * Destructuring the Response
    DeleteProtocolsListResponse (..),
    newDeleteProtocolsListResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FMS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteProtocolsList' smart constructor.
data DeleteProtocolsList = DeleteProtocolsList'
  { -- | The ID of the protocols list that you want to delete. You can retrieve
    -- this ID from @PutProtocolsList@, @ListProtocolsLists@, and
    -- @GetProtocolsLost@.
    listId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteProtocolsList' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'listId', 'deleteProtocolsList_listId' - The ID of the protocols list that you want to delete. You can retrieve
-- this ID from @PutProtocolsList@, @ListProtocolsLists@, and
-- @GetProtocolsLost@.
newDeleteProtocolsList ::
  -- | 'listId'
  Prelude.Text ->
  DeleteProtocolsList
newDeleteProtocolsList pListId_ =
  DeleteProtocolsList' {listId = pListId_}

-- | The ID of the protocols list that you want to delete. You can retrieve
-- this ID from @PutProtocolsList@, @ListProtocolsLists@, and
-- @GetProtocolsLost@.
deleteProtocolsList_listId :: Lens.Lens' DeleteProtocolsList Prelude.Text
deleteProtocolsList_listId = Lens.lens (\DeleteProtocolsList' {listId} -> listId) (\s@DeleteProtocolsList' {} a -> s {listId = a} :: DeleteProtocolsList)

instance Core.AWSRequest DeleteProtocolsList where
  type
    AWSResponse DeleteProtocolsList =
      DeleteProtocolsListResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull DeleteProtocolsListResponse'

instance Prelude.Hashable DeleteProtocolsList where
  hashWithSalt _salt DeleteProtocolsList' {..} =
    _salt `Prelude.hashWithSalt` listId

instance Prelude.NFData DeleteProtocolsList where
  rnf DeleteProtocolsList' {..} = Prelude.rnf listId

instance Data.ToHeaders DeleteProtocolsList where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSFMS_20180101.DeleteProtocolsList" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteProtocolsList where
  toJSON DeleteProtocolsList' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("ListId" Data..= listId)]
      )

instance Data.ToPath DeleteProtocolsList where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteProtocolsList where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteProtocolsListResponse' smart constructor.
data DeleteProtocolsListResponse = DeleteProtocolsListResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteProtocolsListResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteProtocolsListResponse ::
  DeleteProtocolsListResponse
newDeleteProtocolsListResponse =
  DeleteProtocolsListResponse'

instance Prelude.NFData DeleteProtocolsListResponse where
  rnf _ = ()
