{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.FMS.DeleteProtocolsList
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Permanently deletes an AWS Firewall Manager protocols list.
module Network.AWS.FMS.DeleteProtocolsList
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

import Network.AWS.FMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteProtocolsList' smart constructor.
data DeleteProtocolsList = DeleteProtocolsList'
  { -- | The ID of the protocols list that you want to delete. You can retrieve
    -- this ID from @PutProtocolsList@, @ListProtocolsLists@, and
    -- @GetProtocolsLost@.
    listId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest DeleteProtocolsList where
  type
    Rs DeleteProtocolsList =
      DeleteProtocolsListResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull DeleteProtocolsListResponse'

instance Prelude.Hashable DeleteProtocolsList

instance Prelude.NFData DeleteProtocolsList

instance Prelude.ToHeaders DeleteProtocolsList where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSFMS_20180101.DeleteProtocolsList" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteProtocolsList where
  toJSON DeleteProtocolsList' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("ListId" Prelude..= listId)]
      )

instance Prelude.ToPath DeleteProtocolsList where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteProtocolsList where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteProtocolsListResponse' smart constructor.
data DeleteProtocolsListResponse = DeleteProtocolsListResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteProtocolsListResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteProtocolsListResponse ::
  DeleteProtocolsListResponse
newDeleteProtocolsListResponse =
  DeleteProtocolsListResponse'

instance Prelude.NFData DeleteProtocolsListResponse
