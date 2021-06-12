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

import qualified Network.AWS.Core as Core
import Network.AWS.FMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteProtocolsList' smart constructor.
data DeleteProtocolsList = DeleteProtocolsList'
  { -- | The ID of the protocols list that you want to delete. You can retrieve
    -- this ID from @PutProtocolsList@, @ListProtocolsLists@, and
    -- @GetProtocolsLost@.
    listId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  DeleteProtocolsList
newDeleteProtocolsList pListId_ =
  DeleteProtocolsList' {listId = pListId_}

-- | The ID of the protocols list that you want to delete. You can retrieve
-- this ID from @PutProtocolsList@, @ListProtocolsLists@, and
-- @GetProtocolsLost@.
deleteProtocolsList_listId :: Lens.Lens' DeleteProtocolsList Core.Text
deleteProtocolsList_listId = Lens.lens (\DeleteProtocolsList' {listId} -> listId) (\s@DeleteProtocolsList' {} a -> s {listId = a} :: DeleteProtocolsList)

instance Core.AWSRequest DeleteProtocolsList where
  type
    AWSResponse DeleteProtocolsList =
      DeleteProtocolsListResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull DeleteProtocolsListResponse'

instance Core.Hashable DeleteProtocolsList

instance Core.NFData DeleteProtocolsList

instance Core.ToHeaders DeleteProtocolsList where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSFMS_20180101.DeleteProtocolsList" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteProtocolsList where
  toJSON DeleteProtocolsList' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("ListId" Core..= listId)]
      )

instance Core.ToPath DeleteProtocolsList where
  toPath = Core.const "/"

instance Core.ToQuery DeleteProtocolsList where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteProtocolsListResponse' smart constructor.
data DeleteProtocolsListResponse = DeleteProtocolsListResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteProtocolsListResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteProtocolsListResponse ::
  DeleteProtocolsListResponse
newDeleteProtocolsListResponse =
  DeleteProtocolsListResponse'

instance Core.NFData DeleteProtocolsListResponse
