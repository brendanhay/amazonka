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
-- Module      : Network.AWS.FMS.DeleteAppsList
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Permanently deletes an AWS Firewall Manager applications list.
module Network.AWS.FMS.DeleteAppsList
  ( -- * Creating a Request
    DeleteAppsList (..),
    newDeleteAppsList,

    -- * Request Lenses
    deleteAppsList_listId,

    -- * Destructuring the Response
    DeleteAppsListResponse (..),
    newDeleteAppsListResponse,
  )
where

import Network.AWS.FMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteAppsList' smart constructor.
data DeleteAppsList = DeleteAppsList'
  { -- | The ID of the applications list that you want to delete. You can
    -- retrieve this ID from @PutAppsList@, @ListAppsLists@, and @GetAppsList@.
    listId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteAppsList' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'listId', 'deleteAppsList_listId' - The ID of the applications list that you want to delete. You can
-- retrieve this ID from @PutAppsList@, @ListAppsLists@, and @GetAppsList@.
newDeleteAppsList ::
  -- | 'listId'
  Prelude.Text ->
  DeleteAppsList
newDeleteAppsList pListId_ =
  DeleteAppsList' {listId = pListId_}

-- | The ID of the applications list that you want to delete. You can
-- retrieve this ID from @PutAppsList@, @ListAppsLists@, and @GetAppsList@.
deleteAppsList_listId :: Lens.Lens' DeleteAppsList Prelude.Text
deleteAppsList_listId = Lens.lens (\DeleteAppsList' {listId} -> listId) (\s@DeleteAppsList' {} a -> s {listId = a} :: DeleteAppsList)

instance Prelude.AWSRequest DeleteAppsList where
  type Rs DeleteAppsList = DeleteAppsListResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull DeleteAppsListResponse'

instance Prelude.Hashable DeleteAppsList

instance Prelude.NFData DeleteAppsList

instance Prelude.ToHeaders DeleteAppsList where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSFMS_20180101.DeleteAppsList" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteAppsList where
  toJSON DeleteAppsList' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("ListId" Prelude..= listId)]
      )

instance Prelude.ToPath DeleteAppsList where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteAppsList where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteAppsListResponse' smart constructor.
data DeleteAppsListResponse = DeleteAppsListResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteAppsListResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteAppsListResponse ::
  DeleteAppsListResponse
newDeleteAppsListResponse = DeleteAppsListResponse'

instance Prelude.NFData DeleteAppsListResponse
