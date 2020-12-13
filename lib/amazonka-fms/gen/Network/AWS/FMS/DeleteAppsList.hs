{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.DeleteAppsList
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Permanently deletes an AWS Firewall Manager applications list.
module Network.AWS.FMS.DeleteAppsList
  ( -- * Creating a request
    DeleteAppsList (..),
    mkDeleteAppsList,

    -- ** Request lenses
    dalListId,

    -- * Destructuring the response
    DeleteAppsListResponse (..),
    mkDeleteAppsListResponse,
  )
where

import Network.AWS.FMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteAppsList' smart constructor.
newtype DeleteAppsList = DeleteAppsList'
  { -- | The ID of the applications list that you want to delete. You can retrieve this ID from @PutAppsList@ , @ListAppsLists@ , and @GetAppsList@ .
    listId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteAppsList' with the minimum fields required to make a request.
--
-- * 'listId' - The ID of the applications list that you want to delete. You can retrieve this ID from @PutAppsList@ , @ListAppsLists@ , and @GetAppsList@ .
mkDeleteAppsList ::
  -- | 'listId'
  Lude.Text ->
  DeleteAppsList
mkDeleteAppsList pListId_ = DeleteAppsList' {listId = pListId_}

-- | The ID of the applications list that you want to delete. You can retrieve this ID from @PutAppsList@ , @ListAppsLists@ , and @GetAppsList@ .
--
-- /Note:/ Consider using 'listId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dalListId :: Lens.Lens' DeleteAppsList Lude.Text
dalListId = Lens.lens (listId :: DeleteAppsList -> Lude.Text) (\s a -> s {listId = a} :: DeleteAppsList)
{-# DEPRECATED dalListId "Use generic-lens or generic-optics with 'listId' instead." #-}

instance Lude.AWSRequest DeleteAppsList where
  type Rs DeleteAppsList = DeleteAppsListResponse
  request = Req.postJSON fmsService
  response = Res.receiveNull DeleteAppsListResponse'

instance Lude.ToHeaders DeleteAppsList where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSFMS_20180101.DeleteAppsList" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteAppsList where
  toJSON DeleteAppsList' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("ListId" Lude..= listId)])

instance Lude.ToPath DeleteAppsList where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteAppsList where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteAppsListResponse' smart constructor.
data DeleteAppsListResponse = DeleteAppsListResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteAppsListResponse' with the minimum fields required to make a request.
mkDeleteAppsListResponse ::
  DeleteAppsListResponse
mkDeleteAppsListResponse = DeleteAppsListResponse'
