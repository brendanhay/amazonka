{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.DeleteProtocolsList
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Permanently deletes an AWS Firewall Manager protocols list.
module Network.AWS.FMS.DeleteProtocolsList
  ( -- * Creating a request
    DeleteProtocolsList (..),
    mkDeleteProtocolsList,

    -- ** Request lenses
    dplListId,

    -- * Destructuring the response
    DeleteProtocolsListResponse (..),
    mkDeleteProtocolsListResponse,
  )
where

import Network.AWS.FMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteProtocolsList' smart constructor.
newtype DeleteProtocolsList = DeleteProtocolsList'
  { -- | The ID of the protocols list that you want to delete. You can retrieve this ID from @PutProtocolsList@ , @ListProtocolsLists@ , and @GetProtocolsLost@ .
    listId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteProtocolsList' with the minimum fields required to make a request.
--
-- * 'listId' - The ID of the protocols list that you want to delete. You can retrieve this ID from @PutProtocolsList@ , @ListProtocolsLists@ , and @GetProtocolsLost@ .
mkDeleteProtocolsList ::
  -- | 'listId'
  Lude.Text ->
  DeleteProtocolsList
mkDeleteProtocolsList pListId_ =
  DeleteProtocolsList' {listId = pListId_}

-- | The ID of the protocols list that you want to delete. You can retrieve this ID from @PutProtocolsList@ , @ListProtocolsLists@ , and @GetProtocolsLost@ .
--
-- /Note:/ Consider using 'listId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dplListId :: Lens.Lens' DeleteProtocolsList Lude.Text
dplListId = Lens.lens (listId :: DeleteProtocolsList -> Lude.Text) (\s a -> s {listId = a} :: DeleteProtocolsList)
{-# DEPRECATED dplListId "Use generic-lens or generic-optics with 'listId' instead." #-}

instance Lude.AWSRequest DeleteProtocolsList where
  type Rs DeleteProtocolsList = DeleteProtocolsListResponse
  request = Req.postJSON fmsService
  response = Res.receiveNull DeleteProtocolsListResponse'

instance Lude.ToHeaders DeleteProtocolsList where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSFMS_20180101.DeleteProtocolsList" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteProtocolsList where
  toJSON DeleteProtocolsList' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("ListId" Lude..= listId)])

instance Lude.ToPath DeleteProtocolsList where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteProtocolsList where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteProtocolsListResponse' smart constructor.
data DeleteProtocolsListResponse = DeleteProtocolsListResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteProtocolsListResponse' with the minimum fields required to make a request.
mkDeleteProtocolsListResponse ::
  DeleteProtocolsListResponse
mkDeleteProtocolsListResponse = DeleteProtocolsListResponse'
