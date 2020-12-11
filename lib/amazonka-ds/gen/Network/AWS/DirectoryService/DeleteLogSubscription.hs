{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.DeleteLogSubscription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified log subscription.
module Network.AWS.DirectoryService.DeleteLogSubscription
  ( -- * Creating a request
    DeleteLogSubscription (..),
    mkDeleteLogSubscription,

    -- ** Request lenses
    dlsDirectoryId,

    -- * Destructuring the response
    DeleteLogSubscriptionResponse (..),
    mkDeleteLogSubscriptionResponse,

    -- ** Response lenses
    dlsrsResponseStatus,
  )
where

import Network.AWS.DirectoryService.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteLogSubscription' smart constructor.
newtype DeleteLogSubscription = DeleteLogSubscription'
  { directoryId ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteLogSubscription' with the minimum fields required to make a request.
--
-- * 'directoryId' - Identifier of the directory whose log subscription you want to delete.
mkDeleteLogSubscription ::
  -- | 'directoryId'
  Lude.Text ->
  DeleteLogSubscription
mkDeleteLogSubscription pDirectoryId_ =
  DeleteLogSubscription' {directoryId = pDirectoryId_}

-- | Identifier of the directory whose log subscription you want to delete.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlsDirectoryId :: Lens.Lens' DeleteLogSubscription Lude.Text
dlsDirectoryId = Lens.lens (directoryId :: DeleteLogSubscription -> Lude.Text) (\s a -> s {directoryId = a} :: DeleteLogSubscription)
{-# DEPRECATED dlsDirectoryId "Use generic-lens or generic-optics with 'directoryId' instead." #-}

instance Lude.AWSRequest DeleteLogSubscription where
  type Rs DeleteLogSubscription = DeleteLogSubscriptionResponse
  request = Req.postJSON directoryServiceService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteLogSubscriptionResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteLogSubscription where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "DirectoryService_20150416.DeleteLogSubscription" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteLogSubscription where
  toJSON DeleteLogSubscription' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("DirectoryId" Lude..= directoryId)])

instance Lude.ToPath DeleteLogSubscription where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteLogSubscription where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteLogSubscriptionResponse' smart constructor.
newtype DeleteLogSubscriptionResponse = DeleteLogSubscriptionResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteLogSubscriptionResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteLogSubscriptionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteLogSubscriptionResponse
mkDeleteLogSubscriptionResponse pResponseStatus_ =
  DeleteLogSubscriptionResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlsrsResponseStatus :: Lens.Lens' DeleteLogSubscriptionResponse Lude.Int
dlsrsResponseStatus = Lens.lens (responseStatus :: DeleteLogSubscriptionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteLogSubscriptionResponse)
{-# DEPRECATED dlsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
