{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.DeleteProtection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an AWS Shield Advanced 'Protection' .
module Network.AWS.Shield.DeleteProtection
  ( -- * Creating a request
    DeleteProtection (..),
    mkDeleteProtection,

    -- ** Request lenses
    dProtectionId,

    -- * Destructuring the response
    DeleteProtectionResponse (..),
    mkDeleteProtectionResponse,

    -- ** Response lenses
    delrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Shield.Types

-- | /See:/ 'mkDeleteProtection' smart constructor.
newtype DeleteProtection = DeleteProtection'
  { protectionId ::
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

-- | Creates a value of 'DeleteProtection' with the minimum fields required to make a request.
--
-- * 'protectionId' - The unique identifier (ID) for the 'Protection' object to be deleted.
mkDeleteProtection ::
  -- | 'protectionId'
  Lude.Text ->
  DeleteProtection
mkDeleteProtection pProtectionId_ =
  DeleteProtection' {protectionId = pProtectionId_}

-- | The unique identifier (ID) for the 'Protection' object to be deleted.
--
-- /Note:/ Consider using 'protectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dProtectionId :: Lens.Lens' DeleteProtection Lude.Text
dProtectionId = Lens.lens (protectionId :: DeleteProtection -> Lude.Text) (\s a -> s {protectionId = a} :: DeleteProtection)
{-# DEPRECATED dProtectionId "Use generic-lens or generic-optics with 'protectionId' instead." #-}

instance Lude.AWSRequest DeleteProtection where
  type Rs DeleteProtection = DeleteProtectionResponse
  request = Req.postJSON shieldService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteProtectionResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteProtection where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSShield_20160616.DeleteProtection" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteProtection where
  toJSON DeleteProtection' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("ProtectionId" Lude..= protectionId)])

instance Lude.ToPath DeleteProtection where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteProtection where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteProtectionResponse' smart constructor.
newtype DeleteProtectionResponse = DeleteProtectionResponse'
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

-- | Creates a value of 'DeleteProtectionResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteProtectionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteProtectionResponse
mkDeleteProtectionResponse pResponseStatus_ =
  DeleteProtectionResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
delrsResponseStatus :: Lens.Lens' DeleteProtectionResponse Lude.Int
delrsResponseStatus = Lens.lens (responseStatus :: DeleteProtectionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteProtectionResponse)
{-# DEPRECATED delrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
