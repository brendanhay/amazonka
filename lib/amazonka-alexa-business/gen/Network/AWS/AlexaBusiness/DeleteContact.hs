{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.DeleteContact
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a contact by the contact ARN.
module Network.AWS.AlexaBusiness.DeleteContact
  ( -- * Creating a request
    DeleteContact (..),
    mkDeleteContact,

    -- ** Request lenses
    dcContactARN,

    -- * Destructuring the response
    DeleteContactResponse (..),
    mkDeleteContactResponse,

    -- ** Response lenses
    dcrsResponseStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteContact' smart constructor.
newtype DeleteContact = DeleteContact'
  { -- | The ARN of the contact to delete.
    contactARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteContact' with the minimum fields required to make a request.
--
-- * 'contactARN' - The ARN of the contact to delete.
mkDeleteContact ::
  -- | 'contactARN'
  Lude.Text ->
  DeleteContact
mkDeleteContact pContactARN_ =
  DeleteContact' {contactARN = pContactARN_}

-- | The ARN of the contact to delete.
--
-- /Note:/ Consider using 'contactARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcContactARN :: Lens.Lens' DeleteContact Lude.Text
dcContactARN = Lens.lens (contactARN :: DeleteContact -> Lude.Text) (\s a -> s {contactARN = a} :: DeleteContact)
{-# DEPRECATED dcContactARN "Use generic-lens or generic-optics with 'contactARN' instead." #-}

instance Lude.AWSRequest DeleteContact where
  type Rs DeleteContact = DeleteContactResponse
  request = Req.postJSON alexaBusinessService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteContactResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteContact where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AlexaForBusiness.DeleteContact" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteContact where
  toJSON DeleteContact' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("ContactArn" Lude..= contactARN)])

instance Lude.ToPath DeleteContact where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteContact where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteContactResponse' smart constructor.
newtype DeleteContactResponse = DeleteContactResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteContactResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteContactResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteContactResponse
mkDeleteContactResponse pResponseStatus_ =
  DeleteContactResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrsResponseStatus :: Lens.Lens' DeleteContactResponse Lude.Int
dcrsResponseStatus = Lens.lens (responseStatus :: DeleteContactResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteContactResponse)
{-# DEPRECATED dcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
