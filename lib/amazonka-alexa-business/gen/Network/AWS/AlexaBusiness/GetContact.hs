{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.GetContact
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the contact details by the contact ARN.
module Network.AWS.AlexaBusiness.GetContact
  ( -- * Creating a request
    GetContact (..),
    mkGetContact,

    -- ** Request lenses
    gcContactARN,

    -- * Destructuring the response
    GetContactResponse (..),
    mkGetContactResponse,

    -- ** Response lenses
    gcrsContact,
    gcrsResponseStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetContact' smart constructor.
newtype GetContact = GetContact'
  { -- | The ARN of the contact for which to request details.
    contactARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetContact' with the minimum fields required to make a request.
--
-- * 'contactARN' - The ARN of the contact for which to request details.
mkGetContact ::
  -- | 'contactARN'
  Lude.Text ->
  GetContact
mkGetContact pContactARN_ = GetContact' {contactARN = pContactARN_}

-- | The ARN of the contact for which to request details.
--
-- /Note:/ Consider using 'contactARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcContactARN :: Lens.Lens' GetContact Lude.Text
gcContactARN = Lens.lens (contactARN :: GetContact -> Lude.Text) (\s a -> s {contactARN = a} :: GetContact)
{-# DEPRECATED gcContactARN "Use generic-lens or generic-optics with 'contactARN' instead." #-}

instance Lude.AWSRequest GetContact where
  type Rs GetContact = GetContactResponse
  request = Req.postJSON alexaBusinessService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetContactResponse'
            Lude.<$> (x Lude..?> "Contact") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetContact where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AlexaForBusiness.GetContact" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetContact where
  toJSON GetContact' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("ContactArn" Lude..= contactARN)])

instance Lude.ToPath GetContact where
  toPath = Lude.const "/"

instance Lude.ToQuery GetContact where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetContactResponse' smart constructor.
data GetContactResponse = GetContactResponse'
  { -- | The details of the requested contact.
    contact :: Lude.Maybe Contact,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetContactResponse' with the minimum fields required to make a request.
--
-- * 'contact' - The details of the requested contact.
-- * 'responseStatus' - The response status code.
mkGetContactResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetContactResponse
mkGetContactResponse pResponseStatus_ =
  GetContactResponse'
    { contact = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The details of the requested contact.
--
-- /Note:/ Consider using 'contact' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrsContact :: Lens.Lens' GetContactResponse (Lude.Maybe Contact)
gcrsContact = Lens.lens (contact :: GetContactResponse -> Lude.Maybe Contact) (\s a -> s {contact = a} :: GetContactResponse)
{-# DEPRECATED gcrsContact "Use generic-lens or generic-optics with 'contact' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrsResponseStatus :: Lens.Lens' GetContactResponse Lude.Int
gcrsResponseStatus = Lens.lens (responseStatus :: GetContactResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetContactResponse)
{-# DEPRECATED gcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
