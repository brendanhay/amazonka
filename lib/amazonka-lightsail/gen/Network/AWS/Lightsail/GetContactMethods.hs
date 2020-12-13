{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.GetContactMethods
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the configured contact methods. Specify a protocol in your request to return information about a specific contact method.
--
-- A contact method is used to send you notifications about your Amazon Lightsail resources. You can add one email address and one mobile phone number contact method in each AWS Region. However, SMS text messaging is not supported in some AWS Regions, and SMS text messages cannot be sent to some countries/regions. For more information, see <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-notifications Notifications in Amazon Lightsail> .
module Network.AWS.Lightsail.GetContactMethods
  ( -- * Creating a request
    GetContactMethods (..),
    mkGetContactMethods,

    -- ** Request lenses
    gcmProtocols,

    -- * Destructuring the response
    GetContactMethodsResponse (..),
    mkGetContactMethodsResponse,

    -- ** Response lenses
    gcmrsContactMethods,
    gcmrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetContactMethods' smart constructor.
newtype GetContactMethods = GetContactMethods'
  { -- | The protocols used to send notifications, such as @Email@ , or @SMS@ (text messaging).
    --
    -- Specify a protocol in your request to return information about a specific contact method protocol.
    protocols :: Lude.Maybe [ContactProtocol]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetContactMethods' with the minimum fields required to make a request.
--
-- * 'protocols' - The protocols used to send notifications, such as @Email@ , or @SMS@ (text messaging).
--
-- Specify a protocol in your request to return information about a specific contact method protocol.
mkGetContactMethods ::
  GetContactMethods
mkGetContactMethods = GetContactMethods' {protocols = Lude.Nothing}

-- | The protocols used to send notifications, such as @Email@ , or @SMS@ (text messaging).
--
-- Specify a protocol in your request to return information about a specific contact method protocol.
--
-- /Note:/ Consider using 'protocols' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcmProtocols :: Lens.Lens' GetContactMethods (Lude.Maybe [ContactProtocol])
gcmProtocols = Lens.lens (protocols :: GetContactMethods -> Lude.Maybe [ContactProtocol]) (\s a -> s {protocols = a} :: GetContactMethods)
{-# DEPRECATED gcmProtocols "Use generic-lens or generic-optics with 'protocols' instead." #-}

instance Lude.AWSRequest GetContactMethods where
  type Rs GetContactMethods = GetContactMethodsResponse
  request = Req.postJSON lightsailService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetContactMethodsResponse'
            Lude.<$> (x Lude..?> "contactMethods" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetContactMethods where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Lightsail_20161128.GetContactMethods" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetContactMethods where
  toJSON GetContactMethods' {..} =
    Lude.object
      (Lude.catMaybes [("protocols" Lude..=) Lude.<$> protocols])

instance Lude.ToPath GetContactMethods where
  toPath = Lude.const "/"

instance Lude.ToQuery GetContactMethods where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetContactMethodsResponse' smart constructor.
data GetContactMethodsResponse = GetContactMethodsResponse'
  { -- | An array of objects that describe the contact methods.
    contactMethods :: Lude.Maybe [ContactMethod],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetContactMethodsResponse' with the minimum fields required to make a request.
--
-- * 'contactMethods' - An array of objects that describe the contact methods.
-- * 'responseStatus' - The response status code.
mkGetContactMethodsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetContactMethodsResponse
mkGetContactMethodsResponse pResponseStatus_ =
  GetContactMethodsResponse'
    { contactMethods = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of objects that describe the contact methods.
--
-- /Note:/ Consider using 'contactMethods' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcmrsContactMethods :: Lens.Lens' GetContactMethodsResponse (Lude.Maybe [ContactMethod])
gcmrsContactMethods = Lens.lens (contactMethods :: GetContactMethodsResponse -> Lude.Maybe [ContactMethod]) (\s a -> s {contactMethods = a} :: GetContactMethodsResponse)
{-# DEPRECATED gcmrsContactMethods "Use generic-lens or generic-optics with 'contactMethods' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcmrsResponseStatus :: Lens.Lens' GetContactMethodsResponse Lude.Int
gcmrsResponseStatus = Lens.lens (responseStatus :: GetContactMethodsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetContactMethodsResponse)
{-# DEPRECATED gcmrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
