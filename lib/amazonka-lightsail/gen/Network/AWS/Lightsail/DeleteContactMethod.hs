{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.DeleteContactMethod
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a contact method.
--
-- A contact method is used to send you notifications about your Amazon Lightsail resources. You can add one email address and one mobile phone number contact method in each AWS Region. However, SMS text messaging is not supported in some AWS Regions, and SMS text messages cannot be sent to some countries/regions. For more information, see <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-notifications Notifications in Amazon Lightsail> .
module Network.AWS.Lightsail.DeleteContactMethod
  ( -- * Creating a request
    DeleteContactMethod (..),
    mkDeleteContactMethod,

    -- ** Request lenses
    dcmProtocol,

    -- * Destructuring the response
    DeleteContactMethodResponse (..),
    mkDeleteContactMethodResponse,

    -- ** Response lenses
    dcmrsOperations,
    dcmrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteContactMethod' smart constructor.
newtype DeleteContactMethod = DeleteContactMethod'
  { protocol ::
      ContactProtocol
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteContactMethod' with the minimum fields required to make a request.
--
-- * 'protocol' - The protocol that will be deleted, such as @Email@ or @SMS@ (text messaging).
mkDeleteContactMethod ::
  -- | 'protocol'
  ContactProtocol ->
  DeleteContactMethod
mkDeleteContactMethod pProtocol_ =
  DeleteContactMethod' {protocol = pProtocol_}

-- | The protocol that will be deleted, such as @Email@ or @SMS@ (text messaging).
--
-- /Note:/ Consider using 'protocol' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcmProtocol :: Lens.Lens' DeleteContactMethod ContactProtocol
dcmProtocol = Lens.lens (protocol :: DeleteContactMethod -> ContactProtocol) (\s a -> s {protocol = a} :: DeleteContactMethod)
{-# DEPRECATED dcmProtocol "Use generic-lens or generic-optics with 'protocol' instead." #-}

instance Lude.AWSRequest DeleteContactMethod where
  type Rs DeleteContactMethod = DeleteContactMethodResponse
  request = Req.postJSON lightsailService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteContactMethodResponse'
            Lude.<$> (x Lude..?> "operations" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteContactMethod where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Lightsail_20161128.DeleteContactMethod" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteContactMethod where
  toJSON DeleteContactMethod' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("protocol" Lude..= protocol)])

instance Lude.ToPath DeleteContactMethod where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteContactMethod where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteContactMethodResponse' smart constructor.
data DeleteContactMethodResponse = DeleteContactMethodResponse'
  { operations ::
      Lude.Maybe [Operation],
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteContactMethodResponse' with the minimum fields required to make a request.
--
-- * 'operations' - An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
-- * 'responseStatus' - The response status code.
mkDeleteContactMethodResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteContactMethodResponse
mkDeleteContactMethodResponse pResponseStatus_ =
  DeleteContactMethodResponse'
    { operations = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcmrsOperations :: Lens.Lens' DeleteContactMethodResponse (Lude.Maybe [Operation])
dcmrsOperations = Lens.lens (operations :: DeleteContactMethodResponse -> Lude.Maybe [Operation]) (\s a -> s {operations = a} :: DeleteContactMethodResponse)
{-# DEPRECATED dcmrsOperations "Use generic-lens or generic-optics with 'operations' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcmrsResponseStatus :: Lens.Lens' DeleteContactMethodResponse Lude.Int
dcmrsResponseStatus = Lens.lens (responseStatus :: DeleteContactMethodResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteContactMethodResponse)
{-# DEPRECATED dcmrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
