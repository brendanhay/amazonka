{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.StopContact
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Ends the specified contact.
module Network.AWS.Connect.StopContact
  ( -- * Creating a request
    StopContact (..),
    mkStopContact,

    -- ** Request lenses
    scContactId,
    scInstanceId,

    -- * Destructuring the response
    StopContactResponse (..),
    mkStopContactResponse,

    -- ** Response lenses
    scrsResponseStatus,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkStopContact' smart constructor.
data StopContact = StopContact'
  { contactId :: Lude.Text,
    instanceId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StopContact' with the minimum fields required to make a request.
--
-- * 'contactId' - The ID of the contact.
-- * 'instanceId' - The identifier of the Amazon Connect instance.
mkStopContact ::
  -- | 'contactId'
  Lude.Text ->
  -- | 'instanceId'
  Lude.Text ->
  StopContact
mkStopContact pContactId_ pInstanceId_ =
  StopContact' {contactId = pContactId_, instanceId = pInstanceId_}

-- | The ID of the contact.
--
-- /Note:/ Consider using 'contactId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scContactId :: Lens.Lens' StopContact Lude.Text
scContactId = Lens.lens (contactId :: StopContact -> Lude.Text) (\s a -> s {contactId = a} :: StopContact)
{-# DEPRECATED scContactId "Use generic-lens or generic-optics with 'contactId' instead." #-}

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scInstanceId :: Lens.Lens' StopContact Lude.Text
scInstanceId = Lens.lens (instanceId :: StopContact -> Lude.Text) (\s a -> s {instanceId = a} :: StopContact)
{-# DEPRECATED scInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

instance Lude.AWSRequest StopContact where
  type Rs StopContact = StopContactResponse
  request = Req.postJSON connectService
  response =
    Res.receiveEmpty
      ( \s h x ->
          StopContactResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StopContact where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StopContact where
  toJSON StopContact' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ContactId" Lude..= contactId),
            Lude.Just ("InstanceId" Lude..= instanceId)
          ]
      )

instance Lude.ToPath StopContact where
  toPath = Lude.const "/contact/stop"

instance Lude.ToQuery StopContact where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkStopContactResponse' smart constructor.
newtype StopContactResponse = StopContactResponse'
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

-- | Creates a value of 'StopContactResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkStopContactResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StopContactResponse
mkStopContactResponse pResponseStatus_ =
  StopContactResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scrsResponseStatus :: Lens.Lens' StopContactResponse Lude.Int
scrsResponseStatus = Lens.lens (responseStatus :: StopContactResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StopContactResponse)
{-# DEPRECATED scrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
