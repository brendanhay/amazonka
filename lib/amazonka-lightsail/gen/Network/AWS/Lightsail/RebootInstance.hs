{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.RebootInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Restarts a specific instance.
--
-- The @reboot instance@ operation supports tag-based access control via resource tags applied to the resource identified by @instance name@ . For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-controlling-access-using-tags Lightsail Dev Guide> .
module Network.AWS.Lightsail.RebootInstance
  ( -- * Creating a request
    RebootInstance (..),
    mkRebootInstance,

    -- ** Request lenses
    riInstanceName,

    -- * Destructuring the response
    RebootInstanceResponse (..),
    mkRebootInstanceResponse,

    -- ** Response lenses
    rirsOperations,
    rirsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkRebootInstance' smart constructor.
newtype RebootInstance = RebootInstance' {instanceName :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RebootInstance' with the minimum fields required to make a request.
--
-- * 'instanceName' - The name of the instance to reboot.
mkRebootInstance ::
  -- | 'instanceName'
  Lude.Text ->
  RebootInstance
mkRebootInstance pInstanceName_ =
  RebootInstance' {instanceName = pInstanceName_}

-- | The name of the instance to reboot.
--
-- /Note:/ Consider using 'instanceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riInstanceName :: Lens.Lens' RebootInstance Lude.Text
riInstanceName = Lens.lens (instanceName :: RebootInstance -> Lude.Text) (\s a -> s {instanceName = a} :: RebootInstance)
{-# DEPRECATED riInstanceName "Use generic-lens or generic-optics with 'instanceName' instead." #-}

instance Lude.AWSRequest RebootInstance where
  type Rs RebootInstance = RebootInstanceResponse
  request = Req.postJSON lightsailService
  response =
    Res.receiveJSON
      ( \s h x ->
          RebootInstanceResponse'
            Lude.<$> (x Lude..?> "operations" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders RebootInstance where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Lightsail_20161128.RebootInstance" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON RebootInstance where
  toJSON RebootInstance' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("instanceName" Lude..= instanceName)])

instance Lude.ToPath RebootInstance where
  toPath = Lude.const "/"

instance Lude.ToQuery RebootInstance where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkRebootInstanceResponse' smart constructor.
data RebootInstanceResponse = RebootInstanceResponse'
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

-- | Creates a value of 'RebootInstanceResponse' with the minimum fields required to make a request.
--
-- * 'operations' - An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
-- * 'responseStatus' - The response status code.
mkRebootInstanceResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  RebootInstanceResponse
mkRebootInstanceResponse pResponseStatus_ =
  RebootInstanceResponse'
    { operations = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rirsOperations :: Lens.Lens' RebootInstanceResponse (Lude.Maybe [Operation])
rirsOperations = Lens.lens (operations :: RebootInstanceResponse -> Lude.Maybe [Operation]) (\s a -> s {operations = a} :: RebootInstanceResponse)
{-# DEPRECATED rirsOperations "Use generic-lens or generic-optics with 'operations' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rirsResponseStatus :: Lens.Lens' RebootInstanceResponse Lude.Int
rirsResponseStatus = Lens.lens (responseStatus :: RebootInstanceResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: RebootInstanceResponse)
{-# DEPRECATED rirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
