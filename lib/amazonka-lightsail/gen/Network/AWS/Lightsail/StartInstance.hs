{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.StartInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a specific Amazon Lightsail instance from a stopped state. To restart an instance, use the @reboot instance@ operation.
--
-- The @start instance@ operation supports tag-based access control via resource tags applied to the resource identified by @instance name@ . For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-controlling-access-using-tags Lightsail Dev Guide> .
module Network.AWS.Lightsail.StartInstance
  ( -- * Creating a request
    StartInstance (..),
    mkStartInstance,

    -- ** Request lenses
    sInstanceName,

    -- * Destructuring the response
    StartInstanceResponse (..),
    mkStartInstanceResponse,

    -- ** Response lenses
    sifrsOperations,
    sifrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkStartInstance' smart constructor.
newtype StartInstance = StartInstance'
  { -- | The name of the instance (a virtual private server) to start.
    instanceName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartInstance' with the minimum fields required to make a request.
--
-- * 'instanceName' - The name of the instance (a virtual private server) to start.
mkStartInstance ::
  -- | 'instanceName'
  Lude.Text ->
  StartInstance
mkStartInstance pInstanceName_ =
  StartInstance' {instanceName = pInstanceName_}

-- | The name of the instance (a virtual private server) to start.
--
-- /Note:/ Consider using 'instanceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sInstanceName :: Lens.Lens' StartInstance Lude.Text
sInstanceName = Lens.lens (instanceName :: StartInstance -> Lude.Text) (\s a -> s {instanceName = a} :: StartInstance)
{-# DEPRECATED sInstanceName "Use generic-lens or generic-optics with 'instanceName' instead." #-}

instance Lude.AWSRequest StartInstance where
  type Rs StartInstance = StartInstanceResponse
  request = Req.postJSON lightsailService
  response =
    Res.receiveJSON
      ( \s h x ->
          StartInstanceResponse'
            Lude.<$> (x Lude..?> "operations" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StartInstance where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Lightsail_20161128.StartInstance" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StartInstance where
  toJSON StartInstance' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("instanceName" Lude..= instanceName)])

instance Lude.ToPath StartInstance where
  toPath = Lude.const "/"

instance Lude.ToQuery StartInstance where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkStartInstanceResponse' smart constructor.
data StartInstanceResponse = StartInstanceResponse'
  { -- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
    operations :: Lude.Maybe [Operation],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartInstanceResponse' with the minimum fields required to make a request.
--
-- * 'operations' - An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
-- * 'responseStatus' - The response status code.
mkStartInstanceResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StartInstanceResponse
mkStartInstanceResponse pResponseStatus_ =
  StartInstanceResponse'
    { operations = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sifrsOperations :: Lens.Lens' StartInstanceResponse (Lude.Maybe [Operation])
sifrsOperations = Lens.lens (operations :: StartInstanceResponse -> Lude.Maybe [Operation]) (\s a -> s {operations = a} :: StartInstanceResponse)
{-# DEPRECATED sifrsOperations "Use generic-lens or generic-optics with 'operations' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sifrsResponseStatus :: Lens.Lens' StartInstanceResponse Lude.Int
sifrsResponseStatus = Lens.lens (responseStatus :: StartInstanceResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StartInstanceResponse)
{-# DEPRECATED sifrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
