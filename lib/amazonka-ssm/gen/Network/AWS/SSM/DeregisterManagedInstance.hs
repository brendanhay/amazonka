{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.DeregisterManagedInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the server or virtual machine from the list of registered servers. You can reregister the instance again at any time. If you don't plan to use Run Command on the server, we suggest uninstalling SSM Agent first.
module Network.AWS.SSM.DeregisterManagedInstance
  ( -- * Creating a request
    DeregisterManagedInstance (..),
    mkDeregisterManagedInstance,

    -- ** Request lenses
    dmiInstanceId,

    -- * Destructuring the response
    DeregisterManagedInstanceResponse (..),
    mkDeregisterManagedInstanceResponse,

    -- ** Response lenses
    dmirsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SSM.Types

-- | /See:/ 'mkDeregisterManagedInstance' smart constructor.
newtype DeregisterManagedInstance = DeregisterManagedInstance'
  { instanceId ::
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

-- | Creates a value of 'DeregisterManagedInstance' with the minimum fields required to make a request.
--
-- * 'instanceId' - The ID assigned to the managed instance when you registered it using the activation process.
mkDeregisterManagedInstance ::
  -- | 'instanceId'
  Lude.Text ->
  DeregisterManagedInstance
mkDeregisterManagedInstance pInstanceId_ =
  DeregisterManagedInstance' {instanceId = pInstanceId_}

-- | The ID assigned to the managed instance when you registered it using the activation process.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmiInstanceId :: Lens.Lens' DeregisterManagedInstance Lude.Text
dmiInstanceId = Lens.lens (instanceId :: DeregisterManagedInstance -> Lude.Text) (\s a -> s {instanceId = a} :: DeregisterManagedInstance)
{-# DEPRECATED dmiInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

instance Lude.AWSRequest DeregisterManagedInstance where
  type
    Rs DeregisterManagedInstance =
      DeregisterManagedInstanceResponse
  request = Req.postJSON ssmService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeregisterManagedInstanceResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeregisterManagedInstance where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonSSM.DeregisterManagedInstance" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeregisterManagedInstance where
  toJSON DeregisterManagedInstance' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("InstanceId" Lude..= instanceId)])

instance Lude.ToPath DeregisterManagedInstance where
  toPath = Lude.const "/"

instance Lude.ToQuery DeregisterManagedInstance where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeregisterManagedInstanceResponse' smart constructor.
newtype DeregisterManagedInstanceResponse = DeregisterManagedInstanceResponse'
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

-- | Creates a value of 'DeregisterManagedInstanceResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeregisterManagedInstanceResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeregisterManagedInstanceResponse
mkDeregisterManagedInstanceResponse pResponseStatus_ =
  DeregisterManagedInstanceResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmirsResponseStatus :: Lens.Lens' DeregisterManagedInstanceResponse Lude.Int
dmirsResponseStatus = Lens.lens (responseStatus :: DeregisterManagedInstanceResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeregisterManagedInstanceResponse)
{-# DEPRECATED dmirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
