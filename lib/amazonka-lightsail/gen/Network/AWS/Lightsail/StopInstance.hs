{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.StopInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a specific Amazon Lightsail instance that is currently running.
--
-- The @stop instance@ operation supports tag-based access control via resource tags applied to the resource identified by @instance name@ . For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-controlling-access-using-tags Lightsail Dev Guide> .
module Network.AWS.Lightsail.StopInstance
  ( -- * Creating a request
    StopInstance (..),
    mkStopInstance,

    -- ** Request lenses
    siForce,
    siInstanceName,

    -- * Destructuring the response
    StopInstanceResponse (..),
    mkStopInstanceResponse,

    -- ** Response lenses
    sirsOperations,
    sirsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkStopInstance' smart constructor.
data StopInstance = StopInstance'
  { force :: Lude.Maybe Lude.Bool,
    instanceName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StopInstance' with the minimum fields required to make a request.
--
-- * 'force' - When set to @True@ , forces a Lightsail instance that is stuck in a @stopping@ state to stop.
--
-- /Important:/ Only use the @force@ parameter if your instance is stuck in the @stopping@ state. In any other state, your instance should stop normally without adding this parameter to your API request.
-- * 'instanceName' - The name of the instance (a virtual private server) to stop.
mkStopInstance ::
  -- | 'instanceName'
  Lude.Text ->
  StopInstance
mkStopInstance pInstanceName_ =
  StopInstance'
    { force = Lude.Nothing,
      instanceName = pInstanceName_
    }

-- | When set to @True@ , forces a Lightsail instance that is stuck in a @stopping@ state to stop.
--
-- /Important:/ Only use the @force@ parameter if your instance is stuck in the @stopping@ state. In any other state, your instance should stop normally without adding this parameter to your API request.
--
-- /Note:/ Consider using 'force' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siForce :: Lens.Lens' StopInstance (Lude.Maybe Lude.Bool)
siForce = Lens.lens (force :: StopInstance -> Lude.Maybe Lude.Bool) (\s a -> s {force = a} :: StopInstance)
{-# DEPRECATED siForce "Use generic-lens or generic-optics with 'force' instead." #-}

-- | The name of the instance (a virtual private server) to stop.
--
-- /Note:/ Consider using 'instanceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siInstanceName :: Lens.Lens' StopInstance Lude.Text
siInstanceName = Lens.lens (instanceName :: StopInstance -> Lude.Text) (\s a -> s {instanceName = a} :: StopInstance)
{-# DEPRECATED siInstanceName "Use generic-lens or generic-optics with 'instanceName' instead." #-}

instance Lude.AWSRequest StopInstance where
  type Rs StopInstance = StopInstanceResponse
  request = Req.postJSON lightsailService
  response =
    Res.receiveJSON
      ( \s h x ->
          StopInstanceResponse'
            Lude.<$> (x Lude..?> "operations" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StopInstance where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Lightsail_20161128.StopInstance" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StopInstance where
  toJSON StopInstance' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("force" Lude..=) Lude.<$> force,
            Lude.Just ("instanceName" Lude..= instanceName)
          ]
      )

instance Lude.ToPath StopInstance where
  toPath = Lude.const "/"

instance Lude.ToQuery StopInstance where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkStopInstanceResponse' smart constructor.
data StopInstanceResponse = StopInstanceResponse'
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

-- | Creates a value of 'StopInstanceResponse' with the minimum fields required to make a request.
--
-- * 'operations' - An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
-- * 'responseStatus' - The response status code.
mkStopInstanceResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StopInstanceResponse
mkStopInstanceResponse pResponseStatus_ =
  StopInstanceResponse'
    { operations = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sirsOperations :: Lens.Lens' StopInstanceResponse (Lude.Maybe [Operation])
sirsOperations = Lens.lens (operations :: StopInstanceResponse -> Lude.Maybe [Operation]) (\s a -> s {operations = a} :: StopInstanceResponse)
{-# DEPRECATED sirsOperations "Use generic-lens or generic-optics with 'operations' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sirsResponseStatus :: Lens.Lens' StopInstanceResponse Lude.Int
sirsResponseStatus = Lens.lens (responseStatus :: StopInstanceResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StopInstanceResponse)
{-# DEPRECATED sirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
