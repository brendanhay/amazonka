{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.GetInstanceState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the state of a specific instance. Works on one instance at a time.
module Network.AWS.Lightsail.GetInstanceState
  ( -- * Creating a request
    GetInstanceState (..),
    mkGetInstanceState,

    -- ** Request lenses
    gisInstanceName,

    -- * Destructuring the response
    GetInstanceStateResponse (..),
    mkGetInstanceStateResponse,

    -- ** Response lenses
    gisirsState,
    gisirsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetInstanceState' smart constructor.
newtype GetInstanceState = GetInstanceState'
  { instanceName ::
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

-- | Creates a value of 'GetInstanceState' with the minimum fields required to make a request.
--
-- * 'instanceName' - The name of the instance to get state information about.
mkGetInstanceState ::
  -- | 'instanceName'
  Lude.Text ->
  GetInstanceState
mkGetInstanceState pInstanceName_ =
  GetInstanceState' {instanceName = pInstanceName_}

-- | The name of the instance to get state information about.
--
-- /Note:/ Consider using 'instanceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gisInstanceName :: Lens.Lens' GetInstanceState Lude.Text
gisInstanceName = Lens.lens (instanceName :: GetInstanceState -> Lude.Text) (\s a -> s {instanceName = a} :: GetInstanceState)
{-# DEPRECATED gisInstanceName "Use generic-lens or generic-optics with 'instanceName' instead." #-}

instance Lude.AWSRequest GetInstanceState where
  type Rs GetInstanceState = GetInstanceStateResponse
  request = Req.postJSON lightsailService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetInstanceStateResponse'
            Lude.<$> (x Lude..?> "state") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetInstanceState where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Lightsail_20161128.GetInstanceState" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetInstanceState where
  toJSON GetInstanceState' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("instanceName" Lude..= instanceName)])

instance Lude.ToPath GetInstanceState where
  toPath = Lude.const "/"

instance Lude.ToQuery GetInstanceState where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetInstanceStateResponse' smart constructor.
data GetInstanceStateResponse = GetInstanceStateResponse'
  { state ::
      Lude.Maybe InstanceState,
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

-- | Creates a value of 'GetInstanceStateResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'state' - The state of the instance.
mkGetInstanceStateResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetInstanceStateResponse
mkGetInstanceStateResponse pResponseStatus_ =
  GetInstanceStateResponse'
    { state = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The state of the instance.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gisirsState :: Lens.Lens' GetInstanceStateResponse (Lude.Maybe InstanceState)
gisirsState = Lens.lens (state :: GetInstanceStateResponse -> Lude.Maybe InstanceState) (\s a -> s {state = a} :: GetInstanceStateResponse)
{-# DEPRECATED gisirsState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gisirsResponseStatus :: Lens.Lens' GetInstanceStateResponse Lude.Int
gisirsResponseStatus = Lens.lens (responseStatus :: GetInstanceStateResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetInstanceStateResponse)
{-# DEPRECATED gisirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
