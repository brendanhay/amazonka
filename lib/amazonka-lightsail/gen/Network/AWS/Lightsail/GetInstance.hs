{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.GetInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a specific Amazon Lightsail instance, which is a virtual private server.
module Network.AWS.Lightsail.GetInstance
  ( -- * Creating a request
    GetInstance (..),
    mkGetInstance,

    -- ** Request lenses
    giInstanceName,

    -- * Destructuring the response
    GetInstanceResponse (..),
    mkGetInstanceResponse,

    -- ** Response lenses
    girsInstance,
    girsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetInstance' smart constructor.
newtype GetInstance = GetInstance' {instanceName :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetInstance' with the minimum fields required to make a request.
--
-- * 'instanceName' - The name of the instance.
mkGetInstance ::
  -- | 'instanceName'
  Lude.Text ->
  GetInstance
mkGetInstance pInstanceName_ =
  GetInstance' {instanceName = pInstanceName_}

-- | The name of the instance.
--
-- /Note:/ Consider using 'instanceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giInstanceName :: Lens.Lens' GetInstance Lude.Text
giInstanceName = Lens.lens (instanceName :: GetInstance -> Lude.Text) (\s a -> s {instanceName = a} :: GetInstance)
{-# DEPRECATED giInstanceName "Use generic-lens or generic-optics with 'instanceName' instead." #-}

instance Lude.AWSRequest GetInstance where
  type Rs GetInstance = GetInstanceResponse
  request = Req.postJSON lightsailService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetInstanceResponse'
            Lude.<$> (x Lude..?> "instance") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetInstance where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Lightsail_20161128.GetInstance" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetInstance where
  toJSON GetInstance' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("instanceName" Lude..= instanceName)])

instance Lude.ToPath GetInstance where
  toPath = Lude.const "/"

instance Lude.ToQuery GetInstance where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetInstanceResponse' smart constructor.
data GetInstanceResponse = GetInstanceResponse'
  { instance' ::
      Lude.Maybe Instance,
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

-- | Creates a value of 'GetInstanceResponse' with the minimum fields required to make a request.
--
-- * 'instance'' - An array of key-value pairs containing information about the specified instance.
-- * 'responseStatus' - The response status code.
mkGetInstanceResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetInstanceResponse
mkGetInstanceResponse pResponseStatus_ =
  GetInstanceResponse'
    { instance' = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of key-value pairs containing information about the specified instance.
--
-- /Note:/ Consider using 'instance'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
girsInstance :: Lens.Lens' GetInstanceResponse (Lude.Maybe Instance)
girsInstance = Lens.lens (instance' :: GetInstanceResponse -> Lude.Maybe Instance) (\s a -> s {instance' = a} :: GetInstanceResponse)
{-# DEPRECATED girsInstance "Use generic-lens or generic-optics with 'instance'' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
girsResponseStatus :: Lens.Lens' GetInstanceResponse Lude.Int
girsResponseStatus = Lens.lens (responseStatus :: GetInstanceResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetInstanceResponse)
{-# DEPRECATED girsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
