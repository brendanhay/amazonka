{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.GetOnPremisesInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about an on-premises instance.
module Network.AWS.CodeDeploy.GetOnPremisesInstance
  ( -- * Creating a request
    GetOnPremisesInstance (..),
    mkGetOnPremisesInstance,

    -- ** Request lenses
    gopiInstanceName,

    -- * Destructuring the response
    GetOnPremisesInstanceResponse (..),
    mkGetOnPremisesInstanceResponse,

    -- ** Response lenses
    gopirsInstanceInfo,
    gopirsResponseStatus,
  )
where

import Network.AWS.CodeDeploy.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input of a @GetOnPremisesInstance@ operation.
--
-- /See:/ 'mkGetOnPremisesInstance' smart constructor.
newtype GetOnPremisesInstance = GetOnPremisesInstance'
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

-- | Creates a value of 'GetOnPremisesInstance' with the minimum fields required to make a request.
--
-- * 'instanceName' - The name of the on-premises instance about which to get information.
mkGetOnPremisesInstance ::
  -- | 'instanceName'
  Lude.Text ->
  GetOnPremisesInstance
mkGetOnPremisesInstance pInstanceName_ =
  GetOnPremisesInstance' {instanceName = pInstanceName_}

-- | The name of the on-premises instance about which to get information.
--
-- /Note:/ Consider using 'instanceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gopiInstanceName :: Lens.Lens' GetOnPremisesInstance Lude.Text
gopiInstanceName = Lens.lens (instanceName :: GetOnPremisesInstance -> Lude.Text) (\s a -> s {instanceName = a} :: GetOnPremisesInstance)
{-# DEPRECATED gopiInstanceName "Use generic-lens or generic-optics with 'instanceName' instead." #-}

instance Lude.AWSRequest GetOnPremisesInstance where
  type Rs GetOnPremisesInstance = GetOnPremisesInstanceResponse
  request = Req.postJSON codeDeployService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetOnPremisesInstanceResponse'
            Lude.<$> (x Lude..?> "instanceInfo") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetOnPremisesInstance where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodeDeploy_20141006.GetOnPremisesInstance" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetOnPremisesInstance where
  toJSON GetOnPremisesInstance' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("instanceName" Lude..= instanceName)])

instance Lude.ToPath GetOnPremisesInstance where
  toPath = Lude.const "/"

instance Lude.ToQuery GetOnPremisesInstance where
  toQuery = Lude.const Lude.mempty

-- | Represents the output of a @GetOnPremisesInstance@ operation.
--
-- /See:/ 'mkGetOnPremisesInstanceResponse' smart constructor.
data GetOnPremisesInstanceResponse = GetOnPremisesInstanceResponse'
  { instanceInfo ::
      Lude.Maybe InstanceInfo,
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

-- | Creates a value of 'GetOnPremisesInstanceResponse' with the minimum fields required to make a request.
--
-- * 'instanceInfo' - Information about the on-premises instance.
-- * 'responseStatus' - The response status code.
mkGetOnPremisesInstanceResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetOnPremisesInstanceResponse
mkGetOnPremisesInstanceResponse pResponseStatus_ =
  GetOnPremisesInstanceResponse'
    { instanceInfo = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the on-premises instance.
--
-- /Note:/ Consider using 'instanceInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gopirsInstanceInfo :: Lens.Lens' GetOnPremisesInstanceResponse (Lude.Maybe InstanceInfo)
gopirsInstanceInfo = Lens.lens (instanceInfo :: GetOnPremisesInstanceResponse -> Lude.Maybe InstanceInfo) (\s a -> s {instanceInfo = a} :: GetOnPremisesInstanceResponse)
{-# DEPRECATED gopirsInstanceInfo "Use generic-lens or generic-optics with 'instanceInfo' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gopirsResponseStatus :: Lens.Lens' GetOnPremisesInstanceResponse Lude.Int
gopirsResponseStatus = Lens.lens (responseStatus :: GetOnPremisesInstanceResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetOnPremisesInstanceResponse)
{-# DEPRECATED gopirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
