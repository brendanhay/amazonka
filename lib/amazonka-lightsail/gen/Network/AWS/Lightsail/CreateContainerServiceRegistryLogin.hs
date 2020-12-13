{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.CreateContainerServiceRegistryLogin
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a temporary set of log in credentials that you can use to log in to the Docker process on your local machine. After you're logged in, you can use the native Docker commands to push your local container images to the container image registry of your Amazon Lightsail account so that you can use them with your Lightsail container service. The log in credentials expire 12 hours after they are created, at which point you will need to create a new set of log in credentials.
--
-- After you push your container images to the container image registry of your Lightsail account, use the @RegisterContainerImage@ action to register the pushed images to a specific Lightsail container service.
module Network.AWS.Lightsail.CreateContainerServiceRegistryLogin
  ( -- * Creating a request
    CreateContainerServiceRegistryLogin (..),
    mkCreateContainerServiceRegistryLogin,

    -- * Destructuring the response
    CreateContainerServiceRegistryLoginResponse (..),
    mkCreateContainerServiceRegistryLoginResponse,

    -- ** Response lenses
    ccsrlrsRegistryLogin,
    ccsrlrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateContainerServiceRegistryLogin' smart constructor.
data CreateContainerServiceRegistryLogin = CreateContainerServiceRegistryLogin'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateContainerServiceRegistryLogin' with the minimum fields required to make a request.
mkCreateContainerServiceRegistryLogin ::
  CreateContainerServiceRegistryLogin
mkCreateContainerServiceRegistryLogin =
  CreateContainerServiceRegistryLogin'

instance Lude.AWSRequest CreateContainerServiceRegistryLogin where
  type
    Rs CreateContainerServiceRegistryLogin =
      CreateContainerServiceRegistryLoginResponse
  request = Req.postJSON lightsailService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateContainerServiceRegistryLoginResponse'
            Lude.<$> (x Lude..?> "registryLogin")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateContainerServiceRegistryLogin where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "Lightsail_20161128.CreateContainerServiceRegistryLogin" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateContainerServiceRegistryLogin where
  toJSON = Lude.const (Lude.Object Lude.mempty)

instance Lude.ToPath CreateContainerServiceRegistryLogin where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateContainerServiceRegistryLogin where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateContainerServiceRegistryLoginResponse' smart constructor.
data CreateContainerServiceRegistryLoginResponse = CreateContainerServiceRegistryLoginResponse'
  { -- | An object that describes the log in information for the container service registry of your Lightsail account.
    registryLogin :: Lude.Maybe ContainerServiceRegistryLogin,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateContainerServiceRegistryLoginResponse' with the minimum fields required to make a request.
--
-- * 'registryLogin' - An object that describes the log in information for the container service registry of your Lightsail account.
-- * 'responseStatus' - The response status code.
mkCreateContainerServiceRegistryLoginResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateContainerServiceRegistryLoginResponse
mkCreateContainerServiceRegistryLoginResponse pResponseStatus_ =
  CreateContainerServiceRegistryLoginResponse'
    { registryLogin =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An object that describes the log in information for the container service registry of your Lightsail account.
--
-- /Note:/ Consider using 'registryLogin' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsrlrsRegistryLogin :: Lens.Lens' CreateContainerServiceRegistryLoginResponse (Lude.Maybe ContainerServiceRegistryLogin)
ccsrlrsRegistryLogin = Lens.lens (registryLogin :: CreateContainerServiceRegistryLoginResponse -> Lude.Maybe ContainerServiceRegistryLogin) (\s a -> s {registryLogin = a} :: CreateContainerServiceRegistryLoginResponse)
{-# DEPRECATED ccsrlrsRegistryLogin "Use generic-lens or generic-optics with 'registryLogin' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsrlrsResponseStatus :: Lens.Lens' CreateContainerServiceRegistryLoginResponse Lude.Int
ccsrlrsResponseStatus = Lens.lens (responseStatus :: CreateContainerServiceRegistryLoginResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateContainerServiceRegistryLoginResponse)
{-# DEPRECATED ccsrlrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
