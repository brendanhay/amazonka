{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.DescribeResourceServer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a resource server.
module Network.AWS.CognitoIdentityProvider.DescribeResourceServer
  ( -- * Creating a request
    DescribeResourceServer (..),
    mkDescribeResourceServer,

    -- ** Request lenses
    desUserPoolId,
    desIdentifier,

    -- * Destructuring the response
    DescribeResourceServerResponse (..),
    mkDescribeResourceServerResponse,

    -- ** Response lenses
    drsrsResponseStatus,
    drsrsResourceServer,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeResourceServer' smart constructor.
data DescribeResourceServer = DescribeResourceServer'
  { userPoolId ::
      Lude.Text,
    identifier :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeResourceServer' with the minimum fields required to make a request.
--
-- * 'identifier' - The identifier for the resource server
-- * 'userPoolId' - The user pool ID for the user pool that hosts the resource server.
mkDescribeResourceServer ::
  -- | 'userPoolId'
  Lude.Text ->
  -- | 'identifier'
  Lude.Text ->
  DescribeResourceServer
mkDescribeResourceServer pUserPoolId_ pIdentifier_ =
  DescribeResourceServer'
    { userPoolId = pUserPoolId_,
      identifier = pIdentifier_
    }

-- | The user pool ID for the user pool that hosts the resource server.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desUserPoolId :: Lens.Lens' DescribeResourceServer Lude.Text
desUserPoolId = Lens.lens (userPoolId :: DescribeResourceServer -> Lude.Text) (\s a -> s {userPoolId = a} :: DescribeResourceServer)
{-# DEPRECATED desUserPoolId "Use generic-lens or generic-optics with 'userPoolId' instead." #-}

-- | The identifier for the resource server
--
-- /Note:/ Consider using 'identifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desIdentifier :: Lens.Lens' DescribeResourceServer Lude.Text
desIdentifier = Lens.lens (identifier :: DescribeResourceServer -> Lude.Text) (\s a -> s {identifier = a} :: DescribeResourceServer)
{-# DEPRECATED desIdentifier "Use generic-lens or generic-optics with 'identifier' instead." #-}

instance Lude.AWSRequest DescribeResourceServer where
  type Rs DescribeResourceServer = DescribeResourceServerResponse
  request = Req.postJSON cognitoIdentityProviderService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeResourceServerResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..:> "ResourceServer")
      )

instance Lude.ToHeaders DescribeResourceServer where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSCognitoIdentityProviderService.DescribeResourceServer" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeResourceServer where
  toJSON DescribeResourceServer' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("UserPoolId" Lude..= userPoolId),
            Lude.Just ("Identifier" Lude..= identifier)
          ]
      )

instance Lude.ToPath DescribeResourceServer where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeResourceServer where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeResourceServerResponse' smart constructor.
data DescribeResourceServerResponse = DescribeResourceServerResponse'
  { responseStatus ::
      Lude.Int,
    resourceServer ::
      ResourceServerType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeResourceServerResponse' with the minimum fields required to make a request.
--
-- * 'resourceServer' - The resource server.
-- * 'responseStatus' - The response status code.
mkDescribeResourceServerResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'resourceServer'
  ResourceServerType ->
  DescribeResourceServerResponse
mkDescribeResourceServerResponse pResponseStatus_ pResourceServer_ =
  DescribeResourceServerResponse'
    { responseStatus =
        pResponseStatus_,
      resourceServer = pResourceServer_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsrsResponseStatus :: Lens.Lens' DescribeResourceServerResponse Lude.Int
drsrsResponseStatus = Lens.lens (responseStatus :: DescribeResourceServerResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeResourceServerResponse)
{-# DEPRECATED drsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | The resource server.
--
-- /Note:/ Consider using 'resourceServer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsrsResourceServer :: Lens.Lens' DescribeResourceServerResponse ResourceServerType
drsrsResourceServer = Lens.lens (resourceServer :: DescribeResourceServerResponse -> ResourceServerType) (\s a -> s {resourceServer = a} :: DescribeResourceServerResponse)
{-# DEPRECATED drsrsResourceServer "Use generic-lens or generic-optics with 'resourceServer' instead." #-}
