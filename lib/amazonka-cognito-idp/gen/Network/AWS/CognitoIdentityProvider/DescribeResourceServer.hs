{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    dUserPoolId,
    dIdentifier,

    -- * Destructuring the response
    DescribeResourceServerResponse (..),
    mkDescribeResourceServerResponse,

    -- ** Response lenses
    drsrsResourceServer,
    drsrsResponseStatus,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeResourceServer' smart constructor.
data DescribeResourceServer = DescribeResourceServer'
  { -- | The user pool ID for the user pool that hosts the resource server.
    userPoolId :: Lude.Text,
    -- | The identifier for the resource server
    identifier :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeResourceServer' with the minimum fields required to make a request.
--
-- * 'userPoolId' - The user pool ID for the user pool that hosts the resource server.
-- * 'identifier' - The identifier for the resource server
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
dUserPoolId :: Lens.Lens' DescribeResourceServer Lude.Text
dUserPoolId = Lens.lens (userPoolId :: DescribeResourceServer -> Lude.Text) (\s a -> s {userPoolId = a} :: DescribeResourceServer)
{-# DEPRECATED dUserPoolId "Use generic-lens or generic-optics with 'userPoolId' instead." #-}

-- | The identifier for the resource server
--
-- /Note:/ Consider using 'identifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dIdentifier :: Lens.Lens' DescribeResourceServer Lude.Text
dIdentifier = Lens.lens (identifier :: DescribeResourceServer -> Lude.Text) (\s a -> s {identifier = a} :: DescribeResourceServer)
{-# DEPRECATED dIdentifier "Use generic-lens or generic-optics with 'identifier' instead." #-}

instance Lude.AWSRequest DescribeResourceServer where
  type Rs DescribeResourceServer = DescribeResourceServerResponse
  request = Req.postJSON cognitoIdentityProviderService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeResourceServerResponse'
            Lude.<$> (x Lude..:> "ResourceServer")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
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
  { -- | The resource server.
    resourceServer :: ResourceServerType,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeResourceServerResponse' with the minimum fields required to make a request.
--
-- * 'resourceServer' - The resource server.
-- * 'responseStatus' - The response status code.
mkDescribeResourceServerResponse ::
  -- | 'resourceServer'
  ResourceServerType ->
  -- | 'responseStatus'
  Lude.Int ->
  DescribeResourceServerResponse
mkDescribeResourceServerResponse pResourceServer_ pResponseStatus_ =
  DescribeResourceServerResponse'
    { resourceServer =
        pResourceServer_,
      responseStatus = pResponseStatus_
    }

-- | The resource server.
--
-- /Note:/ Consider using 'resourceServer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsrsResourceServer :: Lens.Lens' DescribeResourceServerResponse ResourceServerType
drsrsResourceServer = Lens.lens (resourceServer :: DescribeResourceServerResponse -> ResourceServerType) (\s a -> s {resourceServer = a} :: DescribeResourceServerResponse)
{-# DEPRECATED drsrsResourceServer "Use generic-lens or generic-optics with 'resourceServer' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsrsResponseStatus :: Lens.Lens' DescribeResourceServerResponse Lude.Int
drsrsResponseStatus = Lens.lens (responseStatus :: DescribeResourceServerResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeResourceServerResponse)
{-# DEPRECATED drsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
