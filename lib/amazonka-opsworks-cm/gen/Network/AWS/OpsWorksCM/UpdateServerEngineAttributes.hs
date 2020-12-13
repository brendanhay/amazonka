{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorksCM.UpdateServerEngineAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates engine-specific attributes on a specified server. The server enters the @MODIFYING@ state when this operation is in progress. Only one update can occur at a time. You can use this command to reset a Chef server's public key (@CHEF_PIVOTAL_KEY@ ) or a Puppet server's admin password (@PUPPET_ADMIN_PASSWORD@ ).
--
-- This operation is asynchronous.
-- This operation can only be called for servers in @HEALTHY@ or @UNHEALTHY@ states. Otherwise, an @InvalidStateException@ is raised. A @ResourceNotFoundException@ is thrown when the server does not exist. A @ValidationException@ is raised when parameters of the request are not valid.
module Network.AWS.OpsWorksCM.UpdateServerEngineAttributes
  ( -- * Creating a request
    UpdateServerEngineAttributes (..),
    mkUpdateServerEngineAttributes,

    -- ** Request lenses
    useaAttributeValue,
    useaServerName,
    useaAttributeName,

    -- * Destructuring the response
    UpdateServerEngineAttributesResponse (..),
    mkUpdateServerEngineAttributesResponse,

    -- ** Response lenses
    usearsServer,
    usearsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorksCM.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateServerEngineAttributes' smart constructor.
data UpdateServerEngineAttributes = UpdateServerEngineAttributes'
  { -- | The value to set for the attribute.
    attributeValue :: Lude.Maybe Lude.Text,
    -- | The name of the server to update.
    serverName :: Lude.Text,
    -- | The name of the engine attribute to update.
    attributeName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateServerEngineAttributes' with the minimum fields required to make a request.
--
-- * 'attributeValue' - The value to set for the attribute.
-- * 'serverName' - The name of the server to update.
-- * 'attributeName' - The name of the engine attribute to update.
mkUpdateServerEngineAttributes ::
  -- | 'serverName'
  Lude.Text ->
  -- | 'attributeName'
  Lude.Text ->
  UpdateServerEngineAttributes
mkUpdateServerEngineAttributes pServerName_ pAttributeName_ =
  UpdateServerEngineAttributes'
    { attributeValue = Lude.Nothing,
      serverName = pServerName_,
      attributeName = pAttributeName_
    }

-- | The value to set for the attribute.
--
-- /Note:/ Consider using 'attributeValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
useaAttributeValue :: Lens.Lens' UpdateServerEngineAttributes (Lude.Maybe Lude.Text)
useaAttributeValue = Lens.lens (attributeValue :: UpdateServerEngineAttributes -> Lude.Maybe Lude.Text) (\s a -> s {attributeValue = a} :: UpdateServerEngineAttributes)
{-# DEPRECATED useaAttributeValue "Use generic-lens or generic-optics with 'attributeValue' instead." #-}

-- | The name of the server to update.
--
-- /Note:/ Consider using 'serverName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
useaServerName :: Lens.Lens' UpdateServerEngineAttributes Lude.Text
useaServerName = Lens.lens (serverName :: UpdateServerEngineAttributes -> Lude.Text) (\s a -> s {serverName = a} :: UpdateServerEngineAttributes)
{-# DEPRECATED useaServerName "Use generic-lens or generic-optics with 'serverName' instead." #-}

-- | The name of the engine attribute to update.
--
-- /Note:/ Consider using 'attributeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
useaAttributeName :: Lens.Lens' UpdateServerEngineAttributes Lude.Text
useaAttributeName = Lens.lens (attributeName :: UpdateServerEngineAttributes -> Lude.Text) (\s a -> s {attributeName = a} :: UpdateServerEngineAttributes)
{-# DEPRECATED useaAttributeName "Use generic-lens or generic-optics with 'attributeName' instead." #-}

instance Lude.AWSRequest UpdateServerEngineAttributes where
  type
    Rs UpdateServerEngineAttributes =
      UpdateServerEngineAttributesResponse
  request = Req.postJSON opsWorksCMService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateServerEngineAttributesResponse'
            Lude.<$> (x Lude..?> "Server") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateServerEngineAttributes where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "OpsWorksCM_V2016_11_01.UpdateServerEngineAttributes" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateServerEngineAttributes where
  toJSON UpdateServerEngineAttributes' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("AttributeValue" Lude..=) Lude.<$> attributeValue,
            Lude.Just ("ServerName" Lude..= serverName),
            Lude.Just ("AttributeName" Lude..= attributeName)
          ]
      )

instance Lude.ToPath UpdateServerEngineAttributes where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateServerEngineAttributes where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateServerEngineAttributesResponse' smart constructor.
data UpdateServerEngineAttributesResponse = UpdateServerEngineAttributesResponse'
  { -- | Contains the response to an @UpdateServerEngineAttributes@ request.
    server :: Lude.Maybe Server,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateServerEngineAttributesResponse' with the minimum fields required to make a request.
--
-- * 'server' - Contains the response to an @UpdateServerEngineAttributes@ request.
-- * 'responseStatus' - The response status code.
mkUpdateServerEngineAttributesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateServerEngineAttributesResponse
mkUpdateServerEngineAttributesResponse pResponseStatus_ =
  UpdateServerEngineAttributesResponse'
    { server = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Contains the response to an @UpdateServerEngineAttributes@ request.
--
-- /Note:/ Consider using 'server' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usearsServer :: Lens.Lens' UpdateServerEngineAttributesResponse (Lude.Maybe Server)
usearsServer = Lens.lens (server :: UpdateServerEngineAttributesResponse -> Lude.Maybe Server) (\s a -> s {server = a} :: UpdateServerEngineAttributesResponse)
{-# DEPRECATED usearsServer "Use generic-lens or generic-optics with 'server' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usearsResponseStatus :: Lens.Lens' UpdateServerEngineAttributesResponse Lude.Int
usearsResponseStatus = Lens.lens (responseStatus :: UpdateServerEngineAttributesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateServerEngineAttributesResponse)
{-# DEPRECATED usearsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
