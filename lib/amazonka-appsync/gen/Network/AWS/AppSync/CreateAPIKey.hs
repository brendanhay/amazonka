{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.CreateAPIKey
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a unique key that you can distribute to clients who are executing your API.
module Network.AWS.AppSync.CreateAPIKey
  ( -- * Creating a request
    CreateAPIKey (..),
    mkCreateAPIKey,

    -- ** Request lenses
    cakExpires,
    cakDescription,
    cakApiId,

    -- * Destructuring the response
    CreateAPIKeyResponse (..),
    mkCreateAPIKeyResponse,

    -- ** Response lenses
    cakrsApiKey,
    cakrsResponseStatus,
  )
where

import Network.AWS.AppSync.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateAPIKey' smart constructor.
data CreateAPIKey = CreateAPIKey'
  { expires ::
      Lude.Maybe Lude.Integer,
    description :: Lude.Maybe Lude.Text,
    apiId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateAPIKey' with the minimum fields required to make a request.
--
-- * 'apiId' - The ID for your GraphQL API.
-- * 'description' - A description of the purpose of the API key.
-- * 'expires' - The time from creation time after which the API key expires. The date is represented as seconds since the epoch, rounded down to the nearest hour. The default value for this parameter is 7 days from creation time. For more information, see .
mkCreateAPIKey ::
  -- | 'apiId'
  Lude.Text ->
  CreateAPIKey
mkCreateAPIKey pApiId_ =
  CreateAPIKey'
    { expires = Lude.Nothing,
      description = Lude.Nothing,
      apiId = pApiId_
    }

-- | The time from creation time after which the API key expires. The date is represented as seconds since the epoch, rounded down to the nearest hour. The default value for this parameter is 7 days from creation time. For more information, see .
--
-- /Note:/ Consider using 'expires' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cakExpires :: Lens.Lens' CreateAPIKey (Lude.Maybe Lude.Integer)
cakExpires = Lens.lens (expires :: CreateAPIKey -> Lude.Maybe Lude.Integer) (\s a -> s {expires = a} :: CreateAPIKey)
{-# DEPRECATED cakExpires "Use generic-lens or generic-optics with 'expires' instead." #-}

-- | A description of the purpose of the API key.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cakDescription :: Lens.Lens' CreateAPIKey (Lude.Maybe Lude.Text)
cakDescription = Lens.lens (description :: CreateAPIKey -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: CreateAPIKey)
{-# DEPRECATED cakDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The ID for your GraphQL API.
--
-- /Note:/ Consider using 'apiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cakApiId :: Lens.Lens' CreateAPIKey Lude.Text
cakApiId = Lens.lens (apiId :: CreateAPIKey -> Lude.Text) (\s a -> s {apiId = a} :: CreateAPIKey)
{-# DEPRECATED cakApiId "Use generic-lens or generic-optics with 'apiId' instead." #-}

instance Lude.AWSRequest CreateAPIKey where
  type Rs CreateAPIKey = CreateAPIKeyResponse
  request = Req.postJSON appSyncService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateAPIKeyResponse'
            Lude.<$> (x Lude..?> "apiKey") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateAPIKey where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateAPIKey where
  toJSON CreateAPIKey' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("expires" Lude..=) Lude.<$> expires,
            ("description" Lude..=) Lude.<$> description
          ]
      )

instance Lude.ToPath CreateAPIKey where
  toPath CreateAPIKey' {..} =
    Lude.mconcat ["/v1/apis/", Lude.toBS apiId, "/apikeys"]

instance Lude.ToQuery CreateAPIKey where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateAPIKeyResponse' smart constructor.
data CreateAPIKeyResponse = CreateAPIKeyResponse'
  { apiKey ::
      Lude.Maybe APIKey,
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

-- | Creates a value of 'CreateAPIKeyResponse' with the minimum fields required to make a request.
--
-- * 'apiKey' - The API key.
-- * 'responseStatus' - The response status code.
mkCreateAPIKeyResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateAPIKeyResponse
mkCreateAPIKeyResponse pResponseStatus_ =
  CreateAPIKeyResponse'
    { apiKey = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The API key.
--
-- /Note:/ Consider using 'apiKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cakrsApiKey :: Lens.Lens' CreateAPIKeyResponse (Lude.Maybe APIKey)
cakrsApiKey = Lens.lens (apiKey :: CreateAPIKeyResponse -> Lude.Maybe APIKey) (\s a -> s {apiKey = a} :: CreateAPIKeyResponse)
{-# DEPRECATED cakrsApiKey "Use generic-lens or generic-optics with 'apiKey' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cakrsResponseStatus :: Lens.Lens' CreateAPIKeyResponse Lude.Int
cakrsResponseStatus = Lens.lens (responseStatus :: CreateAPIKeyResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateAPIKeyResponse)
{-# DEPRECATED cakrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
