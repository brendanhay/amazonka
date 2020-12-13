{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.UpdateAPIKey
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an API key. The key can be updated while it is not deleted.
module Network.AWS.AppSync.UpdateAPIKey
  ( -- * Creating a request
    UpdateAPIKey (..),
    mkUpdateAPIKey,

    -- ** Request lenses
    uakApiId,
    uakExpires,
    uakId,
    uakDescription,

    -- * Destructuring the response
    UpdateAPIKeyResponse (..),
    mkUpdateAPIKeyResponse,

    -- ** Response lenses
    uakrsApiKey,
    uakrsResponseStatus,
  )
where

import Network.AWS.AppSync.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateAPIKey' smart constructor.
data UpdateAPIKey = UpdateAPIKey'
  { -- | The ID for the GraphQL API.
    apiId :: Lude.Text,
    -- | The time from update time after which the API key expires. The date is represented as seconds since the epoch. For more information, see .
    expires :: Lude.Maybe Lude.Integer,
    -- | The API key ID.
    id :: Lude.Text,
    -- | A description of the purpose of the API key.
    description :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateAPIKey' with the minimum fields required to make a request.
--
-- * 'apiId' - The ID for the GraphQL API.
-- * 'expires' - The time from update time after which the API key expires. The date is represented as seconds since the epoch. For more information, see .
-- * 'id' - The API key ID.
-- * 'description' - A description of the purpose of the API key.
mkUpdateAPIKey ::
  -- | 'apiId'
  Lude.Text ->
  -- | 'id'
  Lude.Text ->
  UpdateAPIKey
mkUpdateAPIKey pApiId_ pId_ =
  UpdateAPIKey'
    { apiId = pApiId_,
      expires = Lude.Nothing,
      id = pId_,
      description = Lude.Nothing
    }

-- | The ID for the GraphQL API.
--
-- /Note:/ Consider using 'apiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uakApiId :: Lens.Lens' UpdateAPIKey Lude.Text
uakApiId = Lens.lens (apiId :: UpdateAPIKey -> Lude.Text) (\s a -> s {apiId = a} :: UpdateAPIKey)
{-# DEPRECATED uakApiId "Use generic-lens or generic-optics with 'apiId' instead." #-}

-- | The time from update time after which the API key expires. The date is represented as seconds since the epoch. For more information, see .
--
-- /Note:/ Consider using 'expires' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uakExpires :: Lens.Lens' UpdateAPIKey (Lude.Maybe Lude.Integer)
uakExpires = Lens.lens (expires :: UpdateAPIKey -> Lude.Maybe Lude.Integer) (\s a -> s {expires = a} :: UpdateAPIKey)
{-# DEPRECATED uakExpires "Use generic-lens or generic-optics with 'expires' instead." #-}

-- | The API key ID.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uakId :: Lens.Lens' UpdateAPIKey Lude.Text
uakId = Lens.lens (id :: UpdateAPIKey -> Lude.Text) (\s a -> s {id = a} :: UpdateAPIKey)
{-# DEPRECATED uakId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | A description of the purpose of the API key.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uakDescription :: Lens.Lens' UpdateAPIKey (Lude.Maybe Lude.Text)
uakDescription = Lens.lens (description :: UpdateAPIKey -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: UpdateAPIKey)
{-# DEPRECATED uakDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.AWSRequest UpdateAPIKey where
  type Rs UpdateAPIKey = UpdateAPIKeyResponse
  request = Req.postJSON appSyncService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateAPIKeyResponse'
            Lude.<$> (x Lude..?> "apiKey") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateAPIKey where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateAPIKey where
  toJSON UpdateAPIKey' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("expires" Lude..=) Lude.<$> expires,
            ("description" Lude..=) Lude.<$> description
          ]
      )

instance Lude.ToPath UpdateAPIKey where
  toPath UpdateAPIKey' {..} =
    Lude.mconcat
      ["/v1/apis/", Lude.toBS apiId, "/apikeys/", Lude.toBS id]

instance Lude.ToQuery UpdateAPIKey where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateAPIKeyResponse' smart constructor.
data UpdateAPIKeyResponse = UpdateAPIKeyResponse'
  { -- | The API key.
    apiKey :: Lude.Maybe APIKey,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateAPIKeyResponse' with the minimum fields required to make a request.
--
-- * 'apiKey' - The API key.
-- * 'responseStatus' - The response status code.
mkUpdateAPIKeyResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateAPIKeyResponse
mkUpdateAPIKeyResponse pResponseStatus_ =
  UpdateAPIKeyResponse'
    { apiKey = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The API key.
--
-- /Note:/ Consider using 'apiKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uakrsApiKey :: Lens.Lens' UpdateAPIKeyResponse (Lude.Maybe APIKey)
uakrsApiKey = Lens.lens (apiKey :: UpdateAPIKeyResponse -> Lude.Maybe APIKey) (\s a -> s {apiKey = a} :: UpdateAPIKeyResponse)
{-# DEPRECATED uakrsApiKey "Use generic-lens or generic-optics with 'apiKey' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uakrsResponseStatus :: Lens.Lens' UpdateAPIKeyResponse Lude.Int
uakrsResponseStatus = Lens.lens (responseStatus :: UpdateAPIKeyResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateAPIKeyResponse)
{-# DEPRECATED uakrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
