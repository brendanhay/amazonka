{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.DeleteAPIKey
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the 'ApiKey' resource.
module Network.AWS.APIGateway.DeleteAPIKey
  ( -- * Creating a request
    DeleteAPIKey (..),
    mkDeleteAPIKey,

    -- ** Request lenses
    dakApiKey,

    -- * Destructuring the response
    DeleteAPIKeyResponse (..),
    mkDeleteAPIKeyResponse,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | A request to delete the 'ApiKey' resource.
--
-- /See:/ 'mkDeleteAPIKey' smart constructor.
newtype DeleteAPIKey = DeleteAPIKey' {apiKey :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteAPIKey' with the minimum fields required to make a request.
--
-- * 'apiKey' - [Required] The identifier of the 'ApiKey' resource to be deleted.
mkDeleteAPIKey ::
  -- | 'apiKey'
  Lude.Text ->
  DeleteAPIKey
mkDeleteAPIKey pApiKey_ = DeleteAPIKey' {apiKey = pApiKey_}

-- | [Required] The identifier of the 'ApiKey' resource to be deleted.
--
-- /Note:/ Consider using 'apiKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dakApiKey :: Lens.Lens' DeleteAPIKey Lude.Text
dakApiKey = Lens.lens (apiKey :: DeleteAPIKey -> Lude.Text) (\s a -> s {apiKey = a} :: DeleteAPIKey)
{-# DEPRECATED dakApiKey "Use generic-lens or generic-optics with 'apiKey' instead." #-}

instance Lude.AWSRequest DeleteAPIKey where
  type Rs DeleteAPIKey = DeleteAPIKeyResponse
  request = Req.delete apiGatewayService
  response = Res.receiveNull DeleteAPIKeyResponse'

instance Lude.ToHeaders DeleteAPIKey where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          ["Accept" Lude.=# ("application/json" :: Lude.ByteString)]
      )

instance Lude.ToPath DeleteAPIKey where
  toPath DeleteAPIKey' {..} =
    Lude.mconcat ["/apikeys/", Lude.toBS apiKey]

instance Lude.ToQuery DeleteAPIKey where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteAPIKeyResponse' smart constructor.
data DeleteAPIKeyResponse = DeleteAPIKeyResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteAPIKeyResponse' with the minimum fields required to make a request.
mkDeleteAPIKeyResponse ::
  DeleteAPIKeyResponse
mkDeleteAPIKeyResponse = DeleteAPIKeyResponse'
