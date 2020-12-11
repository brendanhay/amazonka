{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.UpdateRoutingProfileConcurrency
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the channels that agents can handle in the Contact Control Panel (CCP) for a routing profile.
module Network.AWS.Connect.UpdateRoutingProfileConcurrency
  ( -- * Creating a request
    UpdateRoutingProfileConcurrency (..),
    mkUpdateRoutingProfileConcurrency,

    -- ** Request lenses
    urpcInstanceId,
    urpcRoutingProfileId,
    urpcMediaConcurrencies,

    -- * Destructuring the response
    UpdateRoutingProfileConcurrencyResponse (..),
    mkUpdateRoutingProfileConcurrencyResponse,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateRoutingProfileConcurrency' smart constructor.
data UpdateRoutingProfileConcurrency = UpdateRoutingProfileConcurrency'
  { instanceId ::
      Lude.Text,
    routingProfileId ::
      Lude.Text,
    mediaConcurrencies ::
      [MediaConcurrency]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateRoutingProfileConcurrency' with the minimum fields required to make a request.
--
-- * 'instanceId' - The identifier of the Amazon Connect instance.
-- * 'mediaConcurrencies' - The channels agents can handle in the Contact Control Panel (CCP).
-- * 'routingProfileId' - The identifier of the routing profile.
mkUpdateRoutingProfileConcurrency ::
  -- | 'instanceId'
  Lude.Text ->
  -- | 'routingProfileId'
  Lude.Text ->
  UpdateRoutingProfileConcurrency
mkUpdateRoutingProfileConcurrency pInstanceId_ pRoutingProfileId_ =
  UpdateRoutingProfileConcurrency'
    { instanceId = pInstanceId_,
      routingProfileId = pRoutingProfileId_,
      mediaConcurrencies = Lude.mempty
    }

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urpcInstanceId :: Lens.Lens' UpdateRoutingProfileConcurrency Lude.Text
urpcInstanceId = Lens.lens (instanceId :: UpdateRoutingProfileConcurrency -> Lude.Text) (\s a -> s {instanceId = a} :: UpdateRoutingProfileConcurrency)
{-# DEPRECATED urpcInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The identifier of the routing profile.
--
-- /Note:/ Consider using 'routingProfileId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urpcRoutingProfileId :: Lens.Lens' UpdateRoutingProfileConcurrency Lude.Text
urpcRoutingProfileId = Lens.lens (routingProfileId :: UpdateRoutingProfileConcurrency -> Lude.Text) (\s a -> s {routingProfileId = a} :: UpdateRoutingProfileConcurrency)
{-# DEPRECATED urpcRoutingProfileId "Use generic-lens or generic-optics with 'routingProfileId' instead." #-}

-- | The channels agents can handle in the Contact Control Panel (CCP).
--
-- /Note:/ Consider using 'mediaConcurrencies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urpcMediaConcurrencies :: Lens.Lens' UpdateRoutingProfileConcurrency [MediaConcurrency]
urpcMediaConcurrencies = Lens.lens (mediaConcurrencies :: UpdateRoutingProfileConcurrency -> [MediaConcurrency]) (\s a -> s {mediaConcurrencies = a} :: UpdateRoutingProfileConcurrency)
{-# DEPRECATED urpcMediaConcurrencies "Use generic-lens or generic-optics with 'mediaConcurrencies' instead." #-}

instance Lude.AWSRequest UpdateRoutingProfileConcurrency where
  type
    Rs UpdateRoutingProfileConcurrency =
      UpdateRoutingProfileConcurrencyResponse
  request = Req.postJSON connectService
  response = Res.receiveNull UpdateRoutingProfileConcurrencyResponse'

instance Lude.ToHeaders UpdateRoutingProfileConcurrency where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateRoutingProfileConcurrency where
  toJSON UpdateRoutingProfileConcurrency' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("MediaConcurrencies" Lude..= mediaConcurrencies)]
      )

instance Lude.ToPath UpdateRoutingProfileConcurrency where
  toPath UpdateRoutingProfileConcurrency' {..} =
    Lude.mconcat
      [ "/routing-profiles/",
        Lude.toBS instanceId,
        "/",
        Lude.toBS routingProfileId,
        "/concurrency"
      ]

instance Lude.ToQuery UpdateRoutingProfileConcurrency where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateRoutingProfileConcurrencyResponse' smart constructor.
data UpdateRoutingProfileConcurrencyResponse = UpdateRoutingProfileConcurrencyResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateRoutingProfileConcurrencyResponse' with the minimum fields required to make a request.
mkUpdateRoutingProfileConcurrencyResponse ::
  UpdateRoutingProfileConcurrencyResponse
mkUpdateRoutingProfileConcurrencyResponse =
  UpdateRoutingProfileConcurrencyResponse'
