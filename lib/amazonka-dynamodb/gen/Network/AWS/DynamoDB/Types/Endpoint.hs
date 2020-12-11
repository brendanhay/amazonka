-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.Endpoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.Endpoint
  ( Endpoint (..),

    -- * Smart constructor
    mkEndpoint,

    -- * Lenses
    eAddress,
    eCachePeriodInMinutes,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An endpoint information details.
--
-- /See:/ 'mkEndpoint' smart constructor.
data Endpoint = Endpoint'
  { address :: Lude.Text,
    cachePeriodInMinutes :: Lude.Integer
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Endpoint' with the minimum fields required to make a request.
--
-- * 'address' - IP address of the endpoint.
-- * 'cachePeriodInMinutes' - Endpoint cache time to live (TTL) value.
mkEndpoint ::
  -- | 'address'
  Lude.Text ->
  -- | 'cachePeriodInMinutes'
  Lude.Integer ->
  Endpoint
mkEndpoint pAddress_ pCachePeriodInMinutes_ =
  Endpoint'
    { address = pAddress_,
      cachePeriodInMinutes = pCachePeriodInMinutes_
    }

-- | IP address of the endpoint.
--
-- /Note:/ Consider using 'address' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eAddress :: Lens.Lens' Endpoint Lude.Text
eAddress = Lens.lens (address :: Endpoint -> Lude.Text) (\s a -> s {address = a} :: Endpoint)
{-# DEPRECATED eAddress "Use generic-lens or generic-optics with 'address' instead." #-}

-- | Endpoint cache time to live (TTL) value.
--
-- /Note:/ Consider using 'cachePeriodInMinutes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eCachePeriodInMinutes :: Lens.Lens' Endpoint Lude.Integer
eCachePeriodInMinutes = Lens.lens (cachePeriodInMinutes :: Endpoint -> Lude.Integer) (\s a -> s {cachePeriodInMinutes = a} :: Endpoint)
{-# DEPRECATED eCachePeriodInMinutes "Use generic-lens or generic-optics with 'cachePeriodInMinutes' instead." #-}

instance Lude.FromJSON Endpoint where
  parseJSON =
    Lude.withObject
      "Endpoint"
      ( \x ->
          Endpoint'
            Lude.<$> (x Lude..: "Address") Lude.<*> (x Lude..: "CachePeriodInMinutes")
      )
