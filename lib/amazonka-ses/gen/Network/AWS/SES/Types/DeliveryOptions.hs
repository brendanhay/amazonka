-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types.DeliveryOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SES.Types.DeliveryOptions
  ( DeliveryOptions (..),

    -- * Smart constructor
    mkDeliveryOptions,

    -- * Lenses
    doTLSPolicy,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SES.Types.TLSPolicy

-- | Specifies whether messages that use the configuration set are required to use Transport Layer Security (TLS).
--
-- /See:/ 'mkDeliveryOptions' smart constructor.
newtype DeliveryOptions = DeliveryOptions'
  { tlsPolicy ::
      Lude.Maybe TLSPolicy
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeliveryOptions' with the minimum fields required to make a request.
--
-- * 'tlsPolicy' - Specifies whether messages that use the configuration set are required to use Transport Layer Security (TLS). If the value is @Require@ , messages are only delivered if a TLS connection can be established. If the value is @Optional@ , messages can be delivered in plain text if a TLS connection can't be established.
mkDeliveryOptions ::
  DeliveryOptions
mkDeliveryOptions = DeliveryOptions' {tlsPolicy = Lude.Nothing}

-- | Specifies whether messages that use the configuration set are required to use Transport Layer Security (TLS). If the value is @Require@ , messages are only delivered if a TLS connection can be established. If the value is @Optional@ , messages can be delivered in plain text if a TLS connection can't be established.
--
-- /Note:/ Consider using 'tlsPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doTLSPolicy :: Lens.Lens' DeliveryOptions (Lude.Maybe TLSPolicy)
doTLSPolicy = Lens.lens (tlsPolicy :: DeliveryOptions -> Lude.Maybe TLSPolicy) (\s a -> s {tlsPolicy = a} :: DeliveryOptions)
{-# DEPRECATED doTLSPolicy "Use generic-lens or generic-optics with 'tlsPolicy' instead." #-}

instance Lude.FromXML DeliveryOptions where
  parseXML x = DeliveryOptions' Lude.<$> (x Lude..@? "TlsPolicy")

instance Lude.ToQuery DeliveryOptions where
  toQuery DeliveryOptions' {..} =
    Lude.mconcat ["TlsPolicy" Lude.=: tlsPolicy]
