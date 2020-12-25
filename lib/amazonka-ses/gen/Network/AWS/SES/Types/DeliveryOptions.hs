{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    doTlsPolicy,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SES.Types.TlsPolicy as Types

-- | Specifies whether messages that use the configuration set are required to use Transport Layer Security (TLS).
--
-- /See:/ 'mkDeliveryOptions' smart constructor.
newtype DeliveryOptions = DeliveryOptions'
  { -- | Specifies whether messages that use the configuration set are required to use Transport Layer Security (TLS). If the value is @Require@ , messages are only delivered if a TLS connection can be established. If the value is @Optional@ , messages can be delivered in plain text if a TLS connection can't be established.
    tlsPolicy :: Core.Maybe Types.TlsPolicy
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeliveryOptions' value with any optional fields omitted.
mkDeliveryOptions ::
  DeliveryOptions
mkDeliveryOptions = DeliveryOptions' {tlsPolicy = Core.Nothing}

-- | Specifies whether messages that use the configuration set are required to use Transport Layer Security (TLS). If the value is @Require@ , messages are only delivered if a TLS connection can be established. If the value is @Optional@ , messages can be delivered in plain text if a TLS connection can't be established.
--
-- /Note:/ Consider using 'tlsPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doTlsPolicy :: Lens.Lens' DeliveryOptions (Core.Maybe Types.TlsPolicy)
doTlsPolicy = Lens.field @"tlsPolicy"
{-# DEPRECATED doTlsPolicy "Use generic-lens or generic-optics with 'tlsPolicy' instead." #-}

instance Core.FromXML DeliveryOptions where
  parseXML x = DeliveryOptions' Core.<$> (x Core..@? "TlsPolicy")
