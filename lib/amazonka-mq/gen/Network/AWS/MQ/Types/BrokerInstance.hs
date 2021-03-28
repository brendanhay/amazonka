{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MQ.Types.BrokerInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MQ.Types.BrokerInstance
  ( BrokerInstance (..)
  -- * Smart constructor
  , mkBrokerInstance
  -- * Lenses
  , biConsoleURL
  , biEndpoints
  , biIpAddress
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Returns information about all brokers.
--
-- /See:/ 'mkBrokerInstance' smart constructor.
data BrokerInstance = BrokerInstance'
  { consoleURL :: Core.Maybe Core.Text
    -- ^ The URL of the broker's Web Console.
  , endpoints :: Core.Maybe [Core.Text]
    -- ^ The broker's wire-level protocol endpoints.
  , ipAddress :: Core.Maybe Core.Text
    -- ^ The IP address of the Elastic Network Interface (ENI) attached to the broker. Does not apply to RabbitMQ brokers
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BrokerInstance' value with any optional fields omitted.
mkBrokerInstance
    :: BrokerInstance
mkBrokerInstance
  = BrokerInstance'{consoleURL = Core.Nothing,
                    endpoints = Core.Nothing, ipAddress = Core.Nothing}

-- | The URL of the broker's Web Console.
--
-- /Note:/ Consider using 'consoleURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
biConsoleURL :: Lens.Lens' BrokerInstance (Core.Maybe Core.Text)
biConsoleURL = Lens.field @"consoleURL"
{-# INLINEABLE biConsoleURL #-}
{-# DEPRECATED consoleURL "Use generic-lens or generic-optics with 'consoleURL' instead"  #-}

-- | The broker's wire-level protocol endpoints.
--
-- /Note:/ Consider using 'endpoints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
biEndpoints :: Lens.Lens' BrokerInstance (Core.Maybe [Core.Text])
biEndpoints = Lens.field @"endpoints"
{-# INLINEABLE biEndpoints #-}
{-# DEPRECATED endpoints "Use generic-lens or generic-optics with 'endpoints' instead"  #-}

-- | The IP address of the Elastic Network Interface (ENI) attached to the broker. Does not apply to RabbitMQ brokers
--
-- /Note:/ Consider using 'ipAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
biIpAddress :: Lens.Lens' BrokerInstance (Core.Maybe Core.Text)
biIpAddress = Lens.field @"ipAddress"
{-# INLINEABLE biIpAddress #-}
{-# DEPRECATED ipAddress "Use generic-lens or generic-optics with 'ipAddress' instead"  #-}

instance Core.FromJSON BrokerInstance where
        parseJSON
          = Core.withObject "BrokerInstance" Core.$
              \ x ->
                BrokerInstance' Core.<$>
                  (x Core..:? "consoleURL") Core.<*> x Core..:? "endpoints" Core.<*>
                    x Core..:? "ipAddress"
