{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MQ.Types.BrokerInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MQ.Types.BrokerInstance
  ( BrokerInstance (..),

    -- * Smart constructor
    mkBrokerInstance,

    -- * Lenses
    biIPAddress,
    biConsoleURL,
    biEndpoints,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Returns information about all brokers.
--
-- /See:/ 'mkBrokerInstance' smart constructor.
data BrokerInstance = BrokerInstance'
  { -- | The IP address of the Elastic Network Interface (ENI) attached to the broker. Does not apply to RabbitMQ brokers
    ipAddress :: Lude.Maybe Lude.Text,
    -- | The URL of the broker's Web Console.
    consoleURL :: Lude.Maybe Lude.Text,
    -- | The broker's wire-level protocol endpoints.
    endpoints :: Lude.Maybe [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BrokerInstance' with the minimum fields required to make a request.
--
-- * 'ipAddress' - The IP address of the Elastic Network Interface (ENI) attached to the broker. Does not apply to RabbitMQ brokers
-- * 'consoleURL' - The URL of the broker's Web Console.
-- * 'endpoints' - The broker's wire-level protocol endpoints.
mkBrokerInstance ::
  BrokerInstance
mkBrokerInstance =
  BrokerInstance'
    { ipAddress = Lude.Nothing,
      consoleURL = Lude.Nothing,
      endpoints = Lude.Nothing
    }

-- | The IP address of the Elastic Network Interface (ENI) attached to the broker. Does not apply to RabbitMQ brokers
--
-- /Note:/ Consider using 'ipAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
biIPAddress :: Lens.Lens' BrokerInstance (Lude.Maybe Lude.Text)
biIPAddress = Lens.lens (ipAddress :: BrokerInstance -> Lude.Maybe Lude.Text) (\s a -> s {ipAddress = a} :: BrokerInstance)
{-# DEPRECATED biIPAddress "Use generic-lens or generic-optics with 'ipAddress' instead." #-}

-- | The URL of the broker's Web Console.
--
-- /Note:/ Consider using 'consoleURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
biConsoleURL :: Lens.Lens' BrokerInstance (Lude.Maybe Lude.Text)
biConsoleURL = Lens.lens (consoleURL :: BrokerInstance -> Lude.Maybe Lude.Text) (\s a -> s {consoleURL = a} :: BrokerInstance)
{-# DEPRECATED biConsoleURL "Use generic-lens or generic-optics with 'consoleURL' instead." #-}

-- | The broker's wire-level protocol endpoints.
--
-- /Note:/ Consider using 'endpoints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
biEndpoints :: Lens.Lens' BrokerInstance (Lude.Maybe [Lude.Text])
biEndpoints = Lens.lens (endpoints :: BrokerInstance -> Lude.Maybe [Lude.Text]) (\s a -> s {endpoints = a} :: BrokerInstance)
{-# DEPRECATED biEndpoints "Use generic-lens or generic-optics with 'endpoints' instead." #-}

instance Lude.FromJSON BrokerInstance where
  parseJSON =
    Lude.withObject
      "BrokerInstance"
      ( \x ->
          BrokerInstance'
            Lude.<$> (x Lude..:? "ipAddress")
            Lude.<*> (x Lude..:? "consoleURL")
            Lude.<*> (x Lude..:? "endpoints" Lude..!= Lude.mempty)
      )
