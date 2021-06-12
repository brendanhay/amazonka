{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MQ.Types.BrokerInstance
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MQ.Types.BrokerInstance where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Returns information about all brokers.
--
-- /See:/ 'newBrokerInstance' smart constructor.
data BrokerInstance = BrokerInstance'
  { -- | The broker\'s wire-level protocol endpoints.
    endpoints :: Core.Maybe [Core.Text],
    -- | The IP address of the Elastic Network Interface (ENI) attached to the
    -- broker. Does not apply to RabbitMQ brokers
    ipAddress :: Core.Maybe Core.Text,
    -- | The URL of the broker\'s Web Console.
    consoleURL :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'BrokerInstance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endpoints', 'brokerInstance_endpoints' - The broker\'s wire-level protocol endpoints.
--
-- 'ipAddress', 'brokerInstance_ipAddress' - The IP address of the Elastic Network Interface (ENI) attached to the
-- broker. Does not apply to RabbitMQ brokers
--
-- 'consoleURL', 'brokerInstance_consoleURL' - The URL of the broker\'s Web Console.
newBrokerInstance ::
  BrokerInstance
newBrokerInstance =
  BrokerInstance'
    { endpoints = Core.Nothing,
      ipAddress = Core.Nothing,
      consoleURL = Core.Nothing
    }

-- | The broker\'s wire-level protocol endpoints.
brokerInstance_endpoints :: Lens.Lens' BrokerInstance (Core.Maybe [Core.Text])
brokerInstance_endpoints = Lens.lens (\BrokerInstance' {endpoints} -> endpoints) (\s@BrokerInstance' {} a -> s {endpoints = a} :: BrokerInstance) Core.. Lens.mapping Lens._Coerce

-- | The IP address of the Elastic Network Interface (ENI) attached to the
-- broker. Does not apply to RabbitMQ brokers
brokerInstance_ipAddress :: Lens.Lens' BrokerInstance (Core.Maybe Core.Text)
brokerInstance_ipAddress = Lens.lens (\BrokerInstance' {ipAddress} -> ipAddress) (\s@BrokerInstance' {} a -> s {ipAddress = a} :: BrokerInstance)

-- | The URL of the broker\'s Web Console.
brokerInstance_consoleURL :: Lens.Lens' BrokerInstance (Core.Maybe Core.Text)
brokerInstance_consoleURL = Lens.lens (\BrokerInstance' {consoleURL} -> consoleURL) (\s@BrokerInstance' {} a -> s {consoleURL = a} :: BrokerInstance)

instance Core.FromJSON BrokerInstance where
  parseJSON =
    Core.withObject
      "BrokerInstance"
      ( \x ->
          BrokerInstance'
            Core.<$> (x Core..:? "endpoints" Core..!= Core.mempty)
            Core.<*> (x Core..:? "ipAddress")
            Core.<*> (x Core..:? "consoleURL")
      )

instance Core.Hashable BrokerInstance

instance Core.NFData BrokerInstance
