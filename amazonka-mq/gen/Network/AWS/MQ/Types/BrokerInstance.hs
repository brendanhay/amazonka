{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Returns information about all brokers.
--
-- /See:/ 'newBrokerInstance' smart constructor.
data BrokerInstance = BrokerInstance'
  { -- | The broker\'s wire-level protocol endpoints.
    endpoints :: Prelude.Maybe [Prelude.Text],
    -- | The IP address of the Elastic Network Interface (ENI) attached to the
    -- broker. Does not apply to RabbitMQ brokers
    ipAddress :: Prelude.Maybe Prelude.Text,
    -- | The URL of the broker\'s Web Console.
    consoleURL :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { endpoints = Prelude.Nothing,
      ipAddress = Prelude.Nothing,
      consoleURL = Prelude.Nothing
    }

-- | The broker\'s wire-level protocol endpoints.
brokerInstance_endpoints :: Lens.Lens' BrokerInstance (Prelude.Maybe [Prelude.Text])
brokerInstance_endpoints = Lens.lens (\BrokerInstance' {endpoints} -> endpoints) (\s@BrokerInstance' {} a -> s {endpoints = a} :: BrokerInstance) Prelude.. Lens.mapping Prelude._Coerce

-- | The IP address of the Elastic Network Interface (ENI) attached to the
-- broker. Does not apply to RabbitMQ brokers
brokerInstance_ipAddress :: Lens.Lens' BrokerInstance (Prelude.Maybe Prelude.Text)
brokerInstance_ipAddress = Lens.lens (\BrokerInstance' {ipAddress} -> ipAddress) (\s@BrokerInstance' {} a -> s {ipAddress = a} :: BrokerInstance)

-- | The URL of the broker\'s Web Console.
brokerInstance_consoleURL :: Lens.Lens' BrokerInstance (Prelude.Maybe Prelude.Text)
brokerInstance_consoleURL = Lens.lens (\BrokerInstance' {consoleURL} -> consoleURL) (\s@BrokerInstance' {} a -> s {consoleURL = a} :: BrokerInstance)

instance Prelude.FromJSON BrokerInstance where
  parseJSON =
    Prelude.withObject
      "BrokerInstance"
      ( \x ->
          BrokerInstance'
            Prelude.<$> ( x Prelude..:? "endpoints"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "ipAddress")
            Prelude.<*> (x Prelude..:? "consoleURL")
      )

instance Prelude.Hashable BrokerInstance

instance Prelude.NFData BrokerInstance
