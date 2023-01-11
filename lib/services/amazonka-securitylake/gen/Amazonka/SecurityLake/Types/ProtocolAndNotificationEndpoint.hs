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
-- Module      : Amazonka.SecurityLake.Types.ProtocolAndNotificationEndpoint
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityLake.Types.ProtocolAndNotificationEndpoint where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Protocol used in Amazon Security Lake that dictates how notifications
-- are posted at the endpoint.
--
-- /See:/ 'newProtocolAndNotificationEndpoint' smart constructor.
data ProtocolAndNotificationEndpoint = ProtocolAndNotificationEndpoint'
  { -- | The account that is subscribed to receive exception notifications.
    endpoint :: Prelude.Maybe Prelude.Text,
    -- | The protocol to which notification messages are posted.
    protocol :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ProtocolAndNotificationEndpoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endpoint', 'protocolAndNotificationEndpoint_endpoint' - The account that is subscribed to receive exception notifications.
--
-- 'protocol', 'protocolAndNotificationEndpoint_protocol' - The protocol to which notification messages are posted.
newProtocolAndNotificationEndpoint ::
  ProtocolAndNotificationEndpoint
newProtocolAndNotificationEndpoint =
  ProtocolAndNotificationEndpoint'
    { endpoint =
        Prelude.Nothing,
      protocol = Prelude.Nothing
    }

-- | The account that is subscribed to receive exception notifications.
protocolAndNotificationEndpoint_endpoint :: Lens.Lens' ProtocolAndNotificationEndpoint (Prelude.Maybe Prelude.Text)
protocolAndNotificationEndpoint_endpoint = Lens.lens (\ProtocolAndNotificationEndpoint' {endpoint} -> endpoint) (\s@ProtocolAndNotificationEndpoint' {} a -> s {endpoint = a} :: ProtocolAndNotificationEndpoint)

-- | The protocol to which notification messages are posted.
protocolAndNotificationEndpoint_protocol :: Lens.Lens' ProtocolAndNotificationEndpoint (Prelude.Maybe Prelude.Text)
protocolAndNotificationEndpoint_protocol = Lens.lens (\ProtocolAndNotificationEndpoint' {protocol} -> protocol) (\s@ProtocolAndNotificationEndpoint' {} a -> s {protocol = a} :: ProtocolAndNotificationEndpoint)

instance
  Data.FromJSON
    ProtocolAndNotificationEndpoint
  where
  parseJSON =
    Data.withObject
      "ProtocolAndNotificationEndpoint"
      ( \x ->
          ProtocolAndNotificationEndpoint'
            Prelude.<$> (x Data..:? "endpoint")
            Prelude.<*> (x Data..:? "protocol")
      )

instance
  Prelude.Hashable
    ProtocolAndNotificationEndpoint
  where
  hashWithSalt
    _salt
    ProtocolAndNotificationEndpoint' {..} =
      _salt `Prelude.hashWithSalt` endpoint
        `Prelude.hashWithSalt` protocol

instance
  Prelude.NFData
    ProtocolAndNotificationEndpoint
  where
  rnf ProtocolAndNotificationEndpoint' {..} =
    Prelude.rnf endpoint
      `Prelude.seq` Prelude.rnf protocol
