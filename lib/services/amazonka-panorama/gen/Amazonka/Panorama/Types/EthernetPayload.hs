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
-- Module      : Amazonka.Panorama.Types.EthernetPayload
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Panorama.Types.EthernetPayload where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Panorama.Types.ConnectionType
import Amazonka.Panorama.Types.StaticIpConnectionInfo
import qualified Amazonka.Prelude as Prelude

-- | A device\'s network configuration.
--
-- /See:/ 'newEthernetPayload' smart constructor.
data EthernetPayload = EthernetPayload'
  { -- | Network configuration for a static IP connection.
    staticIpConnectionInfo :: Prelude.Maybe StaticIpConnectionInfo,
    -- | How the device gets an IP address.
    connectionType :: ConnectionType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EthernetPayload' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'staticIpConnectionInfo', 'ethernetPayload_staticIpConnectionInfo' - Network configuration for a static IP connection.
--
-- 'connectionType', 'ethernetPayload_connectionType' - How the device gets an IP address.
newEthernetPayload ::
  -- | 'connectionType'
  ConnectionType ->
  EthernetPayload
newEthernetPayload pConnectionType_ =
  EthernetPayload'
    { staticIpConnectionInfo =
        Prelude.Nothing,
      connectionType = pConnectionType_
    }

-- | Network configuration for a static IP connection.
ethernetPayload_staticIpConnectionInfo :: Lens.Lens' EthernetPayload (Prelude.Maybe StaticIpConnectionInfo)
ethernetPayload_staticIpConnectionInfo = Lens.lens (\EthernetPayload' {staticIpConnectionInfo} -> staticIpConnectionInfo) (\s@EthernetPayload' {} a -> s {staticIpConnectionInfo = a} :: EthernetPayload)

-- | How the device gets an IP address.
ethernetPayload_connectionType :: Lens.Lens' EthernetPayload ConnectionType
ethernetPayload_connectionType = Lens.lens (\EthernetPayload' {connectionType} -> connectionType) (\s@EthernetPayload' {} a -> s {connectionType = a} :: EthernetPayload)

instance Data.FromJSON EthernetPayload where
  parseJSON =
    Data.withObject
      "EthernetPayload"
      ( \x ->
          EthernetPayload'
            Prelude.<$> (x Data..:? "StaticIpConnectionInfo")
            Prelude.<*> (x Data..: "ConnectionType")
      )

instance Prelude.Hashable EthernetPayload where
  hashWithSalt _salt EthernetPayload' {..} =
    _salt
      `Prelude.hashWithSalt` staticIpConnectionInfo
      `Prelude.hashWithSalt` connectionType

instance Prelude.NFData EthernetPayload where
  rnf EthernetPayload' {..} =
    Prelude.rnf staticIpConnectionInfo
      `Prelude.seq` Prelude.rnf connectionType

instance Data.ToJSON EthernetPayload where
  toJSON EthernetPayload' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("StaticIpConnectionInfo" Data..=)
              Prelude.<$> staticIpConnectionInfo,
            Prelude.Just
              ("ConnectionType" Data..= connectionType)
          ]
      )
