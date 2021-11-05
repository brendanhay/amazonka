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
-- Module      : Network.AWS.Panorama.Types.NetworkPayload
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Panorama.Types.NetworkPayload where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Panorama.Types.EthernetPayload
import qualified Network.AWS.Prelude as Prelude

-- | The network configuration for a device.
--
-- /See:/ 'newNetworkPayload' smart constructor.
data NetworkPayload = NetworkPayload'
  { -- | Settings for Ethernet port 1.
    ethernet1 :: Prelude.Maybe EthernetPayload,
    -- | Settings for Ethernet port 0.
    ethernet0 :: Prelude.Maybe EthernetPayload
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NetworkPayload' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ethernet1', 'networkPayload_ethernet1' - Settings for Ethernet port 1.
--
-- 'ethernet0', 'networkPayload_ethernet0' - Settings for Ethernet port 0.
newNetworkPayload ::
  NetworkPayload
newNetworkPayload =
  NetworkPayload'
    { ethernet1 = Prelude.Nothing,
      ethernet0 = Prelude.Nothing
    }

-- | Settings for Ethernet port 1.
networkPayload_ethernet1 :: Lens.Lens' NetworkPayload (Prelude.Maybe EthernetPayload)
networkPayload_ethernet1 = Lens.lens (\NetworkPayload' {ethernet1} -> ethernet1) (\s@NetworkPayload' {} a -> s {ethernet1 = a} :: NetworkPayload)

-- | Settings for Ethernet port 0.
networkPayload_ethernet0 :: Lens.Lens' NetworkPayload (Prelude.Maybe EthernetPayload)
networkPayload_ethernet0 = Lens.lens (\NetworkPayload' {ethernet0} -> ethernet0) (\s@NetworkPayload' {} a -> s {ethernet0 = a} :: NetworkPayload)

instance Core.FromJSON NetworkPayload where
  parseJSON =
    Core.withObject
      "NetworkPayload"
      ( \x ->
          NetworkPayload'
            Prelude.<$> (x Core..:? "Ethernet1")
            Prelude.<*> (x Core..:? "Ethernet0")
      )

instance Prelude.Hashable NetworkPayload

instance Prelude.NFData NetworkPayload

instance Core.ToJSON NetworkPayload where
  toJSON NetworkPayload' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Ethernet1" Core..=) Prelude.<$> ethernet1,
            ("Ethernet0" Core..=) Prelude.<$> ethernet0
          ]
      )
