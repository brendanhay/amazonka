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
-- Module      : Amazonka.IoTWireless.Types.AbpV1_0_x
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTWireless.Types.AbpV1_0_x where

import qualified Amazonka.Core as Core
import Amazonka.IoTWireless.Types.SessionKeysAbpV1_0_x
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | ABP device object for LoRaWAN specification v1.0.x
--
-- /See:/ 'newAbpV1_0_x' smart constructor.
data AbpV1_0_x = AbpV1_0_x'
  { -- | The DevAddr value.
    devAddr :: Prelude.Maybe Prelude.Text,
    -- | Session keys for ABP v1.0.x
    sessionKeys :: Prelude.Maybe SessionKeysAbpV1_0_x
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AbpV1_0_x' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'devAddr', 'abpV1_0_x_devAddr' - The DevAddr value.
--
-- 'sessionKeys', 'abpV1_0_x_sessionKeys' - Session keys for ABP v1.0.x
newAbpV1_0_x ::
  AbpV1_0_x
newAbpV1_0_x =
  AbpV1_0_x'
    { devAddr = Prelude.Nothing,
      sessionKeys = Prelude.Nothing
    }

-- | The DevAddr value.
abpV1_0_x_devAddr :: Lens.Lens' AbpV1_0_x (Prelude.Maybe Prelude.Text)
abpV1_0_x_devAddr = Lens.lens (\AbpV1_0_x' {devAddr} -> devAddr) (\s@AbpV1_0_x' {} a -> s {devAddr = a} :: AbpV1_0_x)

-- | Session keys for ABP v1.0.x
abpV1_0_x_sessionKeys :: Lens.Lens' AbpV1_0_x (Prelude.Maybe SessionKeysAbpV1_0_x)
abpV1_0_x_sessionKeys = Lens.lens (\AbpV1_0_x' {sessionKeys} -> sessionKeys) (\s@AbpV1_0_x' {} a -> s {sessionKeys = a} :: AbpV1_0_x)

instance Core.FromJSON AbpV1_0_x where
  parseJSON =
    Core.withObject
      "AbpV1_0_x"
      ( \x ->
          AbpV1_0_x'
            Prelude.<$> (x Core..:? "DevAddr")
            Prelude.<*> (x Core..:? "SessionKeys")
      )

instance Prelude.Hashable AbpV1_0_x

instance Prelude.NFData AbpV1_0_x

instance Core.ToJSON AbpV1_0_x where
  toJSON AbpV1_0_x' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("DevAddr" Core..=) Prelude.<$> devAddr,
            ("SessionKeys" Core..=) Prelude.<$> sessionKeys
          ]
      )
