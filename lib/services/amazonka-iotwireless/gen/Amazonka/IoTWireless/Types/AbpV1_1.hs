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
-- Module      : Amazonka.IoTWireless.Types.AbpV1_1
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTWireless.Types.AbpV1_1 where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoTWireless.Types.SessionKeysAbpV1_1
import qualified Amazonka.Prelude as Prelude

-- | ABP device object for LoRaWAN specification v1.1
--
-- /See:/ 'newAbpV1_1' smart constructor.
data AbpV1_1 = AbpV1_1'
  { -- | Session keys for ABP v1.1
    sessionKeys :: Prelude.Maybe SessionKeysAbpV1_1,
    -- | The FCnt init value.
    fCntStart :: Prelude.Maybe Prelude.Natural,
    -- | The DevAddr value.
    devAddr :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AbpV1_1' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sessionKeys', 'abpV1_1_sessionKeys' - Session keys for ABP v1.1
--
-- 'fCntStart', 'abpV1_1_fCntStart' - The FCnt init value.
--
-- 'devAddr', 'abpV1_1_devAddr' - The DevAddr value.
newAbpV1_1 ::
  AbpV1_1
newAbpV1_1 =
  AbpV1_1'
    { sessionKeys = Prelude.Nothing,
      fCntStart = Prelude.Nothing,
      devAddr = Prelude.Nothing
    }

-- | Session keys for ABP v1.1
abpV1_1_sessionKeys :: Lens.Lens' AbpV1_1 (Prelude.Maybe SessionKeysAbpV1_1)
abpV1_1_sessionKeys = Lens.lens (\AbpV1_1' {sessionKeys} -> sessionKeys) (\s@AbpV1_1' {} a -> s {sessionKeys = a} :: AbpV1_1)

-- | The FCnt init value.
abpV1_1_fCntStart :: Lens.Lens' AbpV1_1 (Prelude.Maybe Prelude.Natural)
abpV1_1_fCntStart = Lens.lens (\AbpV1_1' {fCntStart} -> fCntStart) (\s@AbpV1_1' {} a -> s {fCntStart = a} :: AbpV1_1)

-- | The DevAddr value.
abpV1_1_devAddr :: Lens.Lens' AbpV1_1 (Prelude.Maybe Prelude.Text)
abpV1_1_devAddr = Lens.lens (\AbpV1_1' {devAddr} -> devAddr) (\s@AbpV1_1' {} a -> s {devAddr = a} :: AbpV1_1)

instance Core.FromJSON AbpV1_1 where
  parseJSON =
    Core.withObject
      "AbpV1_1"
      ( \x ->
          AbpV1_1'
            Prelude.<$> (x Core..:? "SessionKeys")
            Prelude.<*> (x Core..:? "FCntStart")
            Prelude.<*> (x Core..:? "DevAddr")
      )

instance Prelude.Hashable AbpV1_1 where
  hashWithSalt _salt AbpV1_1' {..} =
    _salt `Prelude.hashWithSalt` sessionKeys
      `Prelude.hashWithSalt` fCntStart
      `Prelude.hashWithSalt` devAddr

instance Prelude.NFData AbpV1_1 where
  rnf AbpV1_1' {..} =
    Prelude.rnf sessionKeys
      `Prelude.seq` Prelude.rnf fCntStart
      `Prelude.seq` Prelude.rnf devAddr

instance Core.ToJSON AbpV1_1 where
  toJSON AbpV1_1' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("SessionKeys" Core..=) Prelude.<$> sessionKeys,
            ("FCntStart" Core..=) Prelude.<$> fCntStart,
            ("DevAddr" Core..=) Prelude.<$> devAddr
          ]
      )
