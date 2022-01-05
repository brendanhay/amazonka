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
-- Module      : Amazonka.IoTWireless.Types.OtaaV1_0_x
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTWireless.Types.OtaaV1_0_x where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | OTAA device object for v1.0.x
--
-- /See:/ 'newOtaaV1_0_x' smart constructor.
data OtaaV1_0_x = OtaaV1_0_x'
  { -- | The AppEUI value.
    appEui :: Prelude.Maybe Prelude.Text,
    -- | The AppKey value.
    appKey :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OtaaV1_0_x' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appEui', 'otaaV1_0_x_appEui' - The AppEUI value.
--
-- 'appKey', 'otaaV1_0_x_appKey' - The AppKey value.
newOtaaV1_0_x ::
  OtaaV1_0_x
newOtaaV1_0_x =
  OtaaV1_0_x'
    { appEui = Prelude.Nothing,
      appKey = Prelude.Nothing
    }

-- | The AppEUI value.
otaaV1_0_x_appEui :: Lens.Lens' OtaaV1_0_x (Prelude.Maybe Prelude.Text)
otaaV1_0_x_appEui = Lens.lens (\OtaaV1_0_x' {appEui} -> appEui) (\s@OtaaV1_0_x' {} a -> s {appEui = a} :: OtaaV1_0_x)

-- | The AppKey value.
otaaV1_0_x_appKey :: Lens.Lens' OtaaV1_0_x (Prelude.Maybe Prelude.Text)
otaaV1_0_x_appKey = Lens.lens (\OtaaV1_0_x' {appKey} -> appKey) (\s@OtaaV1_0_x' {} a -> s {appKey = a} :: OtaaV1_0_x)

instance Core.FromJSON OtaaV1_0_x where
  parseJSON =
    Core.withObject
      "OtaaV1_0_x"
      ( \x ->
          OtaaV1_0_x'
            Prelude.<$> (x Core..:? "AppEui")
            Prelude.<*> (x Core..:? "AppKey")
      )

instance Prelude.Hashable OtaaV1_0_x where
  hashWithSalt _salt OtaaV1_0_x' {..} =
    _salt `Prelude.hashWithSalt` appEui
      `Prelude.hashWithSalt` appKey

instance Prelude.NFData OtaaV1_0_x where
  rnf OtaaV1_0_x' {..} =
    Prelude.rnf appEui `Prelude.seq` Prelude.rnf appKey

instance Core.ToJSON OtaaV1_0_x where
  toJSON OtaaV1_0_x' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("AppEui" Core..=) Prelude.<$> appEui,
            ("AppKey" Core..=) Prelude.<$> appKey
          ]
      )
