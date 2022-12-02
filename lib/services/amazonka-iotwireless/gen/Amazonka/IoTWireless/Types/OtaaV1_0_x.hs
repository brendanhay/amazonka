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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTWireless.Types.OtaaV1_0_x where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | OTAA device object for v1.0.x
--
-- /See:/ 'newOtaaV1_0_x' smart constructor.
data OtaaV1_0_x = OtaaV1_0_x'
  { -- | The GenAppKey value.
    genAppKey :: Prelude.Maybe Prelude.Text,
    -- | The AppKey value.
    appKey :: Prelude.Maybe Prelude.Text,
    -- | The AppEUI value.
    appEui :: Prelude.Maybe Prelude.Text
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
-- 'genAppKey', 'otaaV1_0_x_genAppKey' - The GenAppKey value.
--
-- 'appKey', 'otaaV1_0_x_appKey' - The AppKey value.
--
-- 'appEui', 'otaaV1_0_x_appEui' - The AppEUI value.
newOtaaV1_0_x ::
  OtaaV1_0_x
newOtaaV1_0_x =
  OtaaV1_0_x'
    { genAppKey = Prelude.Nothing,
      appKey = Prelude.Nothing,
      appEui = Prelude.Nothing
    }

-- | The GenAppKey value.
otaaV1_0_x_genAppKey :: Lens.Lens' OtaaV1_0_x (Prelude.Maybe Prelude.Text)
otaaV1_0_x_genAppKey = Lens.lens (\OtaaV1_0_x' {genAppKey} -> genAppKey) (\s@OtaaV1_0_x' {} a -> s {genAppKey = a} :: OtaaV1_0_x)

-- | The AppKey value.
otaaV1_0_x_appKey :: Lens.Lens' OtaaV1_0_x (Prelude.Maybe Prelude.Text)
otaaV1_0_x_appKey = Lens.lens (\OtaaV1_0_x' {appKey} -> appKey) (\s@OtaaV1_0_x' {} a -> s {appKey = a} :: OtaaV1_0_x)

-- | The AppEUI value.
otaaV1_0_x_appEui :: Lens.Lens' OtaaV1_0_x (Prelude.Maybe Prelude.Text)
otaaV1_0_x_appEui = Lens.lens (\OtaaV1_0_x' {appEui} -> appEui) (\s@OtaaV1_0_x' {} a -> s {appEui = a} :: OtaaV1_0_x)

instance Data.FromJSON OtaaV1_0_x where
  parseJSON =
    Data.withObject
      "OtaaV1_0_x"
      ( \x ->
          OtaaV1_0_x'
            Prelude.<$> (x Data..:? "GenAppKey")
            Prelude.<*> (x Data..:? "AppKey")
            Prelude.<*> (x Data..:? "AppEui")
      )

instance Prelude.Hashable OtaaV1_0_x where
  hashWithSalt _salt OtaaV1_0_x' {..} =
    _salt `Prelude.hashWithSalt` genAppKey
      `Prelude.hashWithSalt` appKey
      `Prelude.hashWithSalt` appEui

instance Prelude.NFData OtaaV1_0_x where
  rnf OtaaV1_0_x' {..} =
    Prelude.rnf genAppKey
      `Prelude.seq` Prelude.rnf appKey
      `Prelude.seq` Prelude.rnf appEui

instance Data.ToJSON OtaaV1_0_x where
  toJSON OtaaV1_0_x' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("GenAppKey" Data..=) Prelude.<$> genAppKey,
            ("AppKey" Data..=) Prelude.<$> appKey,
            ("AppEui" Data..=) Prelude.<$> appEui
          ]
      )
