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
-- Module      : Amazonka.IoTWireless.Types.UpdateAbpV1_0_x
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTWireless.Types.UpdateAbpV1_0_x where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | ABP device object for LoRaWAN specification v1.0.x
--
-- /See:/ 'newUpdateAbpV1_0_x' smart constructor.
data UpdateAbpV1_0_x = UpdateAbpV1_0_x'
  { -- | The FCnt init value.
    fCntStart :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateAbpV1_0_x' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fCntStart', 'updateAbpV1_0_x_fCntStart' - The FCnt init value.
newUpdateAbpV1_0_x ::
  UpdateAbpV1_0_x
newUpdateAbpV1_0_x =
  UpdateAbpV1_0_x' {fCntStart = Prelude.Nothing}

-- | The FCnt init value.
updateAbpV1_0_x_fCntStart :: Lens.Lens' UpdateAbpV1_0_x (Prelude.Maybe Prelude.Natural)
updateAbpV1_0_x_fCntStart = Lens.lens (\UpdateAbpV1_0_x' {fCntStart} -> fCntStart) (\s@UpdateAbpV1_0_x' {} a -> s {fCntStart = a} :: UpdateAbpV1_0_x)

instance Prelude.Hashable UpdateAbpV1_0_x where
  hashWithSalt _salt UpdateAbpV1_0_x' {..} =
    _salt `Prelude.hashWithSalt` fCntStart

instance Prelude.NFData UpdateAbpV1_0_x where
  rnf UpdateAbpV1_0_x' {..} = Prelude.rnf fCntStart

instance Data.ToJSON UpdateAbpV1_0_x where
  toJSON UpdateAbpV1_0_x' {..} =
    Data.object
      ( Prelude.catMaybes
          [("FCntStart" Data..=) Prelude.<$> fCntStart]
      )
