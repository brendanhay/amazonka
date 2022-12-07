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
-- Module      : Amazonka.IoTWireless.Types.UpdateAbpV1_1
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTWireless.Types.UpdateAbpV1_1 where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | ABP device object for LoRaWAN specification v1.1
--
-- /See:/ 'newUpdateAbpV1_1' smart constructor.
data UpdateAbpV1_1 = UpdateAbpV1_1'
  { -- | The FCnt init value.
    fCntStart :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateAbpV1_1' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fCntStart', 'updateAbpV1_1_fCntStart' - The FCnt init value.
newUpdateAbpV1_1 ::
  UpdateAbpV1_1
newUpdateAbpV1_1 =
  UpdateAbpV1_1' {fCntStart = Prelude.Nothing}

-- | The FCnt init value.
updateAbpV1_1_fCntStart :: Lens.Lens' UpdateAbpV1_1 (Prelude.Maybe Prelude.Natural)
updateAbpV1_1_fCntStart = Lens.lens (\UpdateAbpV1_1' {fCntStart} -> fCntStart) (\s@UpdateAbpV1_1' {} a -> s {fCntStart = a} :: UpdateAbpV1_1)

instance Prelude.Hashable UpdateAbpV1_1 where
  hashWithSalt _salt UpdateAbpV1_1' {..} =
    _salt `Prelude.hashWithSalt` fCntStart

instance Prelude.NFData UpdateAbpV1_1 where
  rnf UpdateAbpV1_1' {..} = Prelude.rnf fCntStart

instance Data.ToJSON UpdateAbpV1_1 where
  toJSON UpdateAbpV1_1' {..} =
    Data.object
      ( Prelude.catMaybes
          [("FCntStart" Data..=) Prelude.<$> fCntStart]
      )
