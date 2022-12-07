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
-- Module      : Amazonka.IoTWireless.Types.UpdateFPorts
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTWireless.Types.UpdateFPorts where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTWireless.Types.Positioning
import qualified Amazonka.Prelude as Prelude

-- | Object for updating the FPorts information.
--
-- /See:/ 'newUpdateFPorts' smart constructor.
data UpdateFPorts = UpdateFPorts'
  { -- | Positioning FPorts for the ClockSync, Stream, and GNSS functions.
    positioning :: Prelude.Maybe Positioning
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateFPorts' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'positioning', 'updateFPorts_positioning' - Positioning FPorts for the ClockSync, Stream, and GNSS functions.
newUpdateFPorts ::
  UpdateFPorts
newUpdateFPorts =
  UpdateFPorts' {positioning = Prelude.Nothing}

-- | Positioning FPorts for the ClockSync, Stream, and GNSS functions.
updateFPorts_positioning :: Lens.Lens' UpdateFPorts (Prelude.Maybe Positioning)
updateFPorts_positioning = Lens.lens (\UpdateFPorts' {positioning} -> positioning) (\s@UpdateFPorts' {} a -> s {positioning = a} :: UpdateFPorts)

instance Prelude.Hashable UpdateFPorts where
  hashWithSalt _salt UpdateFPorts' {..} =
    _salt `Prelude.hashWithSalt` positioning

instance Prelude.NFData UpdateFPorts where
  rnf UpdateFPorts' {..} = Prelude.rnf positioning

instance Data.ToJSON UpdateFPorts where
  toJSON UpdateFPorts' {..} =
    Data.object
      ( Prelude.catMaybes
          [("Positioning" Data..=) Prelude.<$> positioning]
      )
