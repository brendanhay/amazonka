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
-- Module      : Amazonka.IoTWireless.Types.ImportedWirelessDevice
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTWireless.Types.ImportedWirelessDevice where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTWireless.Types.ImportedSidewalkDevice
import qualified Amazonka.Prelude as Prelude

-- | Information about a wireless device that has been added to an import
-- task.
--
-- /See:/ 'newImportedWirelessDevice' smart constructor.
data ImportedWirelessDevice = ImportedWirelessDevice'
  { -- | The Sidewalk-related information about a device that has been added to
    -- an import task.
    sidewalk :: Prelude.Maybe ImportedSidewalkDevice
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImportedWirelessDevice' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sidewalk', 'importedWirelessDevice_sidewalk' - The Sidewalk-related information about a device that has been added to
-- an import task.
newImportedWirelessDevice ::
  ImportedWirelessDevice
newImportedWirelessDevice =
  ImportedWirelessDevice' {sidewalk = Prelude.Nothing}

-- | The Sidewalk-related information about a device that has been added to
-- an import task.
importedWirelessDevice_sidewalk :: Lens.Lens' ImportedWirelessDevice (Prelude.Maybe ImportedSidewalkDevice)
importedWirelessDevice_sidewalk = Lens.lens (\ImportedWirelessDevice' {sidewalk} -> sidewalk) (\s@ImportedWirelessDevice' {} a -> s {sidewalk = a} :: ImportedWirelessDevice)

instance Data.FromJSON ImportedWirelessDevice where
  parseJSON =
    Data.withObject
      "ImportedWirelessDevice"
      ( \x ->
          ImportedWirelessDevice'
            Prelude.<$> (x Data..:? "Sidewalk")
      )

instance Prelude.Hashable ImportedWirelessDevice where
  hashWithSalt _salt ImportedWirelessDevice' {..} =
    _salt `Prelude.hashWithSalt` sidewalk

instance Prelude.NFData ImportedWirelessDevice where
  rnf ImportedWirelessDevice' {..} =
    Prelude.rnf sidewalk
