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
-- Module      : Amazonka.IoTWireless.Types.SidewalkSingleStartImportInfo
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTWireless.Types.SidewalkSingleStartImportInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about an import task created for an individual Sidewalk
-- device.
--
-- /See:/ 'newSidewalkSingleStartImportInfo' smart constructor.
data SidewalkSingleStartImportInfo = SidewalkSingleStartImportInfo'
  { -- | The Sidewalk manufacturing serial number (SMSN) of the device added to
    -- the import task.
    sidewalkManufacturingSn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SidewalkSingleStartImportInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sidewalkManufacturingSn', 'sidewalkSingleStartImportInfo_sidewalkManufacturingSn' - The Sidewalk manufacturing serial number (SMSN) of the device added to
-- the import task.
newSidewalkSingleStartImportInfo ::
  SidewalkSingleStartImportInfo
newSidewalkSingleStartImportInfo =
  SidewalkSingleStartImportInfo'
    { sidewalkManufacturingSn =
        Prelude.Nothing
    }

-- | The Sidewalk manufacturing serial number (SMSN) of the device added to
-- the import task.
sidewalkSingleStartImportInfo_sidewalkManufacturingSn :: Lens.Lens' SidewalkSingleStartImportInfo (Prelude.Maybe Prelude.Text)
sidewalkSingleStartImportInfo_sidewalkManufacturingSn = Lens.lens (\SidewalkSingleStartImportInfo' {sidewalkManufacturingSn} -> sidewalkManufacturingSn) (\s@SidewalkSingleStartImportInfo' {} a -> s {sidewalkManufacturingSn = a} :: SidewalkSingleStartImportInfo)

instance
  Prelude.Hashable
    SidewalkSingleStartImportInfo
  where
  hashWithSalt _salt SidewalkSingleStartImportInfo' {..} =
    _salt
      `Prelude.hashWithSalt` sidewalkManufacturingSn

instance Prelude.NFData SidewalkSingleStartImportInfo where
  rnf SidewalkSingleStartImportInfo' {..} =
    Prelude.rnf sidewalkManufacturingSn

instance Data.ToJSON SidewalkSingleStartImportInfo where
  toJSON SidewalkSingleStartImportInfo' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("SidewalkManufacturingSn" Data..=)
              Prelude.<$> sidewalkManufacturingSn
          ]
      )
