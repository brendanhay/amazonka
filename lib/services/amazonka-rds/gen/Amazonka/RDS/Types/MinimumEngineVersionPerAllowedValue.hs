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
-- Module      : Amazonka.RDS.Types.MinimumEngineVersionPerAllowedValue
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RDS.Types.MinimumEngineVersionPerAllowedValue where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The minimum DB engine version required for each corresponding allowed
-- value for an option setting.
--
-- /See:/ 'newMinimumEngineVersionPerAllowedValue' smart constructor.
data MinimumEngineVersionPerAllowedValue = MinimumEngineVersionPerAllowedValue'
  { -- | The minimum DB engine version required for the allowed value.
    minimumEngineVersion :: Prelude.Maybe Prelude.Text,
    -- | The allowed value for an option setting.
    allowedValue :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MinimumEngineVersionPerAllowedValue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'minimumEngineVersion', 'minimumEngineVersionPerAllowedValue_minimumEngineVersion' - The minimum DB engine version required for the allowed value.
--
-- 'allowedValue', 'minimumEngineVersionPerAllowedValue_allowedValue' - The allowed value for an option setting.
newMinimumEngineVersionPerAllowedValue ::
  MinimumEngineVersionPerAllowedValue
newMinimumEngineVersionPerAllowedValue =
  MinimumEngineVersionPerAllowedValue'
    { minimumEngineVersion =
        Prelude.Nothing,
      allowedValue = Prelude.Nothing
    }

-- | The minimum DB engine version required for the allowed value.
minimumEngineVersionPerAllowedValue_minimumEngineVersion :: Lens.Lens' MinimumEngineVersionPerAllowedValue (Prelude.Maybe Prelude.Text)
minimumEngineVersionPerAllowedValue_minimumEngineVersion = Lens.lens (\MinimumEngineVersionPerAllowedValue' {minimumEngineVersion} -> minimumEngineVersion) (\s@MinimumEngineVersionPerAllowedValue' {} a -> s {minimumEngineVersion = a} :: MinimumEngineVersionPerAllowedValue)

-- | The allowed value for an option setting.
minimumEngineVersionPerAllowedValue_allowedValue :: Lens.Lens' MinimumEngineVersionPerAllowedValue (Prelude.Maybe Prelude.Text)
minimumEngineVersionPerAllowedValue_allowedValue = Lens.lens (\MinimumEngineVersionPerAllowedValue' {allowedValue} -> allowedValue) (\s@MinimumEngineVersionPerAllowedValue' {} a -> s {allowedValue = a} :: MinimumEngineVersionPerAllowedValue)

instance
  Data.FromXML
    MinimumEngineVersionPerAllowedValue
  where
  parseXML x =
    MinimumEngineVersionPerAllowedValue'
      Prelude.<$> (x Data..@? "MinimumEngineVersion")
      Prelude.<*> (x Data..@? "AllowedValue")

instance
  Prelude.Hashable
    MinimumEngineVersionPerAllowedValue
  where
  hashWithSalt
    _salt
    MinimumEngineVersionPerAllowedValue' {..} =
      _salt `Prelude.hashWithSalt` minimumEngineVersion
        `Prelude.hashWithSalt` allowedValue

instance
  Prelude.NFData
    MinimumEngineVersionPerAllowedValue
  where
  rnf MinimumEngineVersionPerAllowedValue' {..} =
    Prelude.rnf minimumEngineVersion
      `Prelude.seq` Prelude.rnf allowedValue
