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
-- Module      : Network.AWS.RDS.Types.MinimumEngineVersionPerAllowedValue
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.MinimumEngineVersionPerAllowedValue where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The minimum DB engine version required for each corresponding allowed
-- value for an option setting.
--
-- /See:/ 'newMinimumEngineVersionPerAllowedValue' smart constructor.
data MinimumEngineVersionPerAllowedValue = MinimumEngineVersionPerAllowedValue'
  { -- | The allowed value for an option setting.
    allowedValue :: Core.Maybe Core.Text,
    -- | The minimum DB engine version required for the allowed value.
    minimumEngineVersion :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'MinimumEngineVersionPerAllowedValue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'allowedValue', 'minimumEngineVersionPerAllowedValue_allowedValue' - The allowed value for an option setting.
--
-- 'minimumEngineVersion', 'minimumEngineVersionPerAllowedValue_minimumEngineVersion' - The minimum DB engine version required for the allowed value.
newMinimumEngineVersionPerAllowedValue ::
  MinimumEngineVersionPerAllowedValue
newMinimumEngineVersionPerAllowedValue =
  MinimumEngineVersionPerAllowedValue'
    { allowedValue =
        Core.Nothing,
      minimumEngineVersion = Core.Nothing
    }

-- | The allowed value for an option setting.
minimumEngineVersionPerAllowedValue_allowedValue :: Lens.Lens' MinimumEngineVersionPerAllowedValue (Core.Maybe Core.Text)
minimumEngineVersionPerAllowedValue_allowedValue = Lens.lens (\MinimumEngineVersionPerAllowedValue' {allowedValue} -> allowedValue) (\s@MinimumEngineVersionPerAllowedValue' {} a -> s {allowedValue = a} :: MinimumEngineVersionPerAllowedValue)

-- | The minimum DB engine version required for the allowed value.
minimumEngineVersionPerAllowedValue_minimumEngineVersion :: Lens.Lens' MinimumEngineVersionPerAllowedValue (Core.Maybe Core.Text)
minimumEngineVersionPerAllowedValue_minimumEngineVersion = Lens.lens (\MinimumEngineVersionPerAllowedValue' {minimumEngineVersion} -> minimumEngineVersion) (\s@MinimumEngineVersionPerAllowedValue' {} a -> s {minimumEngineVersion = a} :: MinimumEngineVersionPerAllowedValue)

instance
  Core.FromXML
    MinimumEngineVersionPerAllowedValue
  where
  parseXML x =
    MinimumEngineVersionPerAllowedValue'
      Core.<$> (x Core..@? "AllowedValue")
      Core.<*> (x Core..@? "MinimumEngineVersion")

instance
  Core.Hashable
    MinimumEngineVersionPerAllowedValue

instance
  Core.NFData
    MinimumEngineVersionPerAllowedValue
