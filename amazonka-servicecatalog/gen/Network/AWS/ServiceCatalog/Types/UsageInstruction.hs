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
-- Module      : Network.AWS.ServiceCatalog.Types.UsageInstruction
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.UsageInstruction where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Additional information provided by the administrator.
--
-- /See:/ 'newUsageInstruction' smart constructor.
data UsageInstruction = UsageInstruction'
  { -- | The usage instruction value for this type.
    value :: Core.Maybe Core.Text,
    -- | The usage instruction type for the value.
    type' :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UsageInstruction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'value', 'usageInstruction_value' - The usage instruction value for this type.
--
-- 'type'', 'usageInstruction_type' - The usage instruction type for the value.
newUsageInstruction ::
  UsageInstruction
newUsageInstruction =
  UsageInstruction'
    { value = Core.Nothing,
      type' = Core.Nothing
    }

-- | The usage instruction value for this type.
usageInstruction_value :: Lens.Lens' UsageInstruction (Core.Maybe Core.Text)
usageInstruction_value = Lens.lens (\UsageInstruction' {value} -> value) (\s@UsageInstruction' {} a -> s {value = a} :: UsageInstruction)

-- | The usage instruction type for the value.
usageInstruction_type :: Lens.Lens' UsageInstruction (Core.Maybe Core.Text)
usageInstruction_type = Lens.lens (\UsageInstruction' {type'} -> type') (\s@UsageInstruction' {} a -> s {type' = a} :: UsageInstruction)

instance Core.FromJSON UsageInstruction where
  parseJSON =
    Core.withObject
      "UsageInstruction"
      ( \x ->
          UsageInstruction'
            Core.<$> (x Core..:? "Value") Core.<*> (x Core..:? "Type")
      )

instance Core.Hashable UsageInstruction

instance Core.NFData UsageInstruction
