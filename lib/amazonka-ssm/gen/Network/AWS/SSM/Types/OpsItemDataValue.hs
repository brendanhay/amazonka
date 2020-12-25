{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.OpsItemDataValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.OpsItemDataValue
  ( OpsItemDataValue (..),

    -- * Smart constructor
    mkOpsItemDataValue,

    -- * Lenses
    oidvType,
    oidvValue,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SSM.Types.OpsItemDataType as Types
import qualified Network.AWS.SSM.Types.OpsItemDataValueString as Types

-- | An object that defines the value of the key and its type in the OperationalData map.
--
-- /See:/ 'mkOpsItemDataValue' smart constructor.
data OpsItemDataValue = OpsItemDataValue'
  { -- | The type of key-value pair. Valid types include @SearchableString@ and @String@ .
    type' :: Core.Maybe Types.OpsItemDataType,
    -- | The value of the OperationalData key.
    value :: Core.Maybe Types.OpsItemDataValueString
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'OpsItemDataValue' value with any optional fields omitted.
mkOpsItemDataValue ::
  OpsItemDataValue
mkOpsItemDataValue =
  OpsItemDataValue' {type' = Core.Nothing, value = Core.Nothing}

-- | The type of key-value pair. Valid types include @SearchableString@ and @String@ .
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oidvType :: Lens.Lens' OpsItemDataValue (Core.Maybe Types.OpsItemDataType)
oidvType = Lens.field @"type'"
{-# DEPRECATED oidvType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The value of the OperationalData key.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oidvValue :: Lens.Lens' OpsItemDataValue (Core.Maybe Types.OpsItemDataValueString)
oidvValue = Lens.field @"value"
{-# DEPRECATED oidvValue "Use generic-lens or generic-optics with 'value' instead." #-}

instance Core.FromJSON OpsItemDataValue where
  toJSON OpsItemDataValue {..} =
    Core.object
      ( Core.catMaybes
          [ ("Type" Core..=) Core.<$> type',
            ("Value" Core..=) Core.<$> value
          ]
      )

instance Core.FromJSON OpsItemDataValue where
  parseJSON =
    Core.withObject "OpsItemDataValue" Core.$
      \x ->
        OpsItemDataValue'
          Core.<$> (x Core..:? "Type") Core.<*> (x Core..:? "Value")
