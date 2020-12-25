{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.UpdateProvisioningParameter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.UpdateProvisioningParameter
  ( UpdateProvisioningParameter (..),

    -- * Smart constructor
    mkUpdateProvisioningParameter,

    -- * Lenses
    uppKey,
    uppUsePreviousValue,
    uppValue,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.ServiceCatalog.Types.ParameterKey as Types
import qualified Network.AWS.ServiceCatalog.Types.ParameterValue as Types

-- | The parameter key-value pair used to update a provisioned product.
--
-- /See:/ 'mkUpdateProvisioningParameter' smart constructor.
data UpdateProvisioningParameter = UpdateProvisioningParameter'
  { -- | The parameter key.
    key :: Core.Maybe Types.ParameterKey,
    -- | If set to true, @Value@ is ignored and the previous parameter value is kept.
    usePreviousValue :: Core.Maybe Core.Bool,
    -- | The parameter value.
    value :: Core.Maybe Types.ParameterValue
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateProvisioningParameter' value with any optional fields omitted.
mkUpdateProvisioningParameter ::
  UpdateProvisioningParameter
mkUpdateProvisioningParameter =
  UpdateProvisioningParameter'
    { key = Core.Nothing,
      usePreviousValue = Core.Nothing,
      value = Core.Nothing
    }

-- | The parameter key.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uppKey :: Lens.Lens' UpdateProvisioningParameter (Core.Maybe Types.ParameterKey)
uppKey = Lens.field @"key"
{-# DEPRECATED uppKey "Use generic-lens or generic-optics with 'key' instead." #-}

-- | If set to true, @Value@ is ignored and the previous parameter value is kept.
--
-- /Note:/ Consider using 'usePreviousValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uppUsePreviousValue :: Lens.Lens' UpdateProvisioningParameter (Core.Maybe Core.Bool)
uppUsePreviousValue = Lens.field @"usePreviousValue"
{-# DEPRECATED uppUsePreviousValue "Use generic-lens or generic-optics with 'usePreviousValue' instead." #-}

-- | The parameter value.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uppValue :: Lens.Lens' UpdateProvisioningParameter (Core.Maybe Types.ParameterValue)
uppValue = Lens.field @"value"
{-# DEPRECATED uppValue "Use generic-lens or generic-optics with 'value' instead." #-}

instance Core.FromJSON UpdateProvisioningParameter where
  toJSON UpdateProvisioningParameter {..} =
    Core.object
      ( Core.catMaybes
          [ ("Key" Core..=) Core.<$> key,
            ("UsePreviousValue" Core..=) Core.<$> usePreviousValue,
            ("Value" Core..=) Core.<$> value
          ]
      )

instance Core.FromJSON UpdateProvisioningParameter where
  parseJSON =
    Core.withObject "UpdateProvisioningParameter" Core.$
      \x ->
        UpdateProvisioningParameter'
          Core.<$> (x Core..:? "Key")
          Core.<*> (x Core..:? "UsePreviousValue")
          Core.<*> (x Core..:? "Value")
