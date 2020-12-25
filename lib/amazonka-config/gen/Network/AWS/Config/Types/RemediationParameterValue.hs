{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.RemediationParameterValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.RemediationParameterValue
  ( RemediationParameterValue (..),

    -- * Smart constructor
    mkRemediationParameterValue,

    -- * Lenses
    rpvResourceValue,
    rpvStaticValue,
  )
where

import qualified Network.AWS.Config.Types.ResourceValue as Types
import qualified Network.AWS.Config.Types.StaticValue as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The value is either a dynamic (resource) value or a static value. You must select either a dynamic value or a static value.
--
-- /See:/ 'mkRemediationParameterValue' smart constructor.
data RemediationParameterValue = RemediationParameterValue'
  { -- | The value is dynamic and changes at run-time.
    resourceValue :: Core.Maybe Types.ResourceValue,
    -- | The value is static and does not change at run-time.
    staticValue :: Core.Maybe Types.StaticValue
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RemediationParameterValue' value with any optional fields omitted.
mkRemediationParameterValue ::
  RemediationParameterValue
mkRemediationParameterValue =
  RemediationParameterValue'
    { resourceValue = Core.Nothing,
      staticValue = Core.Nothing
    }

-- | The value is dynamic and changes at run-time.
--
-- /Note:/ Consider using 'resourceValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpvResourceValue :: Lens.Lens' RemediationParameterValue (Core.Maybe Types.ResourceValue)
rpvResourceValue = Lens.field @"resourceValue"
{-# DEPRECATED rpvResourceValue "Use generic-lens or generic-optics with 'resourceValue' instead." #-}

-- | The value is static and does not change at run-time.
--
-- /Note:/ Consider using 'staticValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpvStaticValue :: Lens.Lens' RemediationParameterValue (Core.Maybe Types.StaticValue)
rpvStaticValue = Lens.field @"staticValue"
{-# DEPRECATED rpvStaticValue "Use generic-lens or generic-optics with 'staticValue' instead." #-}

instance Core.FromJSON RemediationParameterValue where
  toJSON RemediationParameterValue {..} =
    Core.object
      ( Core.catMaybes
          [ ("ResourceValue" Core..=) Core.<$> resourceValue,
            ("StaticValue" Core..=) Core.<$> staticValue
          ]
      )

instance Core.FromJSON RemediationParameterValue where
  parseJSON =
    Core.withObject "RemediationParameterValue" Core.$
      \x ->
        RemediationParameterValue'
          Core.<$> (x Core..:? "ResourceValue") Core.<*> (x Core..:? "StaticValue")
