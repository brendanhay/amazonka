{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.Types.HITLayoutParameter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MechanicalTurk.Types.HITLayoutParameter
  ( HITLayoutParameter (..),

    -- * Smart constructor
    mkHITLayoutParameter,

    -- * Lenses
    hitlpName,
    hitlpValue,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MechanicalTurk.Types.String as Types
import qualified Network.AWS.Prelude as Core

-- | The HITLayoutParameter data structure defines parameter values used with a HITLayout. A HITLayout is a reusable Amazon Mechanical Turk project template used to provide Human Intelligence Task (HIT) question data for CreateHIT.
--
-- /See:/ 'mkHITLayoutParameter' smart constructor.
data HITLayoutParameter = HITLayoutParameter'
  { -- | The name of the parameter in the HITLayout.
    name :: Types.String,
    -- | The value substituted for the parameter referenced in the HITLayout.
    value :: Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'HITLayoutParameter' value with any optional fields omitted.
mkHITLayoutParameter ::
  -- | 'name'
  Types.String ->
  -- | 'value'
  Types.String ->
  HITLayoutParameter
mkHITLayoutParameter name value = HITLayoutParameter' {name, value}

-- | The name of the parameter in the HITLayout.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hitlpName :: Lens.Lens' HITLayoutParameter Types.String
hitlpName = Lens.field @"name"
{-# DEPRECATED hitlpName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The value substituted for the parameter referenced in the HITLayout.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hitlpValue :: Lens.Lens' HITLayoutParameter Types.String
hitlpValue = Lens.field @"value"
{-# DEPRECATED hitlpValue "Use generic-lens or generic-optics with 'value' instead." #-}

instance Core.FromJSON HITLayoutParameter where
  toJSON HITLayoutParameter {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Name" Core..= name),
            Core.Just ("Value" Core..= value)
          ]
      )
