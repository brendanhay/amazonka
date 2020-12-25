{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DataPipeline.Types.ParameterValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DataPipeline.Types.ParameterValue
  ( ParameterValue (..),

    -- * Smart constructor
    mkParameterValue,

    -- * Lenses
    pvId,
    pvStringValue,
  )
where

import qualified Network.AWS.DataPipeline.Types.Id as Types
import qualified Network.AWS.DataPipeline.Types.StringValue as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A value or list of parameter values.
--
-- /See:/ 'mkParameterValue' smart constructor.
data ParameterValue = ParameterValue'
  { -- | The ID of the parameter value.
    id :: Types.Id,
    -- | The field value, expressed as a String.
    stringValue :: Types.StringValue
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ParameterValue' value with any optional fields omitted.
mkParameterValue ::
  -- | 'id'
  Types.Id ->
  -- | 'stringValue'
  Types.StringValue ->
  ParameterValue
mkParameterValue id stringValue = ParameterValue' {id, stringValue}

-- | The ID of the parameter value.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pvId :: Lens.Lens' ParameterValue Types.Id
pvId = Lens.field @"id"
{-# DEPRECATED pvId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The field value, expressed as a String.
--
-- /Note:/ Consider using 'stringValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pvStringValue :: Lens.Lens' ParameterValue Types.StringValue
pvStringValue = Lens.field @"stringValue"
{-# DEPRECATED pvStringValue "Use generic-lens or generic-optics with 'stringValue' instead." #-}

instance Core.FromJSON ParameterValue where
  toJSON ParameterValue {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("id" Core..= id),
            Core.Just ("stringValue" Core..= stringValue)
          ]
      )

instance Core.FromJSON ParameterValue where
  parseJSON =
    Core.withObject "ParameterValue" Core.$
      \x ->
        ParameterValue'
          Core.<$> (x Core..: "id") Core.<*> (x Core..: "stringValue")
