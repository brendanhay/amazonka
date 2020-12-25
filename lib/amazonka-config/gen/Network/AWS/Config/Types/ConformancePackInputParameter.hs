{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.ConformancePackInputParameter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.ConformancePackInputParameter
  ( ConformancePackInputParameter (..),

    -- * Smart constructor
    mkConformancePackInputParameter,

    -- * Lenses
    cpipParameterName,
    cpipParameterValue,
  )
where

import qualified Network.AWS.Config.Types.ParameterName as Types
import qualified Network.AWS.Config.Types.ParameterValue as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Input parameters in the form of key-value pairs for the conformance pack, both of which you define. Keys can have a maximum character length of 255 characters, and values can have a maximum length of 4096 characters.
--
-- /See:/ 'mkConformancePackInputParameter' smart constructor.
data ConformancePackInputParameter = ConformancePackInputParameter'
  { -- | One part of a key-value pair.
    parameterName :: Types.ParameterName,
    -- | Another part of the key-value pair.
    parameterValue :: Types.ParameterValue
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ConformancePackInputParameter' value with any optional fields omitted.
mkConformancePackInputParameter ::
  -- | 'parameterName'
  Types.ParameterName ->
  -- | 'parameterValue'
  Types.ParameterValue ->
  ConformancePackInputParameter
mkConformancePackInputParameter parameterName parameterValue =
  ConformancePackInputParameter' {parameterName, parameterValue}

-- | One part of a key-value pair.
--
-- /Note:/ Consider using 'parameterName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpipParameterName :: Lens.Lens' ConformancePackInputParameter Types.ParameterName
cpipParameterName = Lens.field @"parameterName"
{-# DEPRECATED cpipParameterName "Use generic-lens or generic-optics with 'parameterName' instead." #-}

-- | Another part of the key-value pair.
--
-- /Note:/ Consider using 'parameterValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpipParameterValue :: Lens.Lens' ConformancePackInputParameter Types.ParameterValue
cpipParameterValue = Lens.field @"parameterValue"
{-# DEPRECATED cpipParameterValue "Use generic-lens or generic-optics with 'parameterValue' instead." #-}

instance Core.FromJSON ConformancePackInputParameter where
  toJSON ConformancePackInputParameter {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ParameterName" Core..= parameterName),
            Core.Just ("ParameterValue" Core..= parameterValue)
          ]
      )

instance Core.FromJSON ConformancePackInputParameter where
  parseJSON =
    Core.withObject "ConformancePackInputParameter" Core.$
      \x ->
        ConformancePackInputParameter'
          Core.<$> (x Core..: "ParameterName") Core.<*> (x Core..: "ParameterValue")
