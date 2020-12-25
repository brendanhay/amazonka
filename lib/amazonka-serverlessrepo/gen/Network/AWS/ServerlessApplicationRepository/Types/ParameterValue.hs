{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServerlessApplicationRepository.Types.ParameterValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServerlessApplicationRepository.Types.ParameterValue
  ( ParameterValue (..),

    -- * Smart constructor
    mkParameterValue,

    -- * Lenses
    pvValue,
    pvName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Parameter value of the application.
--
-- /See:/ 'mkParameterValue' smart constructor.
data ParameterValue = ParameterValue'
  { -- | The input value associated with the parameter.
    value :: Core.Text,
    -- | The key associated with the parameter. If you don't specify a key and value for a particular parameter, AWS CloudFormation
    --
    --  uses the default value that is specified in your template.
    name :: Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ParameterValue' value with any optional fields omitted.
mkParameterValue ::
  -- | 'value'
  Core.Text ->
  -- | 'name'
  Core.Text ->
  ParameterValue
mkParameterValue value name = ParameterValue' {value, name}

-- | The input value associated with the parameter.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pvValue :: Lens.Lens' ParameterValue Core.Text
pvValue = Lens.field @"value"
{-# DEPRECATED pvValue "Use generic-lens or generic-optics with 'value' instead." #-}

-- | The key associated with the parameter. If you don't specify a key and value for a particular parameter, AWS CloudFormation
--
--  uses the default value that is specified in your template.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pvName :: Lens.Lens' ParameterValue Core.Text
pvName = Lens.field @"name"
{-# DEPRECATED pvName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Core.FromJSON ParameterValue where
  toJSON ParameterValue {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("value" Core..= value),
            Core.Just ("name" Core..= name)
          ]
      )
