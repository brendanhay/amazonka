{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.TemplateParameter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.TemplateParameter
  ( TemplateParameter (..),

    -- * Smart constructor
    mkTemplateParameter,

    -- * Lenses
    tpDefaultValue,
    tpDescription,
    tpNoEcho,
    tpParameterKey,
  )
where

import qualified Network.AWS.CloudFormation.Types.Description as Types
import qualified Network.AWS.CloudFormation.Types.ParameterKey as Types
import qualified Network.AWS.CloudFormation.Types.ParameterValue as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The TemplateParameter data type.
--
-- /See:/ 'mkTemplateParameter' smart constructor.
data TemplateParameter = TemplateParameter'
  { -- | The default value associated with the parameter.
    defaultValue :: Core.Maybe Types.ParameterValue,
    -- | User defined description associated with the parameter.
    description :: Core.Maybe Types.Description,
    -- | Flag indicating whether the parameter should be displayed as plain text in logs and UIs.
    noEcho :: Core.Maybe Core.Bool,
    -- | The name associated with the parameter.
    parameterKey :: Core.Maybe Types.ParameterKey
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TemplateParameter' value with any optional fields omitted.
mkTemplateParameter ::
  TemplateParameter
mkTemplateParameter =
  TemplateParameter'
    { defaultValue = Core.Nothing,
      description = Core.Nothing,
      noEcho = Core.Nothing,
      parameterKey = Core.Nothing
    }

-- | The default value associated with the parameter.
--
-- /Note:/ Consider using 'defaultValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tpDefaultValue :: Lens.Lens' TemplateParameter (Core.Maybe Types.ParameterValue)
tpDefaultValue = Lens.field @"defaultValue"
{-# DEPRECATED tpDefaultValue "Use generic-lens or generic-optics with 'defaultValue' instead." #-}

-- | User defined description associated with the parameter.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tpDescription :: Lens.Lens' TemplateParameter (Core.Maybe Types.Description)
tpDescription = Lens.field @"description"
{-# DEPRECATED tpDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Flag indicating whether the parameter should be displayed as plain text in logs and UIs.
--
-- /Note:/ Consider using 'noEcho' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tpNoEcho :: Lens.Lens' TemplateParameter (Core.Maybe Core.Bool)
tpNoEcho = Lens.field @"noEcho"
{-# DEPRECATED tpNoEcho "Use generic-lens or generic-optics with 'noEcho' instead." #-}

-- | The name associated with the parameter.
--
-- /Note:/ Consider using 'parameterKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tpParameterKey :: Lens.Lens' TemplateParameter (Core.Maybe Types.ParameterKey)
tpParameterKey = Lens.field @"parameterKey"
{-# DEPRECATED tpParameterKey "Use generic-lens or generic-optics with 'parameterKey' instead." #-}

instance Core.FromXML TemplateParameter where
  parseXML x =
    TemplateParameter'
      Core.<$> (x Core..@? "DefaultValue")
      Core.<*> (x Core..@? "Description")
      Core.<*> (x Core..@? "NoEcho")
      Core.<*> (x Core..@? "ParameterKey")
