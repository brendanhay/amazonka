{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.ProvisioningArtifactParameter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.ProvisioningArtifactParameter
  ( ProvisioningArtifactParameter (..),

    -- * Smart constructor
    mkProvisioningArtifactParameter,

    -- * Lenses
    papDefaultValue,
    papDescription,
    papIsNoEcho,
    papParameterConstraints,
    papParameterKey,
    papParameterType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.ServiceCatalog.Types.DefaultValue as Types
import qualified Network.AWS.ServiceCatalog.Types.Description as Types
import qualified Network.AWS.ServiceCatalog.Types.ParameterConstraints as Types
import qualified Network.AWS.ServiceCatalog.Types.ParameterKey as Types
import qualified Network.AWS.ServiceCatalog.Types.ParameterType as Types

-- | Information about a parameter used to provision a product.
--
-- /See:/ 'mkProvisioningArtifactParameter' smart constructor.
data ProvisioningArtifactParameter = ProvisioningArtifactParameter'
  { -- | The default value.
    defaultValue :: Core.Maybe Types.DefaultValue,
    -- | The description of the parameter.
    description :: Core.Maybe Types.Description,
    -- | If this value is true, the value for this parameter is obfuscated from view when the parameter is retrieved. This parameter is used to hide sensitive information.
    isNoEcho :: Core.Maybe Core.Bool,
    -- | Constraints that the administrator has put on a parameter.
    parameterConstraints :: Core.Maybe Types.ParameterConstraints,
    -- | The parameter key.
    parameterKey :: Core.Maybe Types.ParameterKey,
    -- | The parameter type.
    parameterType :: Core.Maybe Types.ParameterType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ProvisioningArtifactParameter' value with any optional fields omitted.
mkProvisioningArtifactParameter ::
  ProvisioningArtifactParameter
mkProvisioningArtifactParameter =
  ProvisioningArtifactParameter'
    { defaultValue = Core.Nothing,
      description = Core.Nothing,
      isNoEcho = Core.Nothing,
      parameterConstraints = Core.Nothing,
      parameterKey = Core.Nothing,
      parameterType = Core.Nothing
    }

-- | The default value.
--
-- /Note:/ Consider using 'defaultValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
papDefaultValue :: Lens.Lens' ProvisioningArtifactParameter (Core.Maybe Types.DefaultValue)
papDefaultValue = Lens.field @"defaultValue"
{-# DEPRECATED papDefaultValue "Use generic-lens or generic-optics with 'defaultValue' instead." #-}

-- | The description of the parameter.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
papDescription :: Lens.Lens' ProvisioningArtifactParameter (Core.Maybe Types.Description)
papDescription = Lens.field @"description"
{-# DEPRECATED papDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | If this value is true, the value for this parameter is obfuscated from view when the parameter is retrieved. This parameter is used to hide sensitive information.
--
-- /Note:/ Consider using 'isNoEcho' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
papIsNoEcho :: Lens.Lens' ProvisioningArtifactParameter (Core.Maybe Core.Bool)
papIsNoEcho = Lens.field @"isNoEcho"
{-# DEPRECATED papIsNoEcho "Use generic-lens or generic-optics with 'isNoEcho' instead." #-}

-- | Constraints that the administrator has put on a parameter.
--
-- /Note:/ Consider using 'parameterConstraints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
papParameterConstraints :: Lens.Lens' ProvisioningArtifactParameter (Core.Maybe Types.ParameterConstraints)
papParameterConstraints = Lens.field @"parameterConstraints"
{-# DEPRECATED papParameterConstraints "Use generic-lens or generic-optics with 'parameterConstraints' instead." #-}

-- | The parameter key.
--
-- /Note:/ Consider using 'parameterKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
papParameterKey :: Lens.Lens' ProvisioningArtifactParameter (Core.Maybe Types.ParameterKey)
papParameterKey = Lens.field @"parameterKey"
{-# DEPRECATED papParameterKey "Use generic-lens or generic-optics with 'parameterKey' instead." #-}

-- | The parameter type.
--
-- /Note:/ Consider using 'parameterType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
papParameterType :: Lens.Lens' ProvisioningArtifactParameter (Core.Maybe Types.ParameterType)
papParameterType = Lens.field @"parameterType"
{-# DEPRECATED papParameterType "Use generic-lens or generic-optics with 'parameterType' instead." #-}

instance Core.FromJSON ProvisioningArtifactParameter where
  parseJSON =
    Core.withObject "ProvisioningArtifactParameter" Core.$
      \x ->
        ProvisioningArtifactParameter'
          Core.<$> (x Core..:? "DefaultValue")
          Core.<*> (x Core..:? "Description")
          Core.<*> (x Core..:? "IsNoEcho")
          Core.<*> (x Core..:? "ParameterConstraints")
          Core.<*> (x Core..:? "ParameterKey")
          Core.<*> (x Core..:? "ParameterType")
