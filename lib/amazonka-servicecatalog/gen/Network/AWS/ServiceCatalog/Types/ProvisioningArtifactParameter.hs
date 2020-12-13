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
    papIsNoEcho,
    papParameterKey,
    papParameterType,
    papParameterConstraints,
    papDefaultValue,
    papDescription,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.ServiceCatalog.Types.ParameterConstraints

-- | Information about a parameter used to provision a product.
--
-- /See:/ 'mkProvisioningArtifactParameter' smart constructor.
data ProvisioningArtifactParameter = ProvisioningArtifactParameter'
  { -- | If this value is true, the value for this parameter is obfuscated from view when the parameter is retrieved. This parameter is used to hide sensitive information.
    isNoEcho :: Lude.Maybe Lude.Bool,
    -- | The parameter key.
    parameterKey :: Lude.Maybe Lude.Text,
    -- | The parameter type.
    parameterType :: Lude.Maybe Lude.Text,
    -- | Constraints that the administrator has put on a parameter.
    parameterConstraints :: Lude.Maybe ParameterConstraints,
    -- | The default value.
    defaultValue :: Lude.Maybe Lude.Text,
    -- | The description of the parameter.
    description :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ProvisioningArtifactParameter' with the minimum fields required to make a request.
--
-- * 'isNoEcho' - If this value is true, the value for this parameter is obfuscated from view when the parameter is retrieved. This parameter is used to hide sensitive information.
-- * 'parameterKey' - The parameter key.
-- * 'parameterType' - The parameter type.
-- * 'parameterConstraints' - Constraints that the administrator has put on a parameter.
-- * 'defaultValue' - The default value.
-- * 'description' - The description of the parameter.
mkProvisioningArtifactParameter ::
  ProvisioningArtifactParameter
mkProvisioningArtifactParameter =
  ProvisioningArtifactParameter'
    { isNoEcho = Lude.Nothing,
      parameterKey = Lude.Nothing,
      parameterType = Lude.Nothing,
      parameterConstraints = Lude.Nothing,
      defaultValue = Lude.Nothing,
      description = Lude.Nothing
    }

-- | If this value is true, the value for this parameter is obfuscated from view when the parameter is retrieved. This parameter is used to hide sensitive information.
--
-- /Note:/ Consider using 'isNoEcho' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
papIsNoEcho :: Lens.Lens' ProvisioningArtifactParameter (Lude.Maybe Lude.Bool)
papIsNoEcho = Lens.lens (isNoEcho :: ProvisioningArtifactParameter -> Lude.Maybe Lude.Bool) (\s a -> s {isNoEcho = a} :: ProvisioningArtifactParameter)
{-# DEPRECATED papIsNoEcho "Use generic-lens or generic-optics with 'isNoEcho' instead." #-}

-- | The parameter key.
--
-- /Note:/ Consider using 'parameterKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
papParameterKey :: Lens.Lens' ProvisioningArtifactParameter (Lude.Maybe Lude.Text)
papParameterKey = Lens.lens (parameterKey :: ProvisioningArtifactParameter -> Lude.Maybe Lude.Text) (\s a -> s {parameterKey = a} :: ProvisioningArtifactParameter)
{-# DEPRECATED papParameterKey "Use generic-lens or generic-optics with 'parameterKey' instead." #-}

-- | The parameter type.
--
-- /Note:/ Consider using 'parameterType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
papParameterType :: Lens.Lens' ProvisioningArtifactParameter (Lude.Maybe Lude.Text)
papParameterType = Lens.lens (parameterType :: ProvisioningArtifactParameter -> Lude.Maybe Lude.Text) (\s a -> s {parameterType = a} :: ProvisioningArtifactParameter)
{-# DEPRECATED papParameterType "Use generic-lens or generic-optics with 'parameterType' instead." #-}

-- | Constraints that the administrator has put on a parameter.
--
-- /Note:/ Consider using 'parameterConstraints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
papParameterConstraints :: Lens.Lens' ProvisioningArtifactParameter (Lude.Maybe ParameterConstraints)
papParameterConstraints = Lens.lens (parameterConstraints :: ProvisioningArtifactParameter -> Lude.Maybe ParameterConstraints) (\s a -> s {parameterConstraints = a} :: ProvisioningArtifactParameter)
{-# DEPRECATED papParameterConstraints "Use generic-lens or generic-optics with 'parameterConstraints' instead." #-}

-- | The default value.
--
-- /Note:/ Consider using 'defaultValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
papDefaultValue :: Lens.Lens' ProvisioningArtifactParameter (Lude.Maybe Lude.Text)
papDefaultValue = Lens.lens (defaultValue :: ProvisioningArtifactParameter -> Lude.Maybe Lude.Text) (\s a -> s {defaultValue = a} :: ProvisioningArtifactParameter)
{-# DEPRECATED papDefaultValue "Use generic-lens or generic-optics with 'defaultValue' instead." #-}

-- | The description of the parameter.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
papDescription :: Lens.Lens' ProvisioningArtifactParameter (Lude.Maybe Lude.Text)
papDescription = Lens.lens (description :: ProvisioningArtifactParameter -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: ProvisioningArtifactParameter)
{-# DEPRECATED papDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.FromJSON ProvisioningArtifactParameter where
  parseJSON =
    Lude.withObject
      "ProvisioningArtifactParameter"
      ( \x ->
          ProvisioningArtifactParameter'
            Lude.<$> (x Lude..:? "IsNoEcho")
            Lude.<*> (x Lude..:? "ParameterKey")
            Lude.<*> (x Lude..:? "ParameterType")
            Lude.<*> (x Lude..:? "ParameterConstraints")
            Lude.<*> (x Lude..:? "DefaultValue")
            Lude.<*> (x Lude..:? "Description")
      )
