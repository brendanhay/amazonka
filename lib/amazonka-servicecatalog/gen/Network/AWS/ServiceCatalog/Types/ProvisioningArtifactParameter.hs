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
    pIsNoEcho,
    pParameterKey,
    pParameterType,
    pParameterConstraints,
    pDefaultValue,
    pDescription,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.ServiceCatalog.Types.ParameterConstraints

-- | Information about a parameter used to provision a product.
--
-- /See:/ 'mkProvisioningArtifactParameter' smart constructor.
data ProvisioningArtifactParameter = ProvisioningArtifactParameter'
  { isNoEcho ::
      Lude.Maybe Lude.Bool,
    parameterKey ::
      Lude.Maybe Lude.Text,
    parameterType ::
      Lude.Maybe Lude.Text,
    parameterConstraints ::
      Lude.Maybe ParameterConstraints,
    defaultValue ::
      Lude.Maybe Lude.Text,
    description ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ProvisioningArtifactParameter' with the minimum fields required to make a request.
--
-- * 'defaultValue' - The default value.
-- * 'description' - The description of the parameter.
-- * 'isNoEcho' - If this value is true, the value for this parameter is obfuscated from view when the parameter is retrieved. This parameter is used to hide sensitive information.
-- * 'parameterConstraints' - Constraints that the administrator has put on a parameter.
-- * 'parameterKey' - The parameter key.
-- * 'parameterType' - The parameter type.
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
pIsNoEcho :: Lens.Lens' ProvisioningArtifactParameter (Lude.Maybe Lude.Bool)
pIsNoEcho = Lens.lens (isNoEcho :: ProvisioningArtifactParameter -> Lude.Maybe Lude.Bool) (\s a -> s {isNoEcho = a} :: ProvisioningArtifactParameter)
{-# DEPRECATED pIsNoEcho "Use generic-lens or generic-optics with 'isNoEcho' instead." #-}

-- | The parameter key.
--
-- /Note:/ Consider using 'parameterKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pParameterKey :: Lens.Lens' ProvisioningArtifactParameter (Lude.Maybe Lude.Text)
pParameterKey = Lens.lens (parameterKey :: ProvisioningArtifactParameter -> Lude.Maybe Lude.Text) (\s a -> s {parameterKey = a} :: ProvisioningArtifactParameter)
{-# DEPRECATED pParameterKey "Use generic-lens or generic-optics with 'parameterKey' instead." #-}

-- | The parameter type.
--
-- /Note:/ Consider using 'parameterType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pParameterType :: Lens.Lens' ProvisioningArtifactParameter (Lude.Maybe Lude.Text)
pParameterType = Lens.lens (parameterType :: ProvisioningArtifactParameter -> Lude.Maybe Lude.Text) (\s a -> s {parameterType = a} :: ProvisioningArtifactParameter)
{-# DEPRECATED pParameterType "Use generic-lens or generic-optics with 'parameterType' instead." #-}

-- | Constraints that the administrator has put on a parameter.
--
-- /Note:/ Consider using 'parameterConstraints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pParameterConstraints :: Lens.Lens' ProvisioningArtifactParameter (Lude.Maybe ParameterConstraints)
pParameterConstraints = Lens.lens (parameterConstraints :: ProvisioningArtifactParameter -> Lude.Maybe ParameterConstraints) (\s a -> s {parameterConstraints = a} :: ProvisioningArtifactParameter)
{-# DEPRECATED pParameterConstraints "Use generic-lens or generic-optics with 'parameterConstraints' instead." #-}

-- | The default value.
--
-- /Note:/ Consider using 'defaultValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pDefaultValue :: Lens.Lens' ProvisioningArtifactParameter (Lude.Maybe Lude.Text)
pDefaultValue = Lens.lens (defaultValue :: ProvisioningArtifactParameter -> Lude.Maybe Lude.Text) (\s a -> s {defaultValue = a} :: ProvisioningArtifactParameter)
{-# DEPRECATED pDefaultValue "Use generic-lens or generic-optics with 'defaultValue' instead." #-}

-- | The description of the parameter.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pDescription :: Lens.Lens' ProvisioningArtifactParameter (Lude.Maybe Lude.Text)
pDescription = Lens.lens (description :: ProvisioningArtifactParameter -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: ProvisioningArtifactParameter)
{-# DEPRECATED pDescription "Use generic-lens or generic-optics with 'description' instead." #-}

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
