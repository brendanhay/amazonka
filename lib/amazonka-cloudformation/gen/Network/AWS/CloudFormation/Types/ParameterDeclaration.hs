-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.ParameterDeclaration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.ParameterDeclaration
  ( ParameterDeclaration (..),

    -- * Smart constructor
    mkParameterDeclaration,

    -- * Lenses
    pdParameterKey,
    pdParameterType,
    pdParameterConstraints,
    pdDefaultValue,
    pdNoEcho,
    pdDescription,
  )
where

import Network.AWS.CloudFormation.Types.ParameterConstraints
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The ParameterDeclaration data type.
--
-- /See:/ 'mkParameterDeclaration' smart constructor.
data ParameterDeclaration = ParameterDeclaration'
  { parameterKey ::
      Lude.Maybe Lude.Text,
    parameterType :: Lude.Maybe Lude.Text,
    parameterConstraints ::
      Lude.Maybe ParameterConstraints,
    defaultValue :: Lude.Maybe Lude.Text,
    noEcho :: Lude.Maybe Lude.Bool,
    description :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ParameterDeclaration' with the minimum fields required to make a request.
--
-- * 'defaultValue' - The default value of the parameter.
-- * 'description' - The description that is associate with the parameter.
-- * 'noEcho' - Flag that indicates whether the parameter value is shown as plain text in logs and in the AWS Management Console.
-- * 'parameterConstraints' - The criteria that AWS CloudFormation uses to validate parameter values.
-- * 'parameterKey' - The name that is associated with the parameter.
-- * 'parameterType' - The type of parameter.
mkParameterDeclaration ::
  ParameterDeclaration
mkParameterDeclaration =
  ParameterDeclaration'
    { parameterKey = Lude.Nothing,
      parameterType = Lude.Nothing,
      parameterConstraints = Lude.Nothing,
      defaultValue = Lude.Nothing,
      noEcho = Lude.Nothing,
      description = Lude.Nothing
    }

-- | The name that is associated with the parameter.
--
-- /Note:/ Consider using 'parameterKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdParameterKey :: Lens.Lens' ParameterDeclaration (Lude.Maybe Lude.Text)
pdParameterKey = Lens.lens (parameterKey :: ParameterDeclaration -> Lude.Maybe Lude.Text) (\s a -> s {parameterKey = a} :: ParameterDeclaration)
{-# DEPRECATED pdParameterKey "Use generic-lens or generic-optics with 'parameterKey' instead." #-}

-- | The type of parameter.
--
-- /Note:/ Consider using 'parameterType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdParameterType :: Lens.Lens' ParameterDeclaration (Lude.Maybe Lude.Text)
pdParameterType = Lens.lens (parameterType :: ParameterDeclaration -> Lude.Maybe Lude.Text) (\s a -> s {parameterType = a} :: ParameterDeclaration)
{-# DEPRECATED pdParameterType "Use generic-lens or generic-optics with 'parameterType' instead." #-}

-- | The criteria that AWS CloudFormation uses to validate parameter values.
--
-- /Note:/ Consider using 'parameterConstraints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdParameterConstraints :: Lens.Lens' ParameterDeclaration (Lude.Maybe ParameterConstraints)
pdParameterConstraints = Lens.lens (parameterConstraints :: ParameterDeclaration -> Lude.Maybe ParameterConstraints) (\s a -> s {parameterConstraints = a} :: ParameterDeclaration)
{-# DEPRECATED pdParameterConstraints "Use generic-lens or generic-optics with 'parameterConstraints' instead." #-}

-- | The default value of the parameter.
--
-- /Note:/ Consider using 'defaultValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdDefaultValue :: Lens.Lens' ParameterDeclaration (Lude.Maybe Lude.Text)
pdDefaultValue = Lens.lens (defaultValue :: ParameterDeclaration -> Lude.Maybe Lude.Text) (\s a -> s {defaultValue = a} :: ParameterDeclaration)
{-# DEPRECATED pdDefaultValue "Use generic-lens or generic-optics with 'defaultValue' instead." #-}

-- | Flag that indicates whether the parameter value is shown as plain text in logs and in the AWS Management Console.
--
-- /Note:/ Consider using 'noEcho' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdNoEcho :: Lens.Lens' ParameterDeclaration (Lude.Maybe Lude.Bool)
pdNoEcho = Lens.lens (noEcho :: ParameterDeclaration -> Lude.Maybe Lude.Bool) (\s a -> s {noEcho = a} :: ParameterDeclaration)
{-# DEPRECATED pdNoEcho "Use generic-lens or generic-optics with 'noEcho' instead." #-}

-- | The description that is associate with the parameter.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdDescription :: Lens.Lens' ParameterDeclaration (Lude.Maybe Lude.Text)
pdDescription = Lens.lens (description :: ParameterDeclaration -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: ParameterDeclaration)
{-# DEPRECATED pdDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.FromXML ParameterDeclaration where
  parseXML x =
    ParameterDeclaration'
      Lude.<$> (x Lude..@? "ParameterKey")
      Lude.<*> (x Lude..@? "ParameterType")
      Lude.<*> (x Lude..@? "ParameterConstraints")
      Lude.<*> (x Lude..@? "DefaultValue")
      Lude.<*> (x Lude..@? "NoEcho")
      Lude.<*> (x Lude..@? "Description")
