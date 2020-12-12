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
    tpParameterKey,
    tpDefaultValue,
    tpNoEcho,
    tpDescription,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The TemplateParameter data type.
--
-- /See:/ 'mkTemplateParameter' smart constructor.
data TemplateParameter = TemplateParameter'
  { parameterKey ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'TemplateParameter' with the minimum fields required to make a request.
--
-- * 'defaultValue' - The default value associated with the parameter.
-- * 'description' - User defined description associated with the parameter.
-- * 'noEcho' - Flag indicating whether the parameter should be displayed as plain text in logs and UIs.
-- * 'parameterKey' - The name associated with the parameter.
mkTemplateParameter ::
  TemplateParameter
mkTemplateParameter =
  TemplateParameter'
    { parameterKey = Lude.Nothing,
      defaultValue = Lude.Nothing,
      noEcho = Lude.Nothing,
      description = Lude.Nothing
    }

-- | The name associated with the parameter.
--
-- /Note:/ Consider using 'parameterKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tpParameterKey :: Lens.Lens' TemplateParameter (Lude.Maybe Lude.Text)
tpParameterKey = Lens.lens (parameterKey :: TemplateParameter -> Lude.Maybe Lude.Text) (\s a -> s {parameterKey = a} :: TemplateParameter)
{-# DEPRECATED tpParameterKey "Use generic-lens or generic-optics with 'parameterKey' instead." #-}

-- | The default value associated with the parameter.
--
-- /Note:/ Consider using 'defaultValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tpDefaultValue :: Lens.Lens' TemplateParameter (Lude.Maybe Lude.Text)
tpDefaultValue = Lens.lens (defaultValue :: TemplateParameter -> Lude.Maybe Lude.Text) (\s a -> s {defaultValue = a} :: TemplateParameter)
{-# DEPRECATED tpDefaultValue "Use generic-lens or generic-optics with 'defaultValue' instead." #-}

-- | Flag indicating whether the parameter should be displayed as plain text in logs and UIs.
--
-- /Note:/ Consider using 'noEcho' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tpNoEcho :: Lens.Lens' TemplateParameter (Lude.Maybe Lude.Bool)
tpNoEcho = Lens.lens (noEcho :: TemplateParameter -> Lude.Maybe Lude.Bool) (\s a -> s {noEcho = a} :: TemplateParameter)
{-# DEPRECATED tpNoEcho "Use generic-lens or generic-optics with 'noEcho' instead." #-}

-- | User defined description associated with the parameter.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tpDescription :: Lens.Lens' TemplateParameter (Lude.Maybe Lude.Text)
tpDescription = Lens.lens (description :: TemplateParameter -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: TemplateParameter)
{-# DEPRECATED tpDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.FromXML TemplateParameter where
  parseXML x =
    TemplateParameter'
      Lude.<$> (x Lude..@? "ParameterKey")
      Lude.<*> (x Lude..@? "DefaultValue")
      Lude.<*> (x Lude..@? "NoEcho")
      Lude.<*> (x Lude..@? "Description")
