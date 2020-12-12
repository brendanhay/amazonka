{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.Parameter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.Parameter
  ( Parameter (..),

    -- * Smart constructor
    mkParameter,

    -- * Lenses
    pParameterValue,
    pResolvedValue,
    pParameterKey,
    pUsePreviousValue,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The Parameter data type.
--
-- /See:/ 'mkParameter' smart constructor.
data Parameter = Parameter'
  { parameterValue :: Lude.Maybe Lude.Text,
    resolvedValue :: Lude.Maybe Lude.Text,
    parameterKey :: Lude.Maybe Lude.Text,
    usePreviousValue :: Lude.Maybe Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Parameter' with the minimum fields required to make a request.
--
-- * 'parameterKey' - The key associated with the parameter. If you don't specify a key and value for a particular parameter, AWS CloudFormation uses the default value that is specified in your template.
-- * 'parameterValue' - The input value associated with the parameter.
-- * 'resolvedValue' - Read-only. The value that corresponds to a Systems Manager parameter key. This field is returned only for <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/parameters-section-structure.html#aws-ssm-parameter-types @SSM@ parameter types> in the template.
-- * 'usePreviousValue' - During a stack update, use the existing parameter value that the stack is using for a given parameter key. If you specify @true@ , do not specify a parameter value.
mkParameter ::
  Parameter
mkParameter =
  Parameter'
    { parameterValue = Lude.Nothing,
      resolvedValue = Lude.Nothing,
      parameterKey = Lude.Nothing,
      usePreviousValue = Lude.Nothing
    }

-- | The input value associated with the parameter.
--
-- /Note:/ Consider using 'parameterValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pParameterValue :: Lens.Lens' Parameter (Lude.Maybe Lude.Text)
pParameterValue = Lens.lens (parameterValue :: Parameter -> Lude.Maybe Lude.Text) (\s a -> s {parameterValue = a} :: Parameter)
{-# DEPRECATED pParameterValue "Use generic-lens or generic-optics with 'parameterValue' instead." #-}

-- | Read-only. The value that corresponds to a Systems Manager parameter key. This field is returned only for <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/parameters-section-structure.html#aws-ssm-parameter-types @SSM@ parameter types> in the template.
--
-- /Note:/ Consider using 'resolvedValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pResolvedValue :: Lens.Lens' Parameter (Lude.Maybe Lude.Text)
pResolvedValue = Lens.lens (resolvedValue :: Parameter -> Lude.Maybe Lude.Text) (\s a -> s {resolvedValue = a} :: Parameter)
{-# DEPRECATED pResolvedValue "Use generic-lens or generic-optics with 'resolvedValue' instead." #-}

-- | The key associated with the parameter. If you don't specify a key and value for a particular parameter, AWS CloudFormation uses the default value that is specified in your template.
--
-- /Note:/ Consider using 'parameterKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pParameterKey :: Lens.Lens' Parameter (Lude.Maybe Lude.Text)
pParameterKey = Lens.lens (parameterKey :: Parameter -> Lude.Maybe Lude.Text) (\s a -> s {parameterKey = a} :: Parameter)
{-# DEPRECATED pParameterKey "Use generic-lens or generic-optics with 'parameterKey' instead." #-}

-- | During a stack update, use the existing parameter value that the stack is using for a given parameter key. If you specify @true@ , do not specify a parameter value.
--
-- /Note:/ Consider using 'usePreviousValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pUsePreviousValue :: Lens.Lens' Parameter (Lude.Maybe Lude.Bool)
pUsePreviousValue = Lens.lens (usePreviousValue :: Parameter -> Lude.Maybe Lude.Bool) (\s a -> s {usePreviousValue = a} :: Parameter)
{-# DEPRECATED pUsePreviousValue "Use generic-lens or generic-optics with 'usePreviousValue' instead." #-}

instance Lude.FromXML Parameter where
  parseXML x =
    Parameter'
      Lude.<$> (x Lude..@? "ParameterValue")
      Lude.<*> (x Lude..@? "ResolvedValue")
      Lude.<*> (x Lude..@? "ParameterKey")
      Lude.<*> (x Lude..@? "UsePreviousValue")

instance Lude.ToQuery Parameter where
  toQuery Parameter' {..} =
    Lude.mconcat
      [ "ParameterValue" Lude.=: parameterValue,
        "ResolvedValue" Lude.=: resolvedValue,
        "ParameterKey" Lude.=: parameterKey,
        "UsePreviousValue" Lude.=: usePreviousValue
      ]
