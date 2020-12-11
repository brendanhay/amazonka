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
import qualified Network.AWS.Prelude as Lude

-- | Parameter value of the application.
--
-- /See:/ 'mkParameterValue' smart constructor.
data ParameterValue = ParameterValue'
  { value :: Lude.Text,
    name :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ParameterValue' with the minimum fields required to make a request.
--
-- * 'name' - The key associated with the parameter. If you don't specify a key and value for a particular parameter, AWS CloudFormation
--
--  uses the default value that is specified in your template.
-- * 'value' - The input value associated with the parameter.
mkParameterValue ::
  -- | 'value'
  Lude.Text ->
  -- | 'name'
  Lude.Text ->
  ParameterValue
mkParameterValue pValue_ pName_ =
  ParameterValue' {value = pValue_, name = pName_}

-- | The input value associated with the parameter.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pvValue :: Lens.Lens' ParameterValue Lude.Text
pvValue = Lens.lens (value :: ParameterValue -> Lude.Text) (\s a -> s {value = a} :: ParameterValue)
{-# DEPRECATED pvValue "Use generic-lens or generic-optics with 'value' instead." #-}

-- | The key associated with the parameter. If you don't specify a key and value for a particular parameter, AWS CloudFormation
--
--  uses the default value that is specified in your template.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pvName :: Lens.Lens' ParameterValue Lude.Text
pvName = Lens.lens (name :: ParameterValue -> Lude.Text) (\s a -> s {name = a} :: ParameterValue)
{-# DEPRECATED pvName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.ToJSON ParameterValue where
  toJSON ParameterValue' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("value" Lude..= value),
            Lude.Just ("name" Lude..= name)
          ]
      )
