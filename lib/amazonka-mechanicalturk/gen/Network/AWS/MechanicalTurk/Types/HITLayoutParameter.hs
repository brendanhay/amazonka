{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.Types.HITLayoutParameter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MechanicalTurk.Types.HITLayoutParameter
  ( HITLayoutParameter (..),

    -- * Smart constructor
    mkHITLayoutParameter,

    -- * Lenses
    hitlpValue,
    hitlpName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The HITLayoutParameter data structure defines parameter values used with a HITLayout. A HITLayout is a reusable Amazon Mechanical Turk project template used to provide Human Intelligence Task (HIT) question data for CreateHIT.
--
-- /See:/ 'mkHITLayoutParameter' smart constructor.
data HITLayoutParameter = HITLayoutParameter'
  { -- | The value substituted for the parameter referenced in the HITLayout.
    value :: Lude.Text,
    -- | The name of the parameter in the HITLayout.
    name :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'HITLayoutParameter' with the minimum fields required to make a request.
--
-- * 'value' - The value substituted for the parameter referenced in the HITLayout.
-- * 'name' - The name of the parameter in the HITLayout.
mkHITLayoutParameter ::
  -- | 'value'
  Lude.Text ->
  -- | 'name'
  Lude.Text ->
  HITLayoutParameter
mkHITLayoutParameter pValue_ pName_ =
  HITLayoutParameter' {value = pValue_, name = pName_}

-- | The value substituted for the parameter referenced in the HITLayout.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hitlpValue :: Lens.Lens' HITLayoutParameter Lude.Text
hitlpValue = Lens.lens (value :: HITLayoutParameter -> Lude.Text) (\s a -> s {value = a} :: HITLayoutParameter)
{-# DEPRECATED hitlpValue "Use generic-lens or generic-optics with 'value' instead." #-}

-- | The name of the parameter in the HITLayout.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hitlpName :: Lens.Lens' HITLayoutParameter Lude.Text
hitlpName = Lens.lens (name :: HITLayoutParameter -> Lude.Text) (\s a -> s {name = a} :: HITLayoutParameter)
{-# DEPRECATED hitlpName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.ToJSON HITLayoutParameter where
  toJSON HITLayoutParameter' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Value" Lude..= value),
            Lude.Just ("Name" Lude..= name)
          ]
      )
