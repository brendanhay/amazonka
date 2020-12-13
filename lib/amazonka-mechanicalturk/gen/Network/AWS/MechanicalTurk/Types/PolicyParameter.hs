{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.Types.PolicyParameter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MechanicalTurk.Types.PolicyParameter
  ( PolicyParameter (..),

    -- * Smart constructor
    mkPolicyParameter,

    -- * Lenses
    ppValues,
    ppMapEntries,
    ppKey,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MechanicalTurk.Types.ParameterMapEntry
import qualified Network.AWS.Prelude as Lude

-- | Name of the parameter from the Review policy.
--
-- /See:/ 'mkPolicyParameter' smart constructor.
data PolicyParameter = PolicyParameter'
  { -- | The list of values of the Parameter
    values :: Lude.Maybe [Lude.Text],
    -- | List of ParameterMapEntry objects.
    mapEntries :: Lude.Maybe [ParameterMapEntry],
    -- | Name of the parameter from the list of Review Polices.
    key :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PolicyParameter' with the minimum fields required to make a request.
--
-- * 'values' - The list of values of the Parameter
-- * 'mapEntries' - List of ParameterMapEntry objects.
-- * 'key' - Name of the parameter from the list of Review Polices.
mkPolicyParameter ::
  PolicyParameter
mkPolicyParameter =
  PolicyParameter'
    { values = Lude.Nothing,
      mapEntries = Lude.Nothing,
      key = Lude.Nothing
    }

-- | The list of values of the Parameter
--
-- /Note:/ Consider using 'values' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppValues :: Lens.Lens' PolicyParameter (Lude.Maybe [Lude.Text])
ppValues = Lens.lens (values :: PolicyParameter -> Lude.Maybe [Lude.Text]) (\s a -> s {values = a} :: PolicyParameter)
{-# DEPRECATED ppValues "Use generic-lens or generic-optics with 'values' instead." #-}

-- | List of ParameterMapEntry objects.
--
-- /Note:/ Consider using 'mapEntries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppMapEntries :: Lens.Lens' PolicyParameter (Lude.Maybe [ParameterMapEntry])
ppMapEntries = Lens.lens (mapEntries :: PolicyParameter -> Lude.Maybe [ParameterMapEntry]) (\s a -> s {mapEntries = a} :: PolicyParameter)
{-# DEPRECATED ppMapEntries "Use generic-lens or generic-optics with 'mapEntries' instead." #-}

-- | Name of the parameter from the list of Review Polices.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppKey :: Lens.Lens' PolicyParameter (Lude.Maybe Lude.Text)
ppKey = Lens.lens (key :: PolicyParameter -> Lude.Maybe Lude.Text) (\s a -> s {key = a} :: PolicyParameter)
{-# DEPRECATED ppKey "Use generic-lens or generic-optics with 'key' instead." #-}

instance Lude.FromJSON PolicyParameter where
  parseJSON =
    Lude.withObject
      "PolicyParameter"
      ( \x ->
          PolicyParameter'
            Lude.<$> (x Lude..:? "Values" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "MapEntries" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Key")
      )

instance Lude.ToJSON PolicyParameter where
  toJSON PolicyParameter' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Values" Lude..=) Lude.<$> values,
            ("MapEntries" Lude..=) Lude.<$> mapEntries,
            ("Key" Lude..=) Lude.<$> key
          ]
      )
