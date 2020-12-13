{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.UpdateProvisioningParameter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.UpdateProvisioningParameter
  ( UpdateProvisioningParameter (..),

    -- * Smart constructor
    mkUpdateProvisioningParameter,

    -- * Lenses
    uppValue,
    uppKey,
    uppUsePreviousValue,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The parameter key-value pair used to update a provisioned product.
--
-- /See:/ 'mkUpdateProvisioningParameter' smart constructor.
data UpdateProvisioningParameter = UpdateProvisioningParameter'
  { -- | The parameter value.
    value :: Lude.Maybe Lude.Text,
    -- | The parameter key.
    key :: Lude.Maybe Lude.Text,
    -- | If set to true, @Value@ is ignored and the previous parameter value is kept.
    usePreviousValue :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateProvisioningParameter' with the minimum fields required to make a request.
--
-- * 'value' - The parameter value.
-- * 'key' - The parameter key.
-- * 'usePreviousValue' - If set to true, @Value@ is ignored and the previous parameter value is kept.
mkUpdateProvisioningParameter ::
  UpdateProvisioningParameter
mkUpdateProvisioningParameter =
  UpdateProvisioningParameter'
    { value = Lude.Nothing,
      key = Lude.Nothing,
      usePreviousValue = Lude.Nothing
    }

-- | The parameter value.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uppValue :: Lens.Lens' UpdateProvisioningParameter (Lude.Maybe Lude.Text)
uppValue = Lens.lens (value :: UpdateProvisioningParameter -> Lude.Maybe Lude.Text) (\s a -> s {value = a} :: UpdateProvisioningParameter)
{-# DEPRECATED uppValue "Use generic-lens or generic-optics with 'value' instead." #-}

-- | The parameter key.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uppKey :: Lens.Lens' UpdateProvisioningParameter (Lude.Maybe Lude.Text)
uppKey = Lens.lens (key :: UpdateProvisioningParameter -> Lude.Maybe Lude.Text) (\s a -> s {key = a} :: UpdateProvisioningParameter)
{-# DEPRECATED uppKey "Use generic-lens or generic-optics with 'key' instead." #-}

-- | If set to true, @Value@ is ignored and the previous parameter value is kept.
--
-- /Note:/ Consider using 'usePreviousValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uppUsePreviousValue :: Lens.Lens' UpdateProvisioningParameter (Lude.Maybe Lude.Bool)
uppUsePreviousValue = Lens.lens (usePreviousValue :: UpdateProvisioningParameter -> Lude.Maybe Lude.Bool) (\s a -> s {usePreviousValue = a} :: UpdateProvisioningParameter)
{-# DEPRECATED uppUsePreviousValue "Use generic-lens or generic-optics with 'usePreviousValue' instead." #-}

instance Lude.FromJSON UpdateProvisioningParameter where
  parseJSON =
    Lude.withObject
      "UpdateProvisioningParameter"
      ( \x ->
          UpdateProvisioningParameter'
            Lude.<$> (x Lude..:? "Value")
            Lude.<*> (x Lude..:? "Key")
            Lude.<*> (x Lude..:? "UsePreviousValue")
      )

instance Lude.ToJSON UpdateProvisioningParameter where
  toJSON UpdateProvisioningParameter' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Value" Lude..=) Lude.<$> value,
            ("Key" Lude..=) Lude.<$> key,
            ("UsePreviousValue" Lude..=) Lude.<$> usePreviousValue
          ]
      )
