{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.ProvisioningParameter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.ProvisioningParameter
  ( ProvisioningParameter (..),

    -- * Smart constructor
    mkProvisioningParameter,

    -- * Lenses
    ppValue,
    ppKey,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about a parameter used to provision a product.
--
-- /See:/ 'mkProvisioningParameter' smart constructor.
data ProvisioningParameter = ProvisioningParameter'
  { value ::
      Lude.Maybe Lude.Text,
    key :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ProvisioningParameter' with the minimum fields required to make a request.
--
-- * 'key' - The parameter key.
-- * 'value' - The parameter value.
mkProvisioningParameter ::
  ProvisioningParameter
mkProvisioningParameter =
  ProvisioningParameter' {value = Lude.Nothing, key = Lude.Nothing}

-- | The parameter value.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppValue :: Lens.Lens' ProvisioningParameter (Lude.Maybe Lude.Text)
ppValue = Lens.lens (value :: ProvisioningParameter -> Lude.Maybe Lude.Text) (\s a -> s {value = a} :: ProvisioningParameter)
{-# DEPRECATED ppValue "Use generic-lens or generic-optics with 'value' instead." #-}

-- | The parameter key.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppKey :: Lens.Lens' ProvisioningParameter (Lude.Maybe Lude.Text)
ppKey = Lens.lens (key :: ProvisioningParameter -> Lude.Maybe Lude.Text) (\s a -> s {key = a} :: ProvisioningParameter)
{-# DEPRECATED ppKey "Use generic-lens or generic-optics with 'key' instead." #-}

instance Lude.ToJSON ProvisioningParameter where
  toJSON ProvisioningParameter' {..} =
    Lude.object
      ( Lude.catMaybes
          [("Value" Lude..=) Lude.<$> value, ("Key" Lude..=) Lude.<$> key]
      )
