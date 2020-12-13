{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.MinimumEngineVersionPerAllowedValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.MinimumEngineVersionPerAllowedValue
  ( MinimumEngineVersionPerAllowedValue (..),

    -- * Smart constructor
    mkMinimumEngineVersionPerAllowedValue,

    -- * Lenses
    mevpavMinimumEngineVersion,
    mevpavAllowedValue,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The minimum DB engine version required for each corresponding allowed value for an option setting.
--
-- /See:/ 'mkMinimumEngineVersionPerAllowedValue' smart constructor.
data MinimumEngineVersionPerAllowedValue = MinimumEngineVersionPerAllowedValue'
  { -- | The minimum DB engine version required for the allowed value.
    minimumEngineVersion :: Lude.Maybe Lude.Text,
    -- | The allowed value for an option setting.
    allowedValue :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MinimumEngineVersionPerAllowedValue' with the minimum fields required to make a request.
--
-- * 'minimumEngineVersion' - The minimum DB engine version required for the allowed value.
-- * 'allowedValue' - The allowed value for an option setting.
mkMinimumEngineVersionPerAllowedValue ::
  MinimumEngineVersionPerAllowedValue
mkMinimumEngineVersionPerAllowedValue =
  MinimumEngineVersionPerAllowedValue'
    { minimumEngineVersion =
        Lude.Nothing,
      allowedValue = Lude.Nothing
    }

-- | The minimum DB engine version required for the allowed value.
--
-- /Note:/ Consider using 'minimumEngineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mevpavMinimumEngineVersion :: Lens.Lens' MinimumEngineVersionPerAllowedValue (Lude.Maybe Lude.Text)
mevpavMinimumEngineVersion = Lens.lens (minimumEngineVersion :: MinimumEngineVersionPerAllowedValue -> Lude.Maybe Lude.Text) (\s a -> s {minimumEngineVersion = a} :: MinimumEngineVersionPerAllowedValue)
{-# DEPRECATED mevpavMinimumEngineVersion "Use generic-lens or generic-optics with 'minimumEngineVersion' instead." #-}

-- | The allowed value for an option setting.
--
-- /Note:/ Consider using 'allowedValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mevpavAllowedValue :: Lens.Lens' MinimumEngineVersionPerAllowedValue (Lude.Maybe Lude.Text)
mevpavAllowedValue = Lens.lens (allowedValue :: MinimumEngineVersionPerAllowedValue -> Lude.Maybe Lude.Text) (\s a -> s {allowedValue = a} :: MinimumEngineVersionPerAllowedValue)
{-# DEPRECATED mevpavAllowedValue "Use generic-lens or generic-optics with 'allowedValue' instead." #-}

instance Lude.FromXML MinimumEngineVersionPerAllowedValue where
  parseXML x =
    MinimumEngineVersionPerAllowedValue'
      Lude.<$> (x Lude..@? "MinimumEngineVersion")
      Lude.<*> (x Lude..@? "AllowedValue")
