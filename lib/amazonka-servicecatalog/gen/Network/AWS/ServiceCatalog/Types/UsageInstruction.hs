{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.UsageInstruction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.UsageInstruction
  ( UsageInstruction (..),

    -- * Smart constructor
    mkUsageInstruction,

    -- * Lenses
    uiValue,
    uiType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Additional information provided by the administrator.
--
-- /See:/ 'mkUsageInstruction' smart constructor.
data UsageInstruction = UsageInstruction'
  { value ::
      Lude.Maybe Lude.Text,
    type' :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UsageInstruction' with the minimum fields required to make a request.
--
-- * 'type'' - The usage instruction type for the value.
-- * 'value' - The usage instruction value for this type.
mkUsageInstruction ::
  UsageInstruction
mkUsageInstruction =
  UsageInstruction' {value = Lude.Nothing, type' = Lude.Nothing}

-- | The usage instruction value for this type.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uiValue :: Lens.Lens' UsageInstruction (Lude.Maybe Lude.Text)
uiValue = Lens.lens (value :: UsageInstruction -> Lude.Maybe Lude.Text) (\s a -> s {value = a} :: UsageInstruction)
{-# DEPRECATED uiValue "Use generic-lens or generic-optics with 'value' instead." #-}

-- | The usage instruction type for the value.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uiType :: Lens.Lens' UsageInstruction (Lude.Maybe Lude.Text)
uiType = Lens.lens (type' :: UsageInstruction -> Lude.Maybe Lude.Text) (\s a -> s {type' = a} :: UsageInstruction)
{-# DEPRECATED uiType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Lude.FromJSON UsageInstruction where
  parseJSON =
    Lude.withObject
      "UsageInstruction"
      ( \x ->
          UsageInstruction'
            Lude.<$> (x Lude..:? "Value") Lude.<*> (x Lude..:? "Type")
      )
