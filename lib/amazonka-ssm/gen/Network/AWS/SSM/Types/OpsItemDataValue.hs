{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.OpsItemDataValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.OpsItemDataValue
  ( OpsItemDataValue (..),

    -- * Smart constructor
    mkOpsItemDataValue,

    -- * Lenses
    oidvValue,
    oidvType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SSM.Types.OpsItemDataType

-- | An object that defines the value of the key and its type in the OperationalData map.
--
-- /See:/ 'mkOpsItemDataValue' smart constructor.
data OpsItemDataValue = OpsItemDataValue'
  { -- | The value of the OperationalData key.
    value :: Lude.Maybe Lude.Text,
    -- | The type of key-value pair. Valid types include @SearchableString@ and @String@ .
    type' :: Lude.Maybe OpsItemDataType
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'OpsItemDataValue' with the minimum fields required to make a request.
--
-- * 'value' - The value of the OperationalData key.
-- * 'type'' - The type of key-value pair. Valid types include @SearchableString@ and @String@ .
mkOpsItemDataValue ::
  OpsItemDataValue
mkOpsItemDataValue =
  OpsItemDataValue' {value = Lude.Nothing, type' = Lude.Nothing}

-- | The value of the OperationalData key.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oidvValue :: Lens.Lens' OpsItemDataValue (Lude.Maybe Lude.Text)
oidvValue = Lens.lens (value :: OpsItemDataValue -> Lude.Maybe Lude.Text) (\s a -> s {value = a} :: OpsItemDataValue)
{-# DEPRECATED oidvValue "Use generic-lens or generic-optics with 'value' instead." #-}

-- | The type of key-value pair. Valid types include @SearchableString@ and @String@ .
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oidvType :: Lens.Lens' OpsItemDataValue (Lude.Maybe OpsItemDataType)
oidvType = Lens.lens (type' :: OpsItemDataValue -> Lude.Maybe OpsItemDataType) (\s a -> s {type' = a} :: OpsItemDataValue)
{-# DEPRECATED oidvType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Lude.FromJSON OpsItemDataValue where
  parseJSON =
    Lude.withObject
      "OpsItemDataValue"
      ( \x ->
          OpsItemDataValue'
            Lude.<$> (x Lude..:? "Value") Lude.<*> (x Lude..:? "Type")
      )

instance Lude.ToJSON OpsItemDataValue where
  toJSON OpsItemDataValue' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Value" Lude..=) Lude.<$> value,
            ("Type" Lude..=) Lude.<$> type'
          ]
      )
