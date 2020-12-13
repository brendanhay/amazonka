{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.ListRecordHistorySearchFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.ListRecordHistorySearchFilter
  ( ListRecordHistorySearchFilter (..),

    -- * Smart constructor
    mkListRecordHistorySearchFilter,

    -- * Lenses
    lrhsfValue,
    lrhsfKey,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The search filter to use when listing history records.
--
-- /See:/ 'mkListRecordHistorySearchFilter' smart constructor.
data ListRecordHistorySearchFilter = ListRecordHistorySearchFilter'
  { -- | The filter value.
    value :: Lude.Maybe Lude.Text,
    -- | The filter key.
    --
    --
    --     * @product@ - Filter results based on the specified product identifier.
    --
    --
    --     * @provisionedproduct@ - Filter results based on the provisioned product identifier.
    key :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListRecordHistorySearchFilter' with the minimum fields required to make a request.
--
-- * 'value' - The filter value.
-- * 'key' - The filter key.
--
--
--     * @product@ - Filter results based on the specified product identifier.
--
--
--     * @provisionedproduct@ - Filter results based on the provisioned product identifier.
mkListRecordHistorySearchFilter ::
  ListRecordHistorySearchFilter
mkListRecordHistorySearchFilter =
  ListRecordHistorySearchFilter'
    { value = Lude.Nothing,
      key = Lude.Nothing
    }

-- | The filter value.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrhsfValue :: Lens.Lens' ListRecordHistorySearchFilter (Lude.Maybe Lude.Text)
lrhsfValue = Lens.lens (value :: ListRecordHistorySearchFilter -> Lude.Maybe Lude.Text) (\s a -> s {value = a} :: ListRecordHistorySearchFilter)
{-# DEPRECATED lrhsfValue "Use generic-lens or generic-optics with 'value' instead." #-}

-- | The filter key.
--
--
--     * @product@ - Filter results based on the specified product identifier.
--
--
--     * @provisionedproduct@ - Filter results based on the provisioned product identifier.
--
--
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrhsfKey :: Lens.Lens' ListRecordHistorySearchFilter (Lude.Maybe Lude.Text)
lrhsfKey = Lens.lens (key :: ListRecordHistorySearchFilter -> Lude.Maybe Lude.Text) (\s a -> s {key = a} :: ListRecordHistorySearchFilter)
{-# DEPRECATED lrhsfKey "Use generic-lens or generic-optics with 'key' instead." #-}

instance Lude.ToJSON ListRecordHistorySearchFilter where
  toJSON ListRecordHistorySearchFilter' {..} =
    Lude.object
      ( Lude.catMaybes
          [("Value" Lude..=) Lude.<$> value, ("Key" Lude..=) Lude.<$> key]
      )
