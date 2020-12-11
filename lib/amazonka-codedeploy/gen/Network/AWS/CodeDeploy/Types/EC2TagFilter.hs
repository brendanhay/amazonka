-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.EC2TagFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.EC2TagFilter
  ( EC2TagFilter (..),

    -- * Smart constructor
    mkEC2TagFilter,

    -- * Lenses
    etfValue,
    etfKey,
    etfType,
  )
where

import Network.AWS.CodeDeploy.Types.EC2TagFilterType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about an EC2 tag filter.
--
-- /See:/ 'mkEC2TagFilter' smart constructor.
data EC2TagFilter = EC2TagFilter'
  { value :: Lude.Maybe Lude.Text,
    key :: Lude.Maybe Lude.Text,
    type' :: Lude.Maybe EC2TagFilterType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EC2TagFilter' with the minimum fields required to make a request.
--
-- * 'key' - The tag filter key.
-- * 'type'' - The tag filter type:
--
--
--     * @KEY_ONLY@ : Key only.
--
--
--     * @VALUE_ONLY@ : Value only.
--
--
--     * @KEY_AND_VALUE@ : Key and value.
--
--
-- * 'value' - The tag filter value.
mkEC2TagFilter ::
  EC2TagFilter
mkEC2TagFilter =
  EC2TagFilter'
    { value = Lude.Nothing,
      key = Lude.Nothing,
      type' = Lude.Nothing
    }

-- | The tag filter value.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etfValue :: Lens.Lens' EC2TagFilter (Lude.Maybe Lude.Text)
etfValue = Lens.lens (value :: EC2TagFilter -> Lude.Maybe Lude.Text) (\s a -> s {value = a} :: EC2TagFilter)
{-# DEPRECATED etfValue "Use generic-lens or generic-optics with 'value' instead." #-}

-- | The tag filter key.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etfKey :: Lens.Lens' EC2TagFilter (Lude.Maybe Lude.Text)
etfKey = Lens.lens (key :: EC2TagFilter -> Lude.Maybe Lude.Text) (\s a -> s {key = a} :: EC2TagFilter)
{-# DEPRECATED etfKey "Use generic-lens or generic-optics with 'key' instead." #-}

-- | The tag filter type:
--
--
--     * @KEY_ONLY@ : Key only.
--
--
--     * @VALUE_ONLY@ : Value only.
--
--
--     * @KEY_AND_VALUE@ : Key and value.
--
--
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etfType :: Lens.Lens' EC2TagFilter (Lude.Maybe EC2TagFilterType)
etfType = Lens.lens (type' :: EC2TagFilter -> Lude.Maybe EC2TagFilterType) (\s a -> s {type' = a} :: EC2TagFilter)
{-# DEPRECATED etfType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Lude.FromJSON EC2TagFilter where
  parseJSON =
    Lude.withObject
      "EC2TagFilter"
      ( \x ->
          EC2TagFilter'
            Lude.<$> (x Lude..:? "Value")
            Lude.<*> (x Lude..:? "Key")
            Lude.<*> (x Lude..:? "Type")
      )

instance Lude.ToJSON EC2TagFilter where
  toJSON EC2TagFilter' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Value" Lude..=) Lude.<$> value,
            ("Key" Lude..=) Lude.<$> key,
            ("Type" Lude..=) Lude.<$> type'
          ]
      )
