{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.TagOptionDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.TagOptionDetail
  ( TagOptionDetail (..),

    -- * Smart constructor
    mkTagOptionDetail,

    -- * Lenses
    todValue,
    todActive,
    todKey,
    todId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about a TagOption.
--
-- /See:/ 'mkTagOptionDetail' smart constructor.
data TagOptionDetail = TagOptionDetail'
  { value ::
      Lude.Maybe Lude.Text,
    active :: Lude.Maybe Lude.Bool,
    key :: Lude.Maybe Lude.Text,
    id :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TagOptionDetail' with the minimum fields required to make a request.
--
-- * 'active' - The TagOption active state.
-- * 'id' - The TagOption identifier.
-- * 'key' - The TagOption key.
-- * 'value' - The TagOption value.
mkTagOptionDetail ::
  TagOptionDetail
mkTagOptionDetail =
  TagOptionDetail'
    { value = Lude.Nothing,
      active = Lude.Nothing,
      key = Lude.Nothing,
      id = Lude.Nothing
    }

-- | The TagOption value.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
todValue :: Lens.Lens' TagOptionDetail (Lude.Maybe Lude.Text)
todValue = Lens.lens (value :: TagOptionDetail -> Lude.Maybe Lude.Text) (\s a -> s {value = a} :: TagOptionDetail)
{-# DEPRECATED todValue "Use generic-lens or generic-optics with 'value' instead." #-}

-- | The TagOption active state.
--
-- /Note:/ Consider using 'active' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
todActive :: Lens.Lens' TagOptionDetail (Lude.Maybe Lude.Bool)
todActive = Lens.lens (active :: TagOptionDetail -> Lude.Maybe Lude.Bool) (\s a -> s {active = a} :: TagOptionDetail)
{-# DEPRECATED todActive "Use generic-lens or generic-optics with 'active' instead." #-}

-- | The TagOption key.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
todKey :: Lens.Lens' TagOptionDetail (Lude.Maybe Lude.Text)
todKey = Lens.lens (key :: TagOptionDetail -> Lude.Maybe Lude.Text) (\s a -> s {key = a} :: TagOptionDetail)
{-# DEPRECATED todKey "Use generic-lens or generic-optics with 'key' instead." #-}

-- | The TagOption identifier.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
todId :: Lens.Lens' TagOptionDetail (Lude.Maybe Lude.Text)
todId = Lens.lens (id :: TagOptionDetail -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: TagOptionDetail)
{-# DEPRECATED todId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.FromJSON TagOptionDetail where
  parseJSON =
    Lude.withObject
      "TagOptionDetail"
      ( \x ->
          TagOptionDetail'
            Lude.<$> (x Lude..:? "Value")
            Lude.<*> (x Lude..:? "Active")
            Lude.<*> (x Lude..:? "Key")
            Lude.<*> (x Lude..:? "Id")
      )
