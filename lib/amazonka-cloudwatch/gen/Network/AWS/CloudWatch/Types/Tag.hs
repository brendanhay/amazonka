{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.Types.Tag
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatch.Types.Tag
  ( Tag (..),

    -- * Smart constructor
    mkTag,

    -- * Lenses
    tValue,
    tKey,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A key-value pair associated with a CloudWatch resource.
--
-- /See:/ 'mkTag' smart constructor.
data Tag = Tag'
  { -- | The value for the specified tag key.
    value :: Lude.Text,
    -- | A string that you can use to assign a value. The combination of tag keys and values can help you organize and categorize your resources.
    key :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Tag' with the minimum fields required to make a request.
--
-- * 'value' - The value for the specified tag key.
-- * 'key' - A string that you can use to assign a value. The combination of tag keys and values can help you organize and categorize your resources.
mkTag ::
  -- | 'value'
  Lude.Text ->
  -- | 'key'
  Lude.Text ->
  Tag
mkTag pValue_ pKey_ = Tag' {value = pValue_, key = pKey_}

-- | The value for the specified tag key.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tValue :: Lens.Lens' Tag Lude.Text
tValue = Lens.lens (value :: Tag -> Lude.Text) (\s a -> s {value = a} :: Tag)
{-# DEPRECATED tValue "Use generic-lens or generic-optics with 'value' instead." #-}

-- | A string that you can use to assign a value. The combination of tag keys and values can help you organize and categorize your resources.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tKey :: Lens.Lens' Tag Lude.Text
tKey = Lens.lens (key :: Tag -> Lude.Text) (\s a -> s {key = a} :: Tag)
{-# DEPRECATED tKey "Use generic-lens or generic-optics with 'key' instead." #-}

instance Lude.FromXML Tag where
  parseXML x =
    Tag' Lude.<$> (x Lude..@ "Value") Lude.<*> (x Lude..@ "Key")

instance Lude.ToQuery Tag where
  toQuery Tag' {..} =
    Lude.mconcat ["Value" Lude.=: value, "Key" Lude.=: key]
