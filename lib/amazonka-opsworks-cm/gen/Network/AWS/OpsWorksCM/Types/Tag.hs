{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorksCM.Types.Tag
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorksCM.Types.Tag
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

-- | A map that contains tag keys and tag values to attach to an AWS OpsWorks for Chef Automate or AWS OpsWorks for Puppet Enterprise server. Leading and trailing white spaces are trimmed from both the key and value. A maximum of 50 user-applied tags is allowed for tag-supported AWS OpsWorks-CM resources.
--
-- /See:/ 'mkTag' smart constructor.
data Tag = Tag'
  { -- | An optional tag value, such as @Production@ or @test-owcm-server@ . The value can be a maximum of 255 characters, and contain only Unicode letters, numbers, or separators, or the following special characters: @+ - = . _ : /@
    value :: Lude.Text,
    -- | A tag key, such as @Stage@ or @Name@ . A tag key cannot be empty. The key can be a maximum of 127 characters, and can contain only Unicode letters, numbers, or separators, or the following special characters: @+ - = . _ : /@
    key :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Tag' with the minimum fields required to make a request.
--
-- * 'value' - An optional tag value, such as @Production@ or @test-owcm-server@ . The value can be a maximum of 255 characters, and contain only Unicode letters, numbers, or separators, or the following special characters: @+ - = . _ : /@
-- * 'key' - A tag key, such as @Stage@ or @Name@ . A tag key cannot be empty. The key can be a maximum of 127 characters, and can contain only Unicode letters, numbers, or separators, or the following special characters: @+ - = . _ : /@
mkTag ::
  -- | 'value'
  Lude.Text ->
  -- | 'key'
  Lude.Text ->
  Tag
mkTag pValue_ pKey_ = Tag' {value = pValue_, key = pKey_}

-- | An optional tag value, such as @Production@ or @test-owcm-server@ . The value can be a maximum of 255 characters, and contain only Unicode letters, numbers, or separators, or the following special characters: @+ - = . _ : /@
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tValue :: Lens.Lens' Tag Lude.Text
tValue = Lens.lens (value :: Tag -> Lude.Text) (\s a -> s {value = a} :: Tag)
{-# DEPRECATED tValue "Use generic-lens or generic-optics with 'value' instead." #-}

-- | A tag key, such as @Stage@ or @Name@ . A tag key cannot be empty. The key can be a maximum of 127 characters, and can contain only Unicode letters, numbers, or separators, or the following special characters: @+ - = . _ : /@
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tKey :: Lens.Lens' Tag Lude.Text
tKey = Lens.lens (key :: Tag -> Lude.Text) (\s a -> s {key = a} :: Tag)
{-# DEPRECATED tKey "Use generic-lens or generic-optics with 'key' instead." #-}

instance Lude.FromJSON Tag where
  parseJSON =
    Lude.withObject
      "Tag"
      ( \x ->
          Tag' Lude.<$> (x Lude..: "Value") Lude.<*> (x Lude..: "Key")
      )

instance Lude.ToJSON Tag where
  toJSON Tag' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("Value" Lude..= value), Lude.Just ("Key" Lude..= key)]
      )
