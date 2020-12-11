-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.Tag
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.Tag
  ( Tag (..),

    -- * Smart constructor
    mkTag,

    -- * Lenses
    tKey,
    tValue,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A map that contains tag keys and tag values to attach to an AWS X-Ray group or sampling rule. For more information about ways to use tags, see <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS resources> in the /AWS General Reference/ .
--
-- The following restrictions apply to tags:
--
--     * Maximum number of user-applied tags per resource: 50
--
--
--     * Tag keys and values are case sensitive.
--
--
--     * Don't use @aws:@ as a prefix for keys; it's reserved for AWS use. You cannot edit or delete system tags.
--
--
--
-- /See:/ 'mkTag' smart constructor.
data Tag = Tag' {key :: Lude.Text, value :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Tag' with the minimum fields required to make a request.
--
-- * 'key' - A tag key, such as @Stage@ or @Name@ . A tag key cannot be empty. The key can be a maximum of 128 characters, and can contain only Unicode letters, numbers, or separators, or the following special characters: @+ - = . _ : /@
-- * 'value' - An optional tag value, such as @Production@ or @test-only@ . The value can be a maximum of 255 characters, and contain only Unicode letters, numbers, or separators, or the following special characters: @+ - = . _ : /@
mkTag ::
  -- | 'key'
  Lude.Text ->
  -- | 'value'
  Lude.Text ->
  Tag
mkTag pKey_ pValue_ = Tag' {key = pKey_, value = pValue_}

-- | A tag key, such as @Stage@ or @Name@ . A tag key cannot be empty. The key can be a maximum of 128 characters, and can contain only Unicode letters, numbers, or separators, or the following special characters: @+ - = . _ : /@
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tKey :: Lens.Lens' Tag Lude.Text
tKey = Lens.lens (key :: Tag -> Lude.Text) (\s a -> s {key = a} :: Tag)
{-# DEPRECATED tKey "Use generic-lens or generic-optics with 'key' instead." #-}

-- | An optional tag value, such as @Production@ or @test-only@ . The value can be a maximum of 255 characters, and contain only Unicode letters, numbers, or separators, or the following special characters: @+ - = . _ : /@
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tValue :: Lens.Lens' Tag Lude.Text
tValue = Lens.lens (value :: Tag -> Lude.Text) (\s a -> s {value = a} :: Tag)
{-# DEPRECATED tValue "Use generic-lens or generic-optics with 'value' instead." #-}

instance Lude.FromJSON Tag where
  parseJSON =
    Lude.withObject
      "Tag"
      ( \x ->
          Tag' Lude.<$> (x Lude..: "Key") Lude.<*> (x Lude..: "Value")
      )

instance Lude.ToJSON Tag where
  toJSON Tag' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("Key" Lude..= key), Lude.Just ("Value" Lude..= value)]
      )
