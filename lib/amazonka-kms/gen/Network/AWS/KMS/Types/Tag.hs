{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.Types.Tag
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KMS.Types.Tag
  ( Tag (..),

    -- * Smart constructor
    mkTag,

    -- * Lenses
    tTagValue,
    tTagKey,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A key-value pair. A tag consists of a tag key and a tag value. Tag keys and tag values are both required, but tag values can be empty (null) strings.
--
-- For information about the rules that apply to tag keys and tag values, see <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/allocation-tag-restrictions.html User-Defined Tag Restrictions> in the /AWS Billing and Cost Management User Guide/ .
--
-- /See:/ 'mkTag' smart constructor.
data Tag = Tag'
  { -- | The value of the tag.
    tagValue :: Lude.Text,
    -- | The key of the tag.
    tagKey :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Tag' with the minimum fields required to make a request.
--
-- * 'tagValue' - The value of the tag.
-- * 'tagKey' - The key of the tag.
mkTag ::
  -- | 'tagValue'
  Lude.Text ->
  -- | 'tagKey'
  Lude.Text ->
  Tag
mkTag pTagValue_ pTagKey_ =
  Tag' {tagValue = pTagValue_, tagKey = pTagKey_}

-- | The value of the tag.
--
-- /Note:/ Consider using 'tagValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tTagValue :: Lens.Lens' Tag Lude.Text
tTagValue = Lens.lens (tagValue :: Tag -> Lude.Text) (\s a -> s {tagValue = a} :: Tag)
{-# DEPRECATED tTagValue "Use generic-lens or generic-optics with 'tagValue' instead." #-}

-- | The key of the tag.
--
-- /Note:/ Consider using 'tagKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tTagKey :: Lens.Lens' Tag Lude.Text
tTagKey = Lens.lens (tagKey :: Tag -> Lude.Text) (\s a -> s {tagKey = a} :: Tag)
{-# DEPRECATED tTagKey "Use generic-lens or generic-optics with 'tagKey' instead." #-}

instance Lude.FromJSON Tag where
  parseJSON =
    Lude.withObject
      "Tag"
      ( \x ->
          Tag' Lude.<$> (x Lude..: "TagValue") Lude.<*> (x Lude..: "TagKey")
      )

instance Lude.ToJSON Tag where
  toJSON Tag' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("TagValue" Lude..= tagValue),
            Lude.Just ("TagKey" Lude..= tagKey)
          ]
      )
