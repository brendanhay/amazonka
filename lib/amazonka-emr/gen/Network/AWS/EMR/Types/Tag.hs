-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.Tag
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.Tag
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

-- | A key-value pair containing user-defined metadata that you can associate with an Amazon EMR resource. Tags make it easier to associate clusters in various ways, such as grouping clusters to track your Amazon EMR resource allocation costs. For more information, see <https://docs.aws.amazon.com/emr/latest/ManagementGuide/emr-plan-tags.html Tag Clusters> .
--
-- /See:/ 'mkTag' smart constructor.
data Tag = Tag'
  { value :: Lude.Maybe Lude.Text,
    key :: Lude.Maybe Lude.Text
  }
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
-- * 'key' - A user-defined key, which is the minimum required information for a valid tag. For more information, see <https://docs.aws.amazon.com/emr/latest/ManagementGuide/emr-plan-tags.html Tag > .
-- * 'value' - A user-defined value, which is optional in a tag. For more information, see <https://docs.aws.amazon.com/emr/latest/ManagementGuide/emr-plan-tags.html Tag Clusters> .
mkTag ::
  Tag
mkTag = Tag' {value = Lude.Nothing, key = Lude.Nothing}

-- | A user-defined value, which is optional in a tag. For more information, see <https://docs.aws.amazon.com/emr/latest/ManagementGuide/emr-plan-tags.html Tag Clusters> .
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tValue :: Lens.Lens' Tag (Lude.Maybe Lude.Text)
tValue = Lens.lens (value :: Tag -> Lude.Maybe Lude.Text) (\s a -> s {value = a} :: Tag)
{-# DEPRECATED tValue "Use generic-lens or generic-optics with 'value' instead." #-}

-- | A user-defined key, which is the minimum required information for a valid tag. For more information, see <https://docs.aws.amazon.com/emr/latest/ManagementGuide/emr-plan-tags.html Tag > .
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tKey :: Lens.Lens' Tag (Lude.Maybe Lude.Text)
tKey = Lens.lens (key :: Tag -> Lude.Maybe Lude.Text) (\s a -> s {key = a} :: Tag)
{-# DEPRECATED tKey "Use generic-lens or generic-optics with 'key' instead." #-}

instance Lude.FromJSON Tag where
  parseJSON =
    Lude.withObject
      "Tag"
      ( \x ->
          Tag' Lude.<$> (x Lude..:? "Value") Lude.<*> (x Lude..:? "Key")
      )

instance Lude.ToJSON Tag where
  toJSON Tag' {..} =
    Lude.object
      ( Lude.catMaybes
          [("Value" Lude..=) Lude.<$> value, ("Key" Lude..=) Lude.<$> key]
      )
