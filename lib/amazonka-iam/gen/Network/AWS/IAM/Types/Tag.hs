-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.Tag
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.Tag
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

-- | A structure that represents user-provided metadata that can be associated with a resource such as an IAM user or role. For more information about tagging, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM Identities> in the /IAM User Guide/ .
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
-- * 'key' - The key name that can be used to look up or retrieve the associated value. For example, @Department@ or @Cost Center@ are common choices.
-- * 'value' - The value associated with this tag. For example, tags with a key name of @Department@ could have values such as @Human Resources@ , @Accounting@ , and @Support@ . Tags with a key name of @Cost Center@ might have values that consist of the number associated with the different cost centers in your company. Typically, many resources have tags with the same key name but with different values.
mkTag ::
  -- | 'key'
  Lude.Text ->
  -- | 'value'
  Lude.Text ->
  Tag
mkTag pKey_ pValue_ = Tag' {key = pKey_, value = pValue_}

-- | The key name that can be used to look up or retrieve the associated value. For example, @Department@ or @Cost Center@ are common choices.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tKey :: Lens.Lens' Tag Lude.Text
tKey = Lens.lens (key :: Tag -> Lude.Text) (\s a -> s {key = a} :: Tag)
{-# DEPRECATED tKey "Use generic-lens or generic-optics with 'key' instead." #-}

-- | The value associated with this tag. For example, tags with a key name of @Department@ could have values such as @Human Resources@ , @Accounting@ , and @Support@ . Tags with a key name of @Cost Center@ might have values that consist of the number associated with the different cost centers in your company. Typically, many resources have tags with the same key name but with different values.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tValue :: Lens.Lens' Tag Lude.Text
tValue = Lens.lens (value :: Tag -> Lude.Text) (\s a -> s {value = a} :: Tag)
{-# DEPRECATED tValue "Use generic-lens or generic-optics with 'value' instead." #-}

instance Lude.FromXML Tag where
  parseXML x =
    Tag' Lude.<$> (x Lude..@ "Key") Lude.<*> (x Lude..@ "Value")

instance Lude.ToQuery Tag where
  toQuery Tag' {..} =
    Lude.mconcat ["Key" Lude.=: key, "Value" Lude.=: value]
