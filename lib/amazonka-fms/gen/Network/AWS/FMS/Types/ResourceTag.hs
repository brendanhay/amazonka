{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.Types.ResourceTag
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.FMS.Types.ResourceTag
  ( ResourceTag (..),

    -- * Smart constructor
    mkResourceTag,

    -- * Lenses
    rtValue,
    rtKey,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The resource tags that AWS Firewall Manager uses to determine if a particular resource should be included or excluded from the AWS Firewall Manager policy. Tags enable you to categorize your AWS resources in different ways, for example, by purpose, owner, or environment. Each tag consists of a key and an optional value. Firewall Manager combines the tags with "AND" so that, if you add more than one tag to a policy scope, a resource must have all the specified tags to be included or excluded. For more information, see <https://docs.aws.amazon.com/awsconsolehelpdocs/latest/gsg/tag-editor.html Working with Tag Editor> .
--
-- /See:/ 'mkResourceTag' smart constructor.
data ResourceTag = ResourceTag'
  { -- | The resource tag value.
    value :: Lude.Maybe Lude.Text,
    -- | The resource tag key.
    key :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ResourceTag' with the minimum fields required to make a request.
--
-- * 'value' - The resource tag value.
-- * 'key' - The resource tag key.
mkResourceTag ::
  -- | 'key'
  Lude.Text ->
  ResourceTag
mkResourceTag pKey_ =
  ResourceTag' {value = Lude.Nothing, key = pKey_}

-- | The resource tag value.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtValue :: Lens.Lens' ResourceTag (Lude.Maybe Lude.Text)
rtValue = Lens.lens (value :: ResourceTag -> Lude.Maybe Lude.Text) (\s a -> s {value = a} :: ResourceTag)
{-# DEPRECATED rtValue "Use generic-lens or generic-optics with 'value' instead." #-}

-- | The resource tag key.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtKey :: Lens.Lens' ResourceTag Lude.Text
rtKey = Lens.lens (key :: ResourceTag -> Lude.Text) (\s a -> s {key = a} :: ResourceTag)
{-# DEPRECATED rtKey "Use generic-lens or generic-optics with 'key' instead." #-}

instance Lude.FromJSON ResourceTag where
  parseJSON =
    Lude.withObject
      "ResourceTag"
      ( \x ->
          ResourceTag'
            Lude.<$> (x Lude..:? "Value") Lude.<*> (x Lude..: "Key")
      )

instance Lude.ToJSON ResourceTag where
  toJSON ResourceTag' {..} =
    Lude.object
      ( Lude.catMaybes
          [("Value" Lude..=) Lude.<$> value, Lude.Just ("Key" Lude..= key)]
      )
