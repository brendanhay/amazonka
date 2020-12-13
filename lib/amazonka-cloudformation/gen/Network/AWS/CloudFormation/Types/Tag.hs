{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.Tag
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.Tag
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

-- | The Tag type enables you to specify a key-value pair that can be used to store information about an AWS CloudFormation stack.
--
-- /See:/ 'mkTag' smart constructor.
data Tag = Tag'
  { -- | /Required/ . A string containing the value for this tag. You can specify a maximum of 256 characters for a tag value.
    value :: Lude.Text,
    -- | /Required/ . A string used to identify this tag. You can specify a maximum of 128 characters for a tag key. Tags owned by Amazon Web Services (AWS) have the reserved prefix: @aws:@ .
    key :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Tag' with the minimum fields required to make a request.
--
-- * 'value' - /Required/ . A string containing the value for this tag. You can specify a maximum of 256 characters for a tag value.
-- * 'key' - /Required/ . A string used to identify this tag. You can specify a maximum of 128 characters for a tag key. Tags owned by Amazon Web Services (AWS) have the reserved prefix: @aws:@ .
mkTag ::
  -- | 'value'
  Lude.Text ->
  -- | 'key'
  Lude.Text ->
  Tag
mkTag pValue_ pKey_ = Tag' {value = pValue_, key = pKey_}

-- | /Required/ . A string containing the value for this tag. You can specify a maximum of 256 characters for a tag value.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tValue :: Lens.Lens' Tag Lude.Text
tValue = Lens.lens (value :: Tag -> Lude.Text) (\s a -> s {value = a} :: Tag)
{-# DEPRECATED tValue "Use generic-lens or generic-optics with 'value' instead." #-}

-- | /Required/ . A string used to identify this tag. You can specify a maximum of 128 characters for a tag key. Tags owned by Amazon Web Services (AWS) have the reserved prefix: @aws:@ .
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
