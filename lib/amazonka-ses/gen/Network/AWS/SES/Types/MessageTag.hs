{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types.MessageTag
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SES.Types.MessageTag
  ( MessageTag (..),

    -- * Smart constructor
    mkMessageTag,

    -- * Lenses
    mtName,
    mtValue,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains the name and value of a tag that you can provide to @SendEmail@ or @SendRawEmail@ to apply to an email.
--
-- Message tags, which you use with configuration sets, enable you to publish email sending events. For information about using configuration sets, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html Amazon SES Developer Guide> .
--
-- /See:/ 'mkMessageTag' smart constructor.
data MessageTag = MessageTag'
  { name :: Lude.Text,
    value :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MessageTag' with the minimum fields required to make a request.
--
-- * 'name' - The name of the tag. The name must:
--
--
--     * This value can only contain ASCII letters (a-z, A-Z), numbers (0-9), underscores (_), or dashes (-).
--
--
--     * Contain less than 256 characters.
--
--
-- * 'value' - The value of the tag. The value must:
--
--
--     * This value can only contain ASCII letters (a-z, A-Z), numbers (0-9), underscores (_), or dashes (-).
--
--
--     * Contain less than 256 characters.
mkMessageTag ::
  -- | 'name'
  Lude.Text ->
  -- | 'value'
  Lude.Text ->
  MessageTag
mkMessageTag pName_ pValue_ =
  MessageTag' {name = pName_, value = pValue_}

-- | The name of the tag. The name must:
--
--
--     * This value can only contain ASCII letters (a-z, A-Z), numbers (0-9), underscores (_), or dashes (-).
--
--
--     * Contain less than 256 characters.
--
--
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtName :: Lens.Lens' MessageTag Lude.Text
mtName = Lens.lens (name :: MessageTag -> Lude.Text) (\s a -> s {name = a} :: MessageTag)
{-# DEPRECATED mtName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The value of the tag. The value must:
--
--
--     * This value can only contain ASCII letters (a-z, A-Z), numbers (0-9), underscores (_), or dashes (-).
--
--
--     * Contain less than 256 characters.
--
--
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtValue :: Lens.Lens' MessageTag Lude.Text
mtValue = Lens.lens (value :: MessageTag -> Lude.Text) (\s a -> s {value = a} :: MessageTag)
{-# DEPRECATED mtValue "Use generic-lens or generic-optics with 'value' instead." #-}

instance Lude.ToQuery MessageTag where
  toQuery MessageTag' {..} =
    Lude.mconcat ["Name" Lude.=: name, "Value" Lude.=: value]
