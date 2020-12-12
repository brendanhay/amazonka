{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SNS.Types.Topic
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SNS.Types.Topic
  ( Topic (..),

    -- * Smart constructor
    mkTopic,

    -- * Lenses
    tTopicARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A wrapper type for the topic's Amazon Resource Name (ARN). To retrieve a topic's attributes, use @GetTopicAttributes@ .
--
-- /See:/ 'mkTopic' smart constructor.
newtype Topic = Topic' {topicARN :: Lude.Maybe Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Topic' with the minimum fields required to make a request.
--
-- * 'topicARN' - The topic's ARN.
mkTopic ::
  Topic
mkTopic = Topic' {topicARN = Lude.Nothing}

-- | The topic's ARN.
--
-- /Note:/ Consider using 'topicARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tTopicARN :: Lens.Lens' Topic (Lude.Maybe Lude.Text)
tTopicARN = Lens.lens (topicARN :: Topic -> Lude.Maybe Lude.Text) (\s a -> s {topicARN = a} :: Topic)
{-# DEPRECATED tTopicARN "Use generic-lens or generic-optics with 'topicARN' instead." #-}

instance Lude.FromXML Topic where
  parseXML x = Topic' Lude.<$> (x Lude..@? "TopicArn")
