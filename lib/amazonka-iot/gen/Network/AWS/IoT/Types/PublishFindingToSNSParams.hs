-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.PublishFindingToSNSParams
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.PublishFindingToSNSParams
  ( PublishFindingToSNSParams (..),

    -- * Smart constructor
    mkPublishFindingToSNSParams,

    -- * Lenses
    pftspTopicARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Parameters to define a mitigation action that publishes findings to Amazon SNS. You can implement your own custom actions in response to the Amazon SNS messages.
--
-- /See:/ 'mkPublishFindingToSNSParams' smart constructor.
newtype PublishFindingToSNSParams = PublishFindingToSNSParams'
  { topicARN ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PublishFindingToSNSParams' with the minimum fields required to make a request.
--
-- * 'topicARN' - The ARN of the topic to which you want to publish the findings.
mkPublishFindingToSNSParams ::
  -- | 'topicARN'
  Lude.Text ->
  PublishFindingToSNSParams
mkPublishFindingToSNSParams pTopicARN_ =
  PublishFindingToSNSParams' {topicARN = pTopicARN_}

-- | The ARN of the topic to which you want to publish the findings.
--
-- /Note:/ Consider using 'topicARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pftspTopicARN :: Lens.Lens' PublishFindingToSNSParams Lude.Text
pftspTopicARN = Lens.lens (topicARN :: PublishFindingToSNSParams -> Lude.Text) (\s a -> s {topicARN = a} :: PublishFindingToSNSParams)
{-# DEPRECATED pftspTopicARN "Use generic-lens or generic-optics with 'topicARN' instead." #-}

instance Lude.FromJSON PublishFindingToSNSParams where
  parseJSON =
    Lude.withObject
      "PublishFindingToSNSParams"
      (\x -> PublishFindingToSNSParams' Lude.<$> (x Lude..: "topicArn"))

instance Lude.ToJSON PublishFindingToSNSParams where
  toJSON PublishFindingToSNSParams' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("topicArn" Lude..= topicARN)])
