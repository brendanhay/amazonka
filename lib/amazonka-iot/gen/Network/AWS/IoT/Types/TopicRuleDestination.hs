{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.TopicRuleDestination
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.TopicRuleDestination
  ( TopicRuleDestination (..),

    -- * Smart constructor
    mkTopicRuleDestination,

    -- * Lenses
    trdStatus,
    trdHttpURLProperties,
    trdArn,
    trdStatusReason,
  )
where

import Network.AWS.IoT.Types.HTTPURLDestinationProperties
import Network.AWS.IoT.Types.TopicRuleDestinationStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A topic rule destination.
--
-- /See:/ 'mkTopicRuleDestination' smart constructor.
data TopicRuleDestination = TopicRuleDestination'
  { -- | The status of the topic rule destination. Valid values are:
    --
    --
    --     * IN_PROGRESS
    --
    --     * A topic rule destination was created but has not been confirmed. You can set @status@ to @IN_PROGRESS@ by calling @UpdateTopicRuleDestination@ . Calling @UpdateTopicRuleDestination@ causes a new confirmation challenge to be sent to your confirmation endpoint.
    --
    --
    --     * ENABLED
    --
    --     * Confirmation was completed, and traffic to this destination is allowed. You can set @status@ to @DISABLED@ by calling @UpdateTopicRuleDestination@ .
    --
    --
    --     * DISABLED
    --
    --     * Confirmation was completed, and traffic to this destination is not allowed. You can set @status@ to @ENABLED@ by calling @UpdateTopicRuleDestination@ .
    --
    --
    --     * ERROR
    --
    --     * Confirmation could not be completed, for example if the confirmation timed out. You can call @GetTopicRuleDestination@ for details about the error. You can set @status@ to @IN_PROGRESS@ by calling @UpdateTopicRuleDestination@ . Calling @UpdateTopicRuleDestination@ causes a new confirmation challenge to be sent to your confirmation endpoint.
    status :: Lude.Maybe TopicRuleDestinationStatus,
    -- | Properties of the HTTP URL.
    httpURLProperties :: Lude.Maybe HTTPURLDestinationProperties,
    -- | The topic rule destination URL.
    arn :: Lude.Maybe Lude.Text,
    -- | Additional details or reason why the topic rule destination is in the current status.
    statusReason :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TopicRuleDestination' with the minimum fields required to make a request.
--
-- * 'status' - The status of the topic rule destination. Valid values are:
--
--
--     * IN_PROGRESS
--
--     * A topic rule destination was created but has not been confirmed. You can set @status@ to @IN_PROGRESS@ by calling @UpdateTopicRuleDestination@ . Calling @UpdateTopicRuleDestination@ causes a new confirmation challenge to be sent to your confirmation endpoint.
--
--
--     * ENABLED
--
--     * Confirmation was completed, and traffic to this destination is allowed. You can set @status@ to @DISABLED@ by calling @UpdateTopicRuleDestination@ .
--
--
--     * DISABLED
--
--     * Confirmation was completed, and traffic to this destination is not allowed. You can set @status@ to @ENABLED@ by calling @UpdateTopicRuleDestination@ .
--
--
--     * ERROR
--
--     * Confirmation could not be completed, for example if the confirmation timed out. You can call @GetTopicRuleDestination@ for details about the error. You can set @status@ to @IN_PROGRESS@ by calling @UpdateTopicRuleDestination@ . Calling @UpdateTopicRuleDestination@ causes a new confirmation challenge to be sent to your confirmation endpoint.
--
--
-- * 'httpURLProperties' - Properties of the HTTP URL.
-- * 'arn' - The topic rule destination URL.
-- * 'statusReason' - Additional details or reason why the topic rule destination is in the current status.
mkTopicRuleDestination ::
  TopicRuleDestination
mkTopicRuleDestination =
  TopicRuleDestination'
    { status = Lude.Nothing,
      httpURLProperties = Lude.Nothing,
      arn = Lude.Nothing,
      statusReason = Lude.Nothing
    }

-- | The status of the topic rule destination. Valid values are:
--
--
--     * IN_PROGRESS
--
--     * A topic rule destination was created but has not been confirmed. You can set @status@ to @IN_PROGRESS@ by calling @UpdateTopicRuleDestination@ . Calling @UpdateTopicRuleDestination@ causes a new confirmation challenge to be sent to your confirmation endpoint.
--
--
--     * ENABLED
--
--     * Confirmation was completed, and traffic to this destination is allowed. You can set @status@ to @DISABLED@ by calling @UpdateTopicRuleDestination@ .
--
--
--     * DISABLED
--
--     * Confirmation was completed, and traffic to this destination is not allowed. You can set @status@ to @ENABLED@ by calling @UpdateTopicRuleDestination@ .
--
--
--     * ERROR
--
--     * Confirmation could not be completed, for example if the confirmation timed out. You can call @GetTopicRuleDestination@ for details about the error. You can set @status@ to @IN_PROGRESS@ by calling @UpdateTopicRuleDestination@ . Calling @UpdateTopicRuleDestination@ causes a new confirmation challenge to be sent to your confirmation endpoint.
--
--
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trdStatus :: Lens.Lens' TopicRuleDestination (Lude.Maybe TopicRuleDestinationStatus)
trdStatus = Lens.lens (status :: TopicRuleDestination -> Lude.Maybe TopicRuleDestinationStatus) (\s a -> s {status = a} :: TopicRuleDestination)
{-# DEPRECATED trdStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | Properties of the HTTP URL.
--
-- /Note:/ Consider using 'httpURLProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trdHttpURLProperties :: Lens.Lens' TopicRuleDestination (Lude.Maybe HTTPURLDestinationProperties)
trdHttpURLProperties = Lens.lens (httpURLProperties :: TopicRuleDestination -> Lude.Maybe HTTPURLDestinationProperties) (\s a -> s {httpURLProperties = a} :: TopicRuleDestination)
{-# DEPRECATED trdHttpURLProperties "Use generic-lens or generic-optics with 'httpURLProperties' instead." #-}

-- | The topic rule destination URL.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trdArn :: Lens.Lens' TopicRuleDestination (Lude.Maybe Lude.Text)
trdArn = Lens.lens (arn :: TopicRuleDestination -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: TopicRuleDestination)
{-# DEPRECATED trdArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | Additional details or reason why the topic rule destination is in the current status.
--
-- /Note:/ Consider using 'statusReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trdStatusReason :: Lens.Lens' TopicRuleDestination (Lude.Maybe Lude.Text)
trdStatusReason = Lens.lens (statusReason :: TopicRuleDestination -> Lude.Maybe Lude.Text) (\s a -> s {statusReason = a} :: TopicRuleDestination)
{-# DEPRECATED trdStatusReason "Use generic-lens or generic-optics with 'statusReason' instead." #-}

instance Lude.FromJSON TopicRuleDestination where
  parseJSON =
    Lude.withObject
      "TopicRuleDestination"
      ( \x ->
          TopicRuleDestination'
            Lude.<$> (x Lude..:? "status")
            Lude.<*> (x Lude..:? "httpUrlProperties")
            Lude.<*> (x Lude..:? "arn")
            Lude.<*> (x Lude..:? "statusReason")
      )
