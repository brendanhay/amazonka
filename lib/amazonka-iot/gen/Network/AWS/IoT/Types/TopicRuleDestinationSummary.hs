{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.TopicRuleDestinationSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.TopicRuleDestinationSummary
  ( TopicRuleDestinationSummary (..),

    -- * Smart constructor
    mkTopicRuleDestinationSummary,

    -- * Lenses
    trdsStatus,
    trdsHttpURLSummary,
    trdsArn,
    trdsStatusReason,
  )
where

import Network.AWS.IoT.Types.HTTPURLDestinationSummary
import Network.AWS.IoT.Types.TopicRuleDestinationStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about the topic rule destination.
--
-- /See:/ 'mkTopicRuleDestinationSummary' smart constructor.
data TopicRuleDestinationSummary = TopicRuleDestinationSummary'
  { status ::
      Lude.Maybe
        TopicRuleDestinationStatus,
    httpURLSummary ::
      Lude.Maybe
        HTTPURLDestinationSummary,
    arn :: Lude.Maybe Lude.Text,
    statusReason ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TopicRuleDestinationSummary' with the minimum fields required to make a request.
--
-- * 'arn' - The topic rule destination ARN.
-- * 'httpURLSummary' - Information about the HTTP URL.
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
-- * 'statusReason' - The reason the topic rule destination is in the current status.
mkTopicRuleDestinationSummary ::
  TopicRuleDestinationSummary
mkTopicRuleDestinationSummary =
  TopicRuleDestinationSummary'
    { status = Lude.Nothing,
      httpURLSummary = Lude.Nothing,
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
trdsStatus :: Lens.Lens' TopicRuleDestinationSummary (Lude.Maybe TopicRuleDestinationStatus)
trdsStatus = Lens.lens (status :: TopicRuleDestinationSummary -> Lude.Maybe TopicRuleDestinationStatus) (\s a -> s {status = a} :: TopicRuleDestinationSummary)
{-# DEPRECATED trdsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | Information about the HTTP URL.
--
-- /Note:/ Consider using 'httpURLSummary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trdsHttpURLSummary :: Lens.Lens' TopicRuleDestinationSummary (Lude.Maybe HTTPURLDestinationSummary)
trdsHttpURLSummary = Lens.lens (httpURLSummary :: TopicRuleDestinationSummary -> Lude.Maybe HTTPURLDestinationSummary) (\s a -> s {httpURLSummary = a} :: TopicRuleDestinationSummary)
{-# DEPRECATED trdsHttpURLSummary "Use generic-lens or generic-optics with 'httpURLSummary' instead." #-}

-- | The topic rule destination ARN.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trdsArn :: Lens.Lens' TopicRuleDestinationSummary (Lude.Maybe Lude.Text)
trdsArn = Lens.lens (arn :: TopicRuleDestinationSummary -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: TopicRuleDestinationSummary)
{-# DEPRECATED trdsArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The reason the topic rule destination is in the current status.
--
-- /Note:/ Consider using 'statusReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trdsStatusReason :: Lens.Lens' TopicRuleDestinationSummary (Lude.Maybe Lude.Text)
trdsStatusReason = Lens.lens (statusReason :: TopicRuleDestinationSummary -> Lude.Maybe Lude.Text) (\s a -> s {statusReason = a} :: TopicRuleDestinationSummary)
{-# DEPRECATED trdsStatusReason "Use generic-lens or generic-optics with 'statusReason' instead." #-}

instance Lude.FromJSON TopicRuleDestinationSummary where
  parseJSON =
    Lude.withObject
      "TopicRuleDestinationSummary"
      ( \x ->
          TopicRuleDestinationSummary'
            Lude.<$> (x Lude..:? "status")
            Lude.<*> (x Lude..:? "httpUrlSummary")
            Lude.<*> (x Lude..:? "arn")
            Lude.<*> (x Lude..:? "statusReason")
      )
