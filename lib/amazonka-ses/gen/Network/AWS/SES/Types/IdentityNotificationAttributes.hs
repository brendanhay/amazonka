{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types.IdentityNotificationAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SES.Types.IdentityNotificationAttributes
  ( IdentityNotificationAttributes (..),

    -- * Smart constructor
    mkIdentityNotificationAttributes,

    -- * Lenses
    inaBounceTopic,
    inaComplaintTopic,
    inaDeliveryTopic,
    inaForwardingEnabled,
    inaHeadersInBounceNotificationsEnabled,
    inaHeadersInComplaintNotificationsEnabled,
    inaHeadersInDeliveryNotificationsEnabled,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SES.Types.BounceTopic as Types
import qualified Network.AWS.SES.Types.ComplaintTopic as Types
import qualified Network.AWS.SES.Types.DeliveryTopic as Types

-- | Represents the notification attributes of an identity, including whether an identity has Amazon Simple Notification Service (Amazon SNS) topics set for bounce, complaint, and/or delivery notifications, and whether feedback forwarding is enabled for bounce and complaint notifications.
--
-- /See:/ 'mkIdentityNotificationAttributes' smart constructor.
data IdentityNotificationAttributes = IdentityNotificationAttributes'
  { -- | The Amazon Resource Name (ARN) of the Amazon SNS topic where Amazon SES will publish bounce notifications.
    bounceTopic :: Types.BounceTopic,
    -- | The Amazon Resource Name (ARN) of the Amazon SNS topic where Amazon SES will publish complaint notifications.
    complaintTopic :: Types.ComplaintTopic,
    -- | The Amazon Resource Name (ARN) of the Amazon SNS topic where Amazon SES will publish delivery notifications.
    deliveryTopic :: Types.DeliveryTopic,
    -- | Describes whether Amazon SES will forward bounce and complaint notifications as email. @true@ indicates that Amazon SES will forward bounce and complaint notifications as email, while @false@ indicates that bounce and complaint notifications will be published only to the specified bounce and complaint Amazon SNS topics.
    forwardingEnabled :: Core.Bool,
    -- | Describes whether Amazon SES includes the original email headers in Amazon SNS notifications of type @Bounce@ . A value of @true@ specifies that Amazon SES will include headers in bounce notifications, and a value of @false@ specifies that Amazon SES will not include headers in bounce notifications.
    headersInBounceNotificationsEnabled :: Core.Maybe Core.Bool,
    -- | Describes whether Amazon SES includes the original email headers in Amazon SNS notifications of type @Complaint@ . A value of @true@ specifies that Amazon SES will include headers in complaint notifications, and a value of @false@ specifies that Amazon SES will not include headers in complaint notifications.
    headersInComplaintNotificationsEnabled :: Core.Maybe Core.Bool,
    -- | Describes whether Amazon SES includes the original email headers in Amazon SNS notifications of type @Delivery@ . A value of @true@ specifies that Amazon SES will include headers in delivery notifications, and a value of @false@ specifies that Amazon SES will not include headers in delivery notifications.
    headersInDeliveryNotificationsEnabled :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'IdentityNotificationAttributes' value with any optional fields omitted.
mkIdentityNotificationAttributes ::
  -- | 'bounceTopic'
  Types.BounceTopic ->
  -- | 'complaintTopic'
  Types.ComplaintTopic ->
  -- | 'deliveryTopic'
  Types.DeliveryTopic ->
  -- | 'forwardingEnabled'
  Core.Bool ->
  IdentityNotificationAttributes
mkIdentityNotificationAttributes
  bounceTopic
  complaintTopic
  deliveryTopic
  forwardingEnabled =
    IdentityNotificationAttributes'
      { bounceTopic,
        complaintTopic,
        deliveryTopic,
        forwardingEnabled,
        headersInBounceNotificationsEnabled = Core.Nothing,
        headersInComplaintNotificationsEnabled = Core.Nothing,
        headersInDeliveryNotificationsEnabled = Core.Nothing
      }

-- | The Amazon Resource Name (ARN) of the Amazon SNS topic where Amazon SES will publish bounce notifications.
--
-- /Note:/ Consider using 'bounceTopic' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
inaBounceTopic :: Lens.Lens' IdentityNotificationAttributes Types.BounceTopic
inaBounceTopic = Lens.field @"bounceTopic"
{-# DEPRECATED inaBounceTopic "Use generic-lens or generic-optics with 'bounceTopic' instead." #-}

-- | The Amazon Resource Name (ARN) of the Amazon SNS topic where Amazon SES will publish complaint notifications.
--
-- /Note:/ Consider using 'complaintTopic' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
inaComplaintTopic :: Lens.Lens' IdentityNotificationAttributes Types.ComplaintTopic
inaComplaintTopic = Lens.field @"complaintTopic"
{-# DEPRECATED inaComplaintTopic "Use generic-lens or generic-optics with 'complaintTopic' instead." #-}

-- | The Amazon Resource Name (ARN) of the Amazon SNS topic where Amazon SES will publish delivery notifications.
--
-- /Note:/ Consider using 'deliveryTopic' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
inaDeliveryTopic :: Lens.Lens' IdentityNotificationAttributes Types.DeliveryTopic
inaDeliveryTopic = Lens.field @"deliveryTopic"
{-# DEPRECATED inaDeliveryTopic "Use generic-lens or generic-optics with 'deliveryTopic' instead." #-}

-- | Describes whether Amazon SES will forward bounce and complaint notifications as email. @true@ indicates that Amazon SES will forward bounce and complaint notifications as email, while @false@ indicates that bounce and complaint notifications will be published only to the specified bounce and complaint Amazon SNS topics.
--
-- /Note:/ Consider using 'forwardingEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
inaForwardingEnabled :: Lens.Lens' IdentityNotificationAttributes Core.Bool
inaForwardingEnabled = Lens.field @"forwardingEnabled"
{-# DEPRECATED inaForwardingEnabled "Use generic-lens or generic-optics with 'forwardingEnabled' instead." #-}

-- | Describes whether Amazon SES includes the original email headers in Amazon SNS notifications of type @Bounce@ . A value of @true@ specifies that Amazon SES will include headers in bounce notifications, and a value of @false@ specifies that Amazon SES will not include headers in bounce notifications.
--
-- /Note:/ Consider using 'headersInBounceNotificationsEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
inaHeadersInBounceNotificationsEnabled :: Lens.Lens' IdentityNotificationAttributes (Core.Maybe Core.Bool)
inaHeadersInBounceNotificationsEnabled = Lens.field @"headersInBounceNotificationsEnabled"
{-# DEPRECATED inaHeadersInBounceNotificationsEnabled "Use generic-lens or generic-optics with 'headersInBounceNotificationsEnabled' instead." #-}

-- | Describes whether Amazon SES includes the original email headers in Amazon SNS notifications of type @Complaint@ . A value of @true@ specifies that Amazon SES will include headers in complaint notifications, and a value of @false@ specifies that Amazon SES will not include headers in complaint notifications.
--
-- /Note:/ Consider using 'headersInComplaintNotificationsEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
inaHeadersInComplaintNotificationsEnabled :: Lens.Lens' IdentityNotificationAttributes (Core.Maybe Core.Bool)
inaHeadersInComplaintNotificationsEnabled = Lens.field @"headersInComplaintNotificationsEnabled"
{-# DEPRECATED inaHeadersInComplaintNotificationsEnabled "Use generic-lens or generic-optics with 'headersInComplaintNotificationsEnabled' instead." #-}

-- | Describes whether Amazon SES includes the original email headers in Amazon SNS notifications of type @Delivery@ . A value of @true@ specifies that Amazon SES will include headers in delivery notifications, and a value of @false@ specifies that Amazon SES will not include headers in delivery notifications.
--
-- /Note:/ Consider using 'headersInDeliveryNotificationsEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
inaHeadersInDeliveryNotificationsEnabled :: Lens.Lens' IdentityNotificationAttributes (Core.Maybe Core.Bool)
inaHeadersInDeliveryNotificationsEnabled = Lens.field @"headersInDeliveryNotificationsEnabled"
{-# DEPRECATED inaHeadersInDeliveryNotificationsEnabled "Use generic-lens or generic-optics with 'headersInDeliveryNotificationsEnabled' instead." #-}

instance Core.FromXML IdentityNotificationAttributes where
  parseXML x =
    IdentityNotificationAttributes'
      Core.<$> (x Core..@ "BounceTopic")
      Core.<*> (x Core..@ "ComplaintTopic")
      Core.<*> (x Core..@ "DeliveryTopic")
      Core.<*> (x Core..@ "ForwardingEnabled")
      Core.<*> (x Core..@? "HeadersInBounceNotificationsEnabled")
      Core.<*> (x Core..@? "HeadersInComplaintNotificationsEnabled")
      Core.<*> (x Core..@? "HeadersInDeliveryNotificationsEnabled")
