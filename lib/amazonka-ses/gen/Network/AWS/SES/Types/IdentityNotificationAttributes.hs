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
    inaHeadersInDeliveryNotificationsEnabled,
    inaHeadersInComplaintNotificationsEnabled,
    inaHeadersInBounceNotificationsEnabled,
    inaBounceTopic,
    inaComplaintTopic,
    inaDeliveryTopic,
    inaForwardingEnabled,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents the notification attributes of an identity, including whether an identity has Amazon Simple Notification Service (Amazon SNS) topics set for bounce, complaint, and/or delivery notifications, and whether feedback forwarding is enabled for bounce and complaint notifications.
--
-- /See:/ 'mkIdentityNotificationAttributes' smart constructor.
data IdentityNotificationAttributes = IdentityNotificationAttributes'
  { headersInDeliveryNotificationsEnabled ::
      Lude.Maybe Lude.Bool,
    headersInComplaintNotificationsEnabled ::
      Lude.Maybe Lude.Bool,
    headersInBounceNotificationsEnabled ::
      Lude.Maybe Lude.Bool,
    bounceTopic :: Lude.Text,
    complaintTopic :: Lude.Text,
    deliveryTopic :: Lude.Text,
    forwardingEnabled ::
      Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'IdentityNotificationAttributes' with the minimum fields required to make a request.
--
-- * 'bounceTopic' - The Amazon Resource Name (ARN) of the Amazon SNS topic where Amazon SES will publish bounce notifications.
-- * 'complaintTopic' - The Amazon Resource Name (ARN) of the Amazon SNS topic where Amazon SES will publish complaint notifications.
-- * 'deliveryTopic' - The Amazon Resource Name (ARN) of the Amazon SNS topic where Amazon SES will publish delivery notifications.
-- * 'forwardingEnabled' - Describes whether Amazon SES will forward bounce and complaint notifications as email. @true@ indicates that Amazon SES will forward bounce and complaint notifications as email, while @false@ indicates that bounce and complaint notifications will be published only to the specified bounce and complaint Amazon SNS topics.
-- * 'headersInBounceNotificationsEnabled' - Describes whether Amazon SES includes the original email headers in Amazon SNS notifications of type @Bounce@ . A value of @true@ specifies that Amazon SES will include headers in bounce notifications, and a value of @false@ specifies that Amazon SES will not include headers in bounce notifications.
-- * 'headersInComplaintNotificationsEnabled' - Describes whether Amazon SES includes the original email headers in Amazon SNS notifications of type @Complaint@ . A value of @true@ specifies that Amazon SES will include headers in complaint notifications, and a value of @false@ specifies that Amazon SES will not include headers in complaint notifications.
-- * 'headersInDeliveryNotificationsEnabled' - Describes whether Amazon SES includes the original email headers in Amazon SNS notifications of type @Delivery@ . A value of @true@ specifies that Amazon SES will include headers in delivery notifications, and a value of @false@ specifies that Amazon SES will not include headers in delivery notifications.
mkIdentityNotificationAttributes ::
  -- | 'bounceTopic'
  Lude.Text ->
  -- | 'complaintTopic'
  Lude.Text ->
  -- | 'deliveryTopic'
  Lude.Text ->
  -- | 'forwardingEnabled'
  Lude.Bool ->
  IdentityNotificationAttributes
mkIdentityNotificationAttributes
  pBounceTopic_
  pComplaintTopic_
  pDeliveryTopic_
  pForwardingEnabled_ =
    IdentityNotificationAttributes'
      { headersInDeliveryNotificationsEnabled =
          Lude.Nothing,
        headersInComplaintNotificationsEnabled = Lude.Nothing,
        headersInBounceNotificationsEnabled = Lude.Nothing,
        bounceTopic = pBounceTopic_,
        complaintTopic = pComplaintTopic_,
        deliveryTopic = pDeliveryTopic_,
        forwardingEnabled = pForwardingEnabled_
      }

-- | Describes whether Amazon SES includes the original email headers in Amazon SNS notifications of type @Delivery@ . A value of @true@ specifies that Amazon SES will include headers in delivery notifications, and a value of @false@ specifies that Amazon SES will not include headers in delivery notifications.
--
-- /Note:/ Consider using 'headersInDeliveryNotificationsEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
inaHeadersInDeliveryNotificationsEnabled :: Lens.Lens' IdentityNotificationAttributes (Lude.Maybe Lude.Bool)
inaHeadersInDeliveryNotificationsEnabled = Lens.lens (headersInDeliveryNotificationsEnabled :: IdentityNotificationAttributes -> Lude.Maybe Lude.Bool) (\s a -> s {headersInDeliveryNotificationsEnabled = a} :: IdentityNotificationAttributes)
{-# DEPRECATED inaHeadersInDeliveryNotificationsEnabled "Use generic-lens or generic-optics with 'headersInDeliveryNotificationsEnabled' instead." #-}

-- | Describes whether Amazon SES includes the original email headers in Amazon SNS notifications of type @Complaint@ . A value of @true@ specifies that Amazon SES will include headers in complaint notifications, and a value of @false@ specifies that Amazon SES will not include headers in complaint notifications.
--
-- /Note:/ Consider using 'headersInComplaintNotificationsEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
inaHeadersInComplaintNotificationsEnabled :: Lens.Lens' IdentityNotificationAttributes (Lude.Maybe Lude.Bool)
inaHeadersInComplaintNotificationsEnabled = Lens.lens (headersInComplaintNotificationsEnabled :: IdentityNotificationAttributes -> Lude.Maybe Lude.Bool) (\s a -> s {headersInComplaintNotificationsEnabled = a} :: IdentityNotificationAttributes)
{-# DEPRECATED inaHeadersInComplaintNotificationsEnabled "Use generic-lens or generic-optics with 'headersInComplaintNotificationsEnabled' instead." #-}

-- | Describes whether Amazon SES includes the original email headers in Amazon SNS notifications of type @Bounce@ . A value of @true@ specifies that Amazon SES will include headers in bounce notifications, and a value of @false@ specifies that Amazon SES will not include headers in bounce notifications.
--
-- /Note:/ Consider using 'headersInBounceNotificationsEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
inaHeadersInBounceNotificationsEnabled :: Lens.Lens' IdentityNotificationAttributes (Lude.Maybe Lude.Bool)
inaHeadersInBounceNotificationsEnabled = Lens.lens (headersInBounceNotificationsEnabled :: IdentityNotificationAttributes -> Lude.Maybe Lude.Bool) (\s a -> s {headersInBounceNotificationsEnabled = a} :: IdentityNotificationAttributes)
{-# DEPRECATED inaHeadersInBounceNotificationsEnabled "Use generic-lens or generic-optics with 'headersInBounceNotificationsEnabled' instead." #-}

-- | The Amazon Resource Name (ARN) of the Amazon SNS topic where Amazon SES will publish bounce notifications.
--
-- /Note:/ Consider using 'bounceTopic' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
inaBounceTopic :: Lens.Lens' IdentityNotificationAttributes Lude.Text
inaBounceTopic = Lens.lens (bounceTopic :: IdentityNotificationAttributes -> Lude.Text) (\s a -> s {bounceTopic = a} :: IdentityNotificationAttributes)
{-# DEPRECATED inaBounceTopic "Use generic-lens or generic-optics with 'bounceTopic' instead." #-}

-- | The Amazon Resource Name (ARN) of the Amazon SNS topic where Amazon SES will publish complaint notifications.
--
-- /Note:/ Consider using 'complaintTopic' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
inaComplaintTopic :: Lens.Lens' IdentityNotificationAttributes Lude.Text
inaComplaintTopic = Lens.lens (complaintTopic :: IdentityNotificationAttributes -> Lude.Text) (\s a -> s {complaintTopic = a} :: IdentityNotificationAttributes)
{-# DEPRECATED inaComplaintTopic "Use generic-lens or generic-optics with 'complaintTopic' instead." #-}

-- | The Amazon Resource Name (ARN) of the Amazon SNS topic where Amazon SES will publish delivery notifications.
--
-- /Note:/ Consider using 'deliveryTopic' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
inaDeliveryTopic :: Lens.Lens' IdentityNotificationAttributes Lude.Text
inaDeliveryTopic = Lens.lens (deliveryTopic :: IdentityNotificationAttributes -> Lude.Text) (\s a -> s {deliveryTopic = a} :: IdentityNotificationAttributes)
{-# DEPRECATED inaDeliveryTopic "Use generic-lens or generic-optics with 'deliveryTopic' instead." #-}

-- | Describes whether Amazon SES will forward bounce and complaint notifications as email. @true@ indicates that Amazon SES will forward bounce and complaint notifications as email, while @false@ indicates that bounce and complaint notifications will be published only to the specified bounce and complaint Amazon SNS topics.
--
-- /Note:/ Consider using 'forwardingEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
inaForwardingEnabled :: Lens.Lens' IdentityNotificationAttributes Lude.Bool
inaForwardingEnabled = Lens.lens (forwardingEnabled :: IdentityNotificationAttributes -> Lude.Bool) (\s a -> s {forwardingEnabled = a} :: IdentityNotificationAttributes)
{-# DEPRECATED inaForwardingEnabled "Use generic-lens or generic-optics with 'forwardingEnabled' instead." #-}

instance Lude.FromXML IdentityNotificationAttributes where
  parseXML x =
    IdentityNotificationAttributes'
      Lude.<$> (x Lude..@? "HeadersInDeliveryNotificationsEnabled")
      Lude.<*> (x Lude..@? "HeadersInComplaintNotificationsEnabled")
      Lude.<*> (x Lude..@? "HeadersInBounceNotificationsEnabled")
      Lude.<*> (x Lude..@ "BounceTopic")
      Lude.<*> (x Lude..@ "ComplaintTopic")
      Lude.<*> (x Lude..@ "DeliveryTopic")
      Lude.<*> (x Lude..@ "ForwardingEnabled")
