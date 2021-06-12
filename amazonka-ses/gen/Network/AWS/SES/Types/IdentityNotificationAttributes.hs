{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types.IdentityNotificationAttributes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SES.Types.IdentityNotificationAttributes where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Represents the notification attributes of an identity, including whether
-- an identity has Amazon Simple Notification Service (Amazon SNS) topics
-- set for bounce, complaint, and\/or delivery notifications, and whether
-- feedback forwarding is enabled for bounce and complaint notifications.
--
-- /See:/ 'newIdentityNotificationAttributes' smart constructor.
data IdentityNotificationAttributes = IdentityNotificationAttributes'
  { -- | Describes whether Amazon SES includes the original email headers in
    -- Amazon SNS notifications of type @Complaint@. A value of @true@
    -- specifies that Amazon SES will include headers in complaint
    -- notifications, and a value of @false@ specifies that Amazon SES will not
    -- include headers in complaint notifications.
    headersInComplaintNotificationsEnabled :: Core.Maybe Core.Bool,
    -- | Describes whether Amazon SES includes the original email headers in
    -- Amazon SNS notifications of type @Delivery@. A value of @true@ specifies
    -- that Amazon SES will include headers in delivery notifications, and a
    -- value of @false@ specifies that Amazon SES will not include headers in
    -- delivery notifications.
    headersInDeliveryNotificationsEnabled :: Core.Maybe Core.Bool,
    -- | Describes whether Amazon SES includes the original email headers in
    -- Amazon SNS notifications of type @Bounce@. A value of @true@ specifies
    -- that Amazon SES will include headers in bounce notifications, and a
    -- value of @false@ specifies that Amazon SES will not include headers in
    -- bounce notifications.
    headersInBounceNotificationsEnabled :: Core.Maybe Core.Bool,
    -- | The Amazon Resource Name (ARN) of the Amazon SNS topic where Amazon SES
    -- will publish bounce notifications.
    bounceTopic :: Core.Text,
    -- | The Amazon Resource Name (ARN) of the Amazon SNS topic where Amazon SES
    -- will publish complaint notifications.
    complaintTopic :: Core.Text,
    -- | The Amazon Resource Name (ARN) of the Amazon SNS topic where Amazon SES
    -- will publish delivery notifications.
    deliveryTopic :: Core.Text,
    -- | Describes whether Amazon SES will forward bounce and complaint
    -- notifications as email. @true@ indicates that Amazon SES will forward
    -- bounce and complaint notifications as email, while @false@ indicates
    -- that bounce and complaint notifications will be published only to the
    -- specified bounce and complaint Amazon SNS topics.
    forwardingEnabled :: Core.Bool
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'IdentityNotificationAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'headersInComplaintNotificationsEnabled', 'identityNotificationAttributes_headersInComplaintNotificationsEnabled' - Describes whether Amazon SES includes the original email headers in
-- Amazon SNS notifications of type @Complaint@. A value of @true@
-- specifies that Amazon SES will include headers in complaint
-- notifications, and a value of @false@ specifies that Amazon SES will not
-- include headers in complaint notifications.
--
-- 'headersInDeliveryNotificationsEnabled', 'identityNotificationAttributes_headersInDeliveryNotificationsEnabled' - Describes whether Amazon SES includes the original email headers in
-- Amazon SNS notifications of type @Delivery@. A value of @true@ specifies
-- that Amazon SES will include headers in delivery notifications, and a
-- value of @false@ specifies that Amazon SES will not include headers in
-- delivery notifications.
--
-- 'headersInBounceNotificationsEnabled', 'identityNotificationAttributes_headersInBounceNotificationsEnabled' - Describes whether Amazon SES includes the original email headers in
-- Amazon SNS notifications of type @Bounce@. A value of @true@ specifies
-- that Amazon SES will include headers in bounce notifications, and a
-- value of @false@ specifies that Amazon SES will not include headers in
-- bounce notifications.
--
-- 'bounceTopic', 'identityNotificationAttributes_bounceTopic' - The Amazon Resource Name (ARN) of the Amazon SNS topic where Amazon SES
-- will publish bounce notifications.
--
-- 'complaintTopic', 'identityNotificationAttributes_complaintTopic' - The Amazon Resource Name (ARN) of the Amazon SNS topic where Amazon SES
-- will publish complaint notifications.
--
-- 'deliveryTopic', 'identityNotificationAttributes_deliveryTopic' - The Amazon Resource Name (ARN) of the Amazon SNS topic where Amazon SES
-- will publish delivery notifications.
--
-- 'forwardingEnabled', 'identityNotificationAttributes_forwardingEnabled' - Describes whether Amazon SES will forward bounce and complaint
-- notifications as email. @true@ indicates that Amazon SES will forward
-- bounce and complaint notifications as email, while @false@ indicates
-- that bounce and complaint notifications will be published only to the
-- specified bounce and complaint Amazon SNS topics.
newIdentityNotificationAttributes ::
  -- | 'bounceTopic'
  Core.Text ->
  -- | 'complaintTopic'
  Core.Text ->
  -- | 'deliveryTopic'
  Core.Text ->
  -- | 'forwardingEnabled'
  Core.Bool ->
  IdentityNotificationAttributes
newIdentityNotificationAttributes
  pBounceTopic_
  pComplaintTopic_
  pDeliveryTopic_
  pForwardingEnabled_ =
    IdentityNotificationAttributes'
      { headersInComplaintNotificationsEnabled =
          Core.Nothing,
        headersInDeliveryNotificationsEnabled =
          Core.Nothing,
        headersInBounceNotificationsEnabled =
          Core.Nothing,
        bounceTopic = pBounceTopic_,
        complaintTopic = pComplaintTopic_,
        deliveryTopic = pDeliveryTopic_,
        forwardingEnabled = pForwardingEnabled_
      }

-- | Describes whether Amazon SES includes the original email headers in
-- Amazon SNS notifications of type @Complaint@. A value of @true@
-- specifies that Amazon SES will include headers in complaint
-- notifications, and a value of @false@ specifies that Amazon SES will not
-- include headers in complaint notifications.
identityNotificationAttributes_headersInComplaintNotificationsEnabled :: Lens.Lens' IdentityNotificationAttributes (Core.Maybe Core.Bool)
identityNotificationAttributes_headersInComplaintNotificationsEnabled = Lens.lens (\IdentityNotificationAttributes' {headersInComplaintNotificationsEnabled} -> headersInComplaintNotificationsEnabled) (\s@IdentityNotificationAttributes' {} a -> s {headersInComplaintNotificationsEnabled = a} :: IdentityNotificationAttributes)

-- | Describes whether Amazon SES includes the original email headers in
-- Amazon SNS notifications of type @Delivery@. A value of @true@ specifies
-- that Amazon SES will include headers in delivery notifications, and a
-- value of @false@ specifies that Amazon SES will not include headers in
-- delivery notifications.
identityNotificationAttributes_headersInDeliveryNotificationsEnabled :: Lens.Lens' IdentityNotificationAttributes (Core.Maybe Core.Bool)
identityNotificationAttributes_headersInDeliveryNotificationsEnabled = Lens.lens (\IdentityNotificationAttributes' {headersInDeliveryNotificationsEnabled} -> headersInDeliveryNotificationsEnabled) (\s@IdentityNotificationAttributes' {} a -> s {headersInDeliveryNotificationsEnabled = a} :: IdentityNotificationAttributes)

-- | Describes whether Amazon SES includes the original email headers in
-- Amazon SNS notifications of type @Bounce@. A value of @true@ specifies
-- that Amazon SES will include headers in bounce notifications, and a
-- value of @false@ specifies that Amazon SES will not include headers in
-- bounce notifications.
identityNotificationAttributes_headersInBounceNotificationsEnabled :: Lens.Lens' IdentityNotificationAttributes (Core.Maybe Core.Bool)
identityNotificationAttributes_headersInBounceNotificationsEnabled = Lens.lens (\IdentityNotificationAttributes' {headersInBounceNotificationsEnabled} -> headersInBounceNotificationsEnabled) (\s@IdentityNotificationAttributes' {} a -> s {headersInBounceNotificationsEnabled = a} :: IdentityNotificationAttributes)

-- | The Amazon Resource Name (ARN) of the Amazon SNS topic where Amazon SES
-- will publish bounce notifications.
identityNotificationAttributes_bounceTopic :: Lens.Lens' IdentityNotificationAttributes Core.Text
identityNotificationAttributes_bounceTopic = Lens.lens (\IdentityNotificationAttributes' {bounceTopic} -> bounceTopic) (\s@IdentityNotificationAttributes' {} a -> s {bounceTopic = a} :: IdentityNotificationAttributes)

-- | The Amazon Resource Name (ARN) of the Amazon SNS topic where Amazon SES
-- will publish complaint notifications.
identityNotificationAttributes_complaintTopic :: Lens.Lens' IdentityNotificationAttributes Core.Text
identityNotificationAttributes_complaintTopic = Lens.lens (\IdentityNotificationAttributes' {complaintTopic} -> complaintTopic) (\s@IdentityNotificationAttributes' {} a -> s {complaintTopic = a} :: IdentityNotificationAttributes)

-- | The Amazon Resource Name (ARN) of the Amazon SNS topic where Amazon SES
-- will publish delivery notifications.
identityNotificationAttributes_deliveryTopic :: Lens.Lens' IdentityNotificationAttributes Core.Text
identityNotificationAttributes_deliveryTopic = Lens.lens (\IdentityNotificationAttributes' {deliveryTopic} -> deliveryTopic) (\s@IdentityNotificationAttributes' {} a -> s {deliveryTopic = a} :: IdentityNotificationAttributes)

-- | Describes whether Amazon SES will forward bounce and complaint
-- notifications as email. @true@ indicates that Amazon SES will forward
-- bounce and complaint notifications as email, while @false@ indicates
-- that bounce and complaint notifications will be published only to the
-- specified bounce and complaint Amazon SNS topics.
identityNotificationAttributes_forwardingEnabled :: Lens.Lens' IdentityNotificationAttributes Core.Bool
identityNotificationAttributes_forwardingEnabled = Lens.lens (\IdentityNotificationAttributes' {forwardingEnabled} -> forwardingEnabled) (\s@IdentityNotificationAttributes' {} a -> s {forwardingEnabled = a} :: IdentityNotificationAttributes)

instance Core.FromXML IdentityNotificationAttributes where
  parseXML x =
    IdentityNotificationAttributes'
      Core.<$> (x Core..@? "HeadersInComplaintNotificationsEnabled")
      Core.<*> (x Core..@? "HeadersInDeliveryNotificationsEnabled")
      Core.<*> (x Core..@? "HeadersInBounceNotificationsEnabled")
      Core.<*> (x Core..@ "BounceTopic")
      Core.<*> (x Core..@ "ComplaintTopic")
      Core.<*> (x Core..@ "DeliveryTopic")
      Core.<*> (x Core..@ "ForwardingEnabled")

instance Core.Hashable IdentityNotificationAttributes

instance Core.NFData IdentityNotificationAttributes
