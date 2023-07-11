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
-- Module      : Amazonka.SES.Types.IdentityNotificationAttributes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SES.Types.IdentityNotificationAttributes where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents the notification attributes of an identity, including whether
-- an identity has Amazon Simple Notification Service (Amazon SNS) topics
-- set for bounce, complaint, and\/or delivery notifications, and whether
-- feedback forwarding is enabled for bounce and complaint notifications.
--
-- /See:/ 'newIdentityNotificationAttributes' smart constructor.
data IdentityNotificationAttributes = IdentityNotificationAttributes'
  { -- | Describes whether Amazon SES includes the original email headers in
    -- Amazon SNS notifications of type @Bounce@. A value of @true@ specifies
    -- that Amazon SES will include headers in bounce notifications, and a
    -- value of @false@ specifies that Amazon SES will not include headers in
    -- bounce notifications.
    headersInBounceNotificationsEnabled :: Prelude.Maybe Prelude.Bool,
    -- | Describes whether Amazon SES includes the original email headers in
    -- Amazon SNS notifications of type @Complaint@. A value of @true@
    -- specifies that Amazon SES will include headers in complaint
    -- notifications, and a value of @false@ specifies that Amazon SES will not
    -- include headers in complaint notifications.
    headersInComplaintNotificationsEnabled :: Prelude.Maybe Prelude.Bool,
    -- | Describes whether Amazon SES includes the original email headers in
    -- Amazon SNS notifications of type @Delivery@. A value of @true@ specifies
    -- that Amazon SES will include headers in delivery notifications, and a
    -- value of @false@ specifies that Amazon SES will not include headers in
    -- delivery notifications.
    headersInDeliveryNotificationsEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon Resource Name (ARN) of the Amazon SNS topic where Amazon SES
    -- will publish bounce notifications.
    bounceTopic :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the Amazon SNS topic where Amazon SES
    -- will publish complaint notifications.
    complaintTopic :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the Amazon SNS topic where Amazon SES
    -- will publish delivery notifications.
    deliveryTopic :: Prelude.Text,
    -- | Describes whether Amazon SES will forward bounce and complaint
    -- notifications as email. @true@ indicates that Amazon SES will forward
    -- bounce and complaint notifications as email, while @false@ indicates
    -- that bounce and complaint notifications will be published only to the
    -- specified bounce and complaint Amazon SNS topics.
    forwardingEnabled :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IdentityNotificationAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'headersInBounceNotificationsEnabled', 'identityNotificationAttributes_headersInBounceNotificationsEnabled' - Describes whether Amazon SES includes the original email headers in
-- Amazon SNS notifications of type @Bounce@. A value of @true@ specifies
-- that Amazon SES will include headers in bounce notifications, and a
-- value of @false@ specifies that Amazon SES will not include headers in
-- bounce notifications.
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
  Prelude.Text ->
  -- | 'complaintTopic'
  Prelude.Text ->
  -- | 'deliveryTopic'
  Prelude.Text ->
  -- | 'forwardingEnabled'
  Prelude.Bool ->
  IdentityNotificationAttributes
newIdentityNotificationAttributes
  pBounceTopic_
  pComplaintTopic_
  pDeliveryTopic_
  pForwardingEnabled_ =
    IdentityNotificationAttributes'
      { headersInBounceNotificationsEnabled =
          Prelude.Nothing,
        headersInComplaintNotificationsEnabled =
          Prelude.Nothing,
        headersInDeliveryNotificationsEnabled =
          Prelude.Nothing,
        bounceTopic = pBounceTopic_,
        complaintTopic = pComplaintTopic_,
        deliveryTopic = pDeliveryTopic_,
        forwardingEnabled = pForwardingEnabled_
      }

-- | Describes whether Amazon SES includes the original email headers in
-- Amazon SNS notifications of type @Bounce@. A value of @true@ specifies
-- that Amazon SES will include headers in bounce notifications, and a
-- value of @false@ specifies that Amazon SES will not include headers in
-- bounce notifications.
identityNotificationAttributes_headersInBounceNotificationsEnabled :: Lens.Lens' IdentityNotificationAttributes (Prelude.Maybe Prelude.Bool)
identityNotificationAttributes_headersInBounceNotificationsEnabled = Lens.lens (\IdentityNotificationAttributes' {headersInBounceNotificationsEnabled} -> headersInBounceNotificationsEnabled) (\s@IdentityNotificationAttributes' {} a -> s {headersInBounceNotificationsEnabled = a} :: IdentityNotificationAttributes)

-- | Describes whether Amazon SES includes the original email headers in
-- Amazon SNS notifications of type @Complaint@. A value of @true@
-- specifies that Amazon SES will include headers in complaint
-- notifications, and a value of @false@ specifies that Amazon SES will not
-- include headers in complaint notifications.
identityNotificationAttributes_headersInComplaintNotificationsEnabled :: Lens.Lens' IdentityNotificationAttributes (Prelude.Maybe Prelude.Bool)
identityNotificationAttributes_headersInComplaintNotificationsEnabled = Lens.lens (\IdentityNotificationAttributes' {headersInComplaintNotificationsEnabled} -> headersInComplaintNotificationsEnabled) (\s@IdentityNotificationAttributes' {} a -> s {headersInComplaintNotificationsEnabled = a} :: IdentityNotificationAttributes)

-- | Describes whether Amazon SES includes the original email headers in
-- Amazon SNS notifications of type @Delivery@. A value of @true@ specifies
-- that Amazon SES will include headers in delivery notifications, and a
-- value of @false@ specifies that Amazon SES will not include headers in
-- delivery notifications.
identityNotificationAttributes_headersInDeliveryNotificationsEnabled :: Lens.Lens' IdentityNotificationAttributes (Prelude.Maybe Prelude.Bool)
identityNotificationAttributes_headersInDeliveryNotificationsEnabled = Lens.lens (\IdentityNotificationAttributes' {headersInDeliveryNotificationsEnabled} -> headersInDeliveryNotificationsEnabled) (\s@IdentityNotificationAttributes' {} a -> s {headersInDeliveryNotificationsEnabled = a} :: IdentityNotificationAttributes)

-- | The Amazon Resource Name (ARN) of the Amazon SNS topic where Amazon SES
-- will publish bounce notifications.
identityNotificationAttributes_bounceTopic :: Lens.Lens' IdentityNotificationAttributes Prelude.Text
identityNotificationAttributes_bounceTopic = Lens.lens (\IdentityNotificationAttributes' {bounceTopic} -> bounceTopic) (\s@IdentityNotificationAttributes' {} a -> s {bounceTopic = a} :: IdentityNotificationAttributes)

-- | The Amazon Resource Name (ARN) of the Amazon SNS topic where Amazon SES
-- will publish complaint notifications.
identityNotificationAttributes_complaintTopic :: Lens.Lens' IdentityNotificationAttributes Prelude.Text
identityNotificationAttributes_complaintTopic = Lens.lens (\IdentityNotificationAttributes' {complaintTopic} -> complaintTopic) (\s@IdentityNotificationAttributes' {} a -> s {complaintTopic = a} :: IdentityNotificationAttributes)

-- | The Amazon Resource Name (ARN) of the Amazon SNS topic where Amazon SES
-- will publish delivery notifications.
identityNotificationAttributes_deliveryTopic :: Lens.Lens' IdentityNotificationAttributes Prelude.Text
identityNotificationAttributes_deliveryTopic = Lens.lens (\IdentityNotificationAttributes' {deliveryTopic} -> deliveryTopic) (\s@IdentityNotificationAttributes' {} a -> s {deliveryTopic = a} :: IdentityNotificationAttributes)

-- | Describes whether Amazon SES will forward bounce and complaint
-- notifications as email. @true@ indicates that Amazon SES will forward
-- bounce and complaint notifications as email, while @false@ indicates
-- that bounce and complaint notifications will be published only to the
-- specified bounce and complaint Amazon SNS topics.
identityNotificationAttributes_forwardingEnabled :: Lens.Lens' IdentityNotificationAttributes Prelude.Bool
identityNotificationAttributes_forwardingEnabled = Lens.lens (\IdentityNotificationAttributes' {forwardingEnabled} -> forwardingEnabled) (\s@IdentityNotificationAttributes' {} a -> s {forwardingEnabled = a} :: IdentityNotificationAttributes)

instance Data.FromXML IdentityNotificationAttributes where
  parseXML x =
    IdentityNotificationAttributes'
      Prelude.<$> (x Data..@? "HeadersInBounceNotificationsEnabled")
      Prelude.<*> (x Data..@? "HeadersInComplaintNotificationsEnabled")
      Prelude.<*> (x Data..@? "HeadersInDeliveryNotificationsEnabled")
      Prelude.<*> (x Data..@ "BounceTopic")
      Prelude.<*> (x Data..@ "ComplaintTopic")
      Prelude.<*> (x Data..@ "DeliveryTopic")
      Prelude.<*> (x Data..@ "ForwardingEnabled")

instance
  Prelude.Hashable
    IdentityNotificationAttributes
  where
  hashWithSalt
    _salt
    IdentityNotificationAttributes' {..} =
      _salt
        `Prelude.hashWithSalt` headersInBounceNotificationsEnabled
        `Prelude.hashWithSalt` headersInComplaintNotificationsEnabled
        `Prelude.hashWithSalt` headersInDeliveryNotificationsEnabled
        `Prelude.hashWithSalt` bounceTopic
        `Prelude.hashWithSalt` complaintTopic
        `Prelude.hashWithSalt` deliveryTopic
        `Prelude.hashWithSalt` forwardingEnabled

instance
  Prelude.NFData
    IdentityNotificationAttributes
  where
  rnf IdentityNotificationAttributes' {..} =
    Prelude.rnf headersInBounceNotificationsEnabled
      `Prelude.seq` Prelude.rnf headersInComplaintNotificationsEnabled
      `Prelude.seq` Prelude.rnf headersInDeliveryNotificationsEnabled
      `Prelude.seq` Prelude.rnf bounceTopic
      `Prelude.seq` Prelude.rnf complaintTopic
      `Prelude.seq` Prelude.rnf deliveryTopic
      `Prelude.seq` Prelude.rnf forwardingEnabled
