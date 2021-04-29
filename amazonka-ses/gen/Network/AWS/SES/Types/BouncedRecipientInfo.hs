{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.SES.Types.BouncedRecipientInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SES.Types.BouncedRecipientInfo where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SES.Types.BounceType
import Network.AWS.SES.Types.RecipientDsnFields

-- | Recipient-related information to include in the Delivery Status
-- Notification (DSN) when an email that Amazon SES receives on your behalf
-- bounces.
--
-- For information about receiving email through Amazon SES, see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email.html Amazon SES Developer Guide>.
--
-- /See:/ 'newBouncedRecipientInfo' smart constructor.
data BouncedRecipientInfo = BouncedRecipientInfo'
  { -- | This parameter is used only for sending authorization. It is the ARN of
    -- the identity that is associated with the sending authorization policy
    -- that permits you to receive email for the recipient of the bounced
    -- email. For more information about sending authorization, see the
    -- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html Amazon SES Developer Guide>.
    recipientArn :: Prelude.Maybe Prelude.Text,
    -- | Recipient-related DSN fields, most of which would normally be filled in
    -- automatically when provided with a @BounceType@. You must provide either
    -- this parameter or @BounceType@.
    recipientDsnFields :: Prelude.Maybe RecipientDsnFields,
    -- | The reason for the bounce. You must provide either this parameter or
    -- @RecipientDsnFields@.
    bounceType :: Prelude.Maybe BounceType,
    -- | The email address of the recipient of the bounced email.
    recipient :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'BouncedRecipientInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'recipientArn', 'bouncedRecipientInfo_recipientArn' - This parameter is used only for sending authorization. It is the ARN of
-- the identity that is associated with the sending authorization policy
-- that permits you to receive email for the recipient of the bounced
-- email. For more information about sending authorization, see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html Amazon SES Developer Guide>.
--
-- 'recipientDsnFields', 'bouncedRecipientInfo_recipientDsnFields' - Recipient-related DSN fields, most of which would normally be filled in
-- automatically when provided with a @BounceType@. You must provide either
-- this parameter or @BounceType@.
--
-- 'bounceType', 'bouncedRecipientInfo_bounceType' - The reason for the bounce. You must provide either this parameter or
-- @RecipientDsnFields@.
--
-- 'recipient', 'bouncedRecipientInfo_recipient' - The email address of the recipient of the bounced email.
newBouncedRecipientInfo ::
  -- | 'recipient'
  Prelude.Text ->
  BouncedRecipientInfo
newBouncedRecipientInfo pRecipient_ =
  BouncedRecipientInfo'
    { recipientArn =
        Prelude.Nothing,
      recipientDsnFields = Prelude.Nothing,
      bounceType = Prelude.Nothing,
      recipient = pRecipient_
    }

-- | This parameter is used only for sending authorization. It is the ARN of
-- the identity that is associated with the sending authorization policy
-- that permits you to receive email for the recipient of the bounced
-- email. For more information about sending authorization, see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html Amazon SES Developer Guide>.
bouncedRecipientInfo_recipientArn :: Lens.Lens' BouncedRecipientInfo (Prelude.Maybe Prelude.Text)
bouncedRecipientInfo_recipientArn = Lens.lens (\BouncedRecipientInfo' {recipientArn} -> recipientArn) (\s@BouncedRecipientInfo' {} a -> s {recipientArn = a} :: BouncedRecipientInfo)

-- | Recipient-related DSN fields, most of which would normally be filled in
-- automatically when provided with a @BounceType@. You must provide either
-- this parameter or @BounceType@.
bouncedRecipientInfo_recipientDsnFields :: Lens.Lens' BouncedRecipientInfo (Prelude.Maybe RecipientDsnFields)
bouncedRecipientInfo_recipientDsnFields = Lens.lens (\BouncedRecipientInfo' {recipientDsnFields} -> recipientDsnFields) (\s@BouncedRecipientInfo' {} a -> s {recipientDsnFields = a} :: BouncedRecipientInfo)

-- | The reason for the bounce. You must provide either this parameter or
-- @RecipientDsnFields@.
bouncedRecipientInfo_bounceType :: Lens.Lens' BouncedRecipientInfo (Prelude.Maybe BounceType)
bouncedRecipientInfo_bounceType = Lens.lens (\BouncedRecipientInfo' {bounceType} -> bounceType) (\s@BouncedRecipientInfo' {} a -> s {bounceType = a} :: BouncedRecipientInfo)

-- | The email address of the recipient of the bounced email.
bouncedRecipientInfo_recipient :: Lens.Lens' BouncedRecipientInfo Prelude.Text
bouncedRecipientInfo_recipient = Lens.lens (\BouncedRecipientInfo' {recipient} -> recipient) (\s@BouncedRecipientInfo' {} a -> s {recipient = a} :: BouncedRecipientInfo)

instance Prelude.Hashable BouncedRecipientInfo

instance Prelude.NFData BouncedRecipientInfo

instance Prelude.ToQuery BouncedRecipientInfo where
  toQuery BouncedRecipientInfo' {..} =
    Prelude.mconcat
      [ "RecipientArn" Prelude.=: recipientArn,
        "RecipientDsnFields" Prelude.=: recipientDsnFields,
        "BounceType" Prelude.=: bounceType,
        "Recipient" Prelude.=: recipient
      ]
