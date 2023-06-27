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
-- Module      : Amazonka.SES.Types.BouncedRecipientInfo
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SES.Types.BouncedRecipientInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SES.Types.BounceType
import Amazonka.SES.Types.RecipientDsnFields

-- | Recipient-related information to include in the Delivery Status
-- Notification (DSN) when an email that Amazon SES receives on your behalf
-- bounces.
--
-- For information about receiving email through Amazon SES, see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email.html Amazon SES Developer Guide>.
--
-- /See:/ 'newBouncedRecipientInfo' smart constructor.
data BouncedRecipientInfo = BouncedRecipientInfo'
  { -- | The reason for the bounce. You must provide either this parameter or
    -- @RecipientDsnFields@.
    bounceType :: Prelude.Maybe BounceType,
    -- | This parameter is used only for sending authorization. It is the ARN of
    -- the identity that is associated with the sending authorization policy
    -- that permits you to receive email for the recipient of the bounced
    -- email. For more information about sending authorization, see the
    -- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html Amazon SES Developer Guide>.
    recipientArn :: Prelude.Maybe Prelude.Text,
    -- | Recipient-related DSN fields, most of which would normally be filled in
    -- automatically when provided with a @BounceType@. You must provide either
    -- this parameter or @BounceType@.
    recipientDsnFields :: Prelude.Maybe RecipientDsnFields,
    -- | The email address of the recipient of the bounced email.
    recipient :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BouncedRecipientInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bounceType', 'bouncedRecipientInfo_bounceType' - The reason for the bounce. You must provide either this parameter or
-- @RecipientDsnFields@.
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
-- 'recipient', 'bouncedRecipientInfo_recipient' - The email address of the recipient of the bounced email.
newBouncedRecipientInfo ::
  -- | 'recipient'
  Prelude.Text ->
  BouncedRecipientInfo
newBouncedRecipientInfo pRecipient_ =
  BouncedRecipientInfo'
    { bounceType = Prelude.Nothing,
      recipientArn = Prelude.Nothing,
      recipientDsnFields = Prelude.Nothing,
      recipient = pRecipient_
    }

-- | The reason for the bounce. You must provide either this parameter or
-- @RecipientDsnFields@.
bouncedRecipientInfo_bounceType :: Lens.Lens' BouncedRecipientInfo (Prelude.Maybe BounceType)
bouncedRecipientInfo_bounceType = Lens.lens (\BouncedRecipientInfo' {bounceType} -> bounceType) (\s@BouncedRecipientInfo' {} a -> s {bounceType = a} :: BouncedRecipientInfo)

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

-- | The email address of the recipient of the bounced email.
bouncedRecipientInfo_recipient :: Lens.Lens' BouncedRecipientInfo Prelude.Text
bouncedRecipientInfo_recipient = Lens.lens (\BouncedRecipientInfo' {recipient} -> recipient) (\s@BouncedRecipientInfo' {} a -> s {recipient = a} :: BouncedRecipientInfo)

instance Prelude.Hashable BouncedRecipientInfo where
  hashWithSalt _salt BouncedRecipientInfo' {..} =
    _salt
      `Prelude.hashWithSalt` bounceType
      `Prelude.hashWithSalt` recipientArn
      `Prelude.hashWithSalt` recipientDsnFields
      `Prelude.hashWithSalt` recipient

instance Prelude.NFData BouncedRecipientInfo where
  rnf BouncedRecipientInfo' {..} =
    Prelude.rnf bounceType
      `Prelude.seq` Prelude.rnf recipientArn
      `Prelude.seq` Prelude.rnf recipientDsnFields
      `Prelude.seq` Prelude.rnf recipient

instance Data.ToQuery BouncedRecipientInfo where
  toQuery BouncedRecipientInfo' {..} =
    Prelude.mconcat
      [ "BounceType" Data.=: bounceType,
        "RecipientArn" Data.=: recipientArn,
        "RecipientDsnFields" Data.=: recipientDsnFields,
        "Recipient" Data.=: recipient
      ]
