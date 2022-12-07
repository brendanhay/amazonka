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
-- Module      : Amazonka.CognitoIdentityProvider.Types.NotifyConfigurationType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CognitoIdentityProvider.Types.NotifyConfigurationType where

import Amazonka.CognitoIdentityProvider.Types.NotifyEmailType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The notify configuration type.
--
-- /See:/ 'newNotifyConfigurationType' smart constructor.
data NotifyConfigurationType = NotifyConfigurationType'
  { -- | The email address that is sending the email. The address must be either
    -- individually verified with Amazon Simple Email Service, or from a domain
    -- that has been verified with Amazon SES.
    from :: Prelude.Maybe Prelude.Text,
    -- | The multi-factor authentication (MFA) email template used when MFA is
    -- challenged as part of a detected risk.
    mfaEmail :: Prelude.Maybe NotifyEmailType,
    -- | Email template used when a detected risk event is blocked.
    blockEmail :: Prelude.Maybe NotifyEmailType,
    -- | The email template used when a detected risk event is allowed.
    noActionEmail :: Prelude.Maybe NotifyEmailType,
    -- | The destination to which the receiver of an email should reply to.
    replyTo :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the identity that is associated with
    -- the sending authorization policy. This identity permits Amazon Cognito
    -- to send for the email address specified in the @From@ parameter.
    sourceArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NotifyConfigurationType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'from', 'notifyConfigurationType_from' - The email address that is sending the email. The address must be either
-- individually verified with Amazon Simple Email Service, or from a domain
-- that has been verified with Amazon SES.
--
-- 'mfaEmail', 'notifyConfigurationType_mfaEmail' - The multi-factor authentication (MFA) email template used when MFA is
-- challenged as part of a detected risk.
--
-- 'blockEmail', 'notifyConfigurationType_blockEmail' - Email template used when a detected risk event is blocked.
--
-- 'noActionEmail', 'notifyConfigurationType_noActionEmail' - The email template used when a detected risk event is allowed.
--
-- 'replyTo', 'notifyConfigurationType_replyTo' - The destination to which the receiver of an email should reply to.
--
-- 'sourceArn', 'notifyConfigurationType_sourceArn' - The Amazon Resource Name (ARN) of the identity that is associated with
-- the sending authorization policy. This identity permits Amazon Cognito
-- to send for the email address specified in the @From@ parameter.
newNotifyConfigurationType ::
  -- | 'sourceArn'
  Prelude.Text ->
  NotifyConfigurationType
newNotifyConfigurationType pSourceArn_ =
  NotifyConfigurationType'
    { from = Prelude.Nothing,
      mfaEmail = Prelude.Nothing,
      blockEmail = Prelude.Nothing,
      noActionEmail = Prelude.Nothing,
      replyTo = Prelude.Nothing,
      sourceArn = pSourceArn_
    }

-- | The email address that is sending the email. The address must be either
-- individually verified with Amazon Simple Email Service, or from a domain
-- that has been verified with Amazon SES.
notifyConfigurationType_from :: Lens.Lens' NotifyConfigurationType (Prelude.Maybe Prelude.Text)
notifyConfigurationType_from = Lens.lens (\NotifyConfigurationType' {from} -> from) (\s@NotifyConfigurationType' {} a -> s {from = a} :: NotifyConfigurationType)

-- | The multi-factor authentication (MFA) email template used when MFA is
-- challenged as part of a detected risk.
notifyConfigurationType_mfaEmail :: Lens.Lens' NotifyConfigurationType (Prelude.Maybe NotifyEmailType)
notifyConfigurationType_mfaEmail = Lens.lens (\NotifyConfigurationType' {mfaEmail} -> mfaEmail) (\s@NotifyConfigurationType' {} a -> s {mfaEmail = a} :: NotifyConfigurationType)

-- | Email template used when a detected risk event is blocked.
notifyConfigurationType_blockEmail :: Lens.Lens' NotifyConfigurationType (Prelude.Maybe NotifyEmailType)
notifyConfigurationType_blockEmail = Lens.lens (\NotifyConfigurationType' {blockEmail} -> blockEmail) (\s@NotifyConfigurationType' {} a -> s {blockEmail = a} :: NotifyConfigurationType)

-- | The email template used when a detected risk event is allowed.
notifyConfigurationType_noActionEmail :: Lens.Lens' NotifyConfigurationType (Prelude.Maybe NotifyEmailType)
notifyConfigurationType_noActionEmail = Lens.lens (\NotifyConfigurationType' {noActionEmail} -> noActionEmail) (\s@NotifyConfigurationType' {} a -> s {noActionEmail = a} :: NotifyConfigurationType)

-- | The destination to which the receiver of an email should reply to.
notifyConfigurationType_replyTo :: Lens.Lens' NotifyConfigurationType (Prelude.Maybe Prelude.Text)
notifyConfigurationType_replyTo = Lens.lens (\NotifyConfigurationType' {replyTo} -> replyTo) (\s@NotifyConfigurationType' {} a -> s {replyTo = a} :: NotifyConfigurationType)

-- | The Amazon Resource Name (ARN) of the identity that is associated with
-- the sending authorization policy. This identity permits Amazon Cognito
-- to send for the email address specified in the @From@ parameter.
notifyConfigurationType_sourceArn :: Lens.Lens' NotifyConfigurationType Prelude.Text
notifyConfigurationType_sourceArn = Lens.lens (\NotifyConfigurationType' {sourceArn} -> sourceArn) (\s@NotifyConfigurationType' {} a -> s {sourceArn = a} :: NotifyConfigurationType)

instance Data.FromJSON NotifyConfigurationType where
  parseJSON =
    Data.withObject
      "NotifyConfigurationType"
      ( \x ->
          NotifyConfigurationType'
            Prelude.<$> (x Data..:? "From")
            Prelude.<*> (x Data..:? "MfaEmail")
            Prelude.<*> (x Data..:? "BlockEmail")
            Prelude.<*> (x Data..:? "NoActionEmail")
            Prelude.<*> (x Data..:? "ReplyTo")
            Prelude.<*> (x Data..: "SourceArn")
      )

instance Prelude.Hashable NotifyConfigurationType where
  hashWithSalt _salt NotifyConfigurationType' {..} =
    _salt `Prelude.hashWithSalt` from
      `Prelude.hashWithSalt` mfaEmail
      `Prelude.hashWithSalt` blockEmail
      `Prelude.hashWithSalt` noActionEmail
      `Prelude.hashWithSalt` replyTo
      `Prelude.hashWithSalt` sourceArn

instance Prelude.NFData NotifyConfigurationType where
  rnf NotifyConfigurationType' {..} =
    Prelude.rnf from
      `Prelude.seq` Prelude.rnf mfaEmail
      `Prelude.seq` Prelude.rnf blockEmail
      `Prelude.seq` Prelude.rnf noActionEmail
      `Prelude.seq` Prelude.rnf replyTo
      `Prelude.seq` Prelude.rnf sourceArn

instance Data.ToJSON NotifyConfigurationType where
  toJSON NotifyConfigurationType' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("From" Data..=) Prelude.<$> from,
            ("MfaEmail" Data..=) Prelude.<$> mfaEmail,
            ("BlockEmail" Data..=) Prelude.<$> blockEmail,
            ("NoActionEmail" Data..=) Prelude.<$> noActionEmail,
            ("ReplyTo" Data..=) Prelude.<$> replyTo,
            Prelude.Just ("SourceArn" Data..= sourceArn)
          ]
      )
