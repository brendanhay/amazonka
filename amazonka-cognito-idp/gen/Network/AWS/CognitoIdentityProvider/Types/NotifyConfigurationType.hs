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
-- Module      : Network.AWS.CognitoIdentityProvider.Types.NotifyConfigurationType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.NotifyConfigurationType where

import Network.AWS.CognitoIdentityProvider.Types.NotifyEmailType
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The notify configuration type.
--
-- /See:/ 'newNotifyConfigurationType' smart constructor.
data NotifyConfigurationType = NotifyConfigurationType'
  { -- | The MFA email template used when MFA is challenged as part of a detected
    -- risk.
    mfaEmail :: Core.Maybe NotifyEmailType,
    -- | Email template used when a detected risk event is blocked.
    blockEmail :: Core.Maybe NotifyEmailType,
    -- | The destination to which the receiver of an email should reply to.
    replyTo :: Core.Maybe Core.Text,
    -- | The email address that is sending the email. It must be either
    -- individually verified with Amazon SES, or from a domain that has been
    -- verified with Amazon SES.
    from :: Core.Maybe Core.Text,
    -- | The email template used when a detected risk event is allowed.
    noActionEmail :: Core.Maybe NotifyEmailType,
    -- | The Amazon Resource Name (ARN) of the identity that is associated with
    -- the sending authorization policy. It permits Amazon Cognito to send for
    -- the email address specified in the @From@ parameter.
    sourceArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'NotifyConfigurationType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'mfaEmail', 'notifyConfigurationType_mfaEmail' - The MFA email template used when MFA is challenged as part of a detected
-- risk.
--
-- 'blockEmail', 'notifyConfigurationType_blockEmail' - Email template used when a detected risk event is blocked.
--
-- 'replyTo', 'notifyConfigurationType_replyTo' - The destination to which the receiver of an email should reply to.
--
-- 'from', 'notifyConfigurationType_from' - The email address that is sending the email. It must be either
-- individually verified with Amazon SES, or from a domain that has been
-- verified with Amazon SES.
--
-- 'noActionEmail', 'notifyConfigurationType_noActionEmail' - The email template used when a detected risk event is allowed.
--
-- 'sourceArn', 'notifyConfigurationType_sourceArn' - The Amazon Resource Name (ARN) of the identity that is associated with
-- the sending authorization policy. It permits Amazon Cognito to send for
-- the email address specified in the @From@ parameter.
newNotifyConfigurationType ::
  -- | 'sourceArn'
  Core.Text ->
  NotifyConfigurationType
newNotifyConfigurationType pSourceArn_ =
  NotifyConfigurationType'
    { mfaEmail = Core.Nothing,
      blockEmail = Core.Nothing,
      replyTo = Core.Nothing,
      from = Core.Nothing,
      noActionEmail = Core.Nothing,
      sourceArn = pSourceArn_
    }

-- | The MFA email template used when MFA is challenged as part of a detected
-- risk.
notifyConfigurationType_mfaEmail :: Lens.Lens' NotifyConfigurationType (Core.Maybe NotifyEmailType)
notifyConfigurationType_mfaEmail = Lens.lens (\NotifyConfigurationType' {mfaEmail} -> mfaEmail) (\s@NotifyConfigurationType' {} a -> s {mfaEmail = a} :: NotifyConfigurationType)

-- | Email template used when a detected risk event is blocked.
notifyConfigurationType_blockEmail :: Lens.Lens' NotifyConfigurationType (Core.Maybe NotifyEmailType)
notifyConfigurationType_blockEmail = Lens.lens (\NotifyConfigurationType' {blockEmail} -> blockEmail) (\s@NotifyConfigurationType' {} a -> s {blockEmail = a} :: NotifyConfigurationType)

-- | The destination to which the receiver of an email should reply to.
notifyConfigurationType_replyTo :: Lens.Lens' NotifyConfigurationType (Core.Maybe Core.Text)
notifyConfigurationType_replyTo = Lens.lens (\NotifyConfigurationType' {replyTo} -> replyTo) (\s@NotifyConfigurationType' {} a -> s {replyTo = a} :: NotifyConfigurationType)

-- | The email address that is sending the email. It must be either
-- individually verified with Amazon SES, or from a domain that has been
-- verified with Amazon SES.
notifyConfigurationType_from :: Lens.Lens' NotifyConfigurationType (Core.Maybe Core.Text)
notifyConfigurationType_from = Lens.lens (\NotifyConfigurationType' {from} -> from) (\s@NotifyConfigurationType' {} a -> s {from = a} :: NotifyConfigurationType)

-- | The email template used when a detected risk event is allowed.
notifyConfigurationType_noActionEmail :: Lens.Lens' NotifyConfigurationType (Core.Maybe NotifyEmailType)
notifyConfigurationType_noActionEmail = Lens.lens (\NotifyConfigurationType' {noActionEmail} -> noActionEmail) (\s@NotifyConfigurationType' {} a -> s {noActionEmail = a} :: NotifyConfigurationType)

-- | The Amazon Resource Name (ARN) of the identity that is associated with
-- the sending authorization policy. It permits Amazon Cognito to send for
-- the email address specified in the @From@ parameter.
notifyConfigurationType_sourceArn :: Lens.Lens' NotifyConfigurationType Core.Text
notifyConfigurationType_sourceArn = Lens.lens (\NotifyConfigurationType' {sourceArn} -> sourceArn) (\s@NotifyConfigurationType' {} a -> s {sourceArn = a} :: NotifyConfigurationType)

instance Core.FromJSON NotifyConfigurationType where
  parseJSON =
    Core.withObject
      "NotifyConfigurationType"
      ( \x ->
          NotifyConfigurationType'
            Core.<$> (x Core..:? "MfaEmail")
            Core.<*> (x Core..:? "BlockEmail")
            Core.<*> (x Core..:? "ReplyTo")
            Core.<*> (x Core..:? "From")
            Core.<*> (x Core..:? "NoActionEmail")
            Core.<*> (x Core..: "SourceArn")
      )

instance Core.Hashable NotifyConfigurationType

instance Core.NFData NotifyConfigurationType

instance Core.ToJSON NotifyConfigurationType where
  toJSON NotifyConfigurationType' {..} =
    Core.object
      ( Core.catMaybes
          [ ("MfaEmail" Core..=) Core.<$> mfaEmail,
            ("BlockEmail" Core..=) Core.<$> blockEmail,
            ("ReplyTo" Core..=) Core.<$> replyTo,
            ("From" Core..=) Core.<$> from,
            ("NoActionEmail" Core..=) Core.<$> noActionEmail,
            Core.Just ("SourceArn" Core..= sourceArn)
          ]
      )
