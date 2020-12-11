-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.NotifyConfigurationType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.NotifyConfigurationType
  ( NotifyConfigurationType (..),

    -- * Smart constructor
    mkNotifyConfigurationType,

    -- * Lenses
    nctNoActionEmail,
    nctFrom,
    nctReplyTo,
    nctBlockEmail,
    nctMFAEmail,
    nctSourceARN,
  )
where

import Network.AWS.CognitoIdentityProvider.Types.NotifyEmailType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The notify configuration type.
--
-- /See:/ 'mkNotifyConfigurationType' smart constructor.
data NotifyConfigurationType = NotifyConfigurationType'
  { noActionEmail ::
      Lude.Maybe NotifyEmailType,
    from :: Lude.Maybe Lude.Text,
    replyTo :: Lude.Maybe Lude.Text,
    blockEmail :: Lude.Maybe NotifyEmailType,
    mfaEmail :: Lude.Maybe NotifyEmailType,
    sourceARN :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'NotifyConfigurationType' with the minimum fields required to make a request.
--
-- * 'blockEmail' - Email template used when a detected risk event is blocked.
-- * 'from' - The email address that is sending the email. It must be either individually verified with Amazon SES, or from a domain that has been verified with Amazon SES.
-- * 'mfaEmail' - The MFA email template used when MFA is challenged as part of a detected risk.
-- * 'noActionEmail' - The email template used when a detected risk event is allowed.
-- * 'replyTo' - The destination to which the receiver of an email should reply to.
-- * 'sourceARN' - The Amazon Resource Name (ARN) of the identity that is associated with the sending authorization policy. It permits Amazon Cognito to send for the email address specified in the @From@ parameter.
mkNotifyConfigurationType ::
  -- | 'sourceARN'
  Lude.Text ->
  NotifyConfigurationType
mkNotifyConfigurationType pSourceARN_ =
  NotifyConfigurationType'
    { noActionEmail = Lude.Nothing,
      from = Lude.Nothing,
      replyTo = Lude.Nothing,
      blockEmail = Lude.Nothing,
      mfaEmail = Lude.Nothing,
      sourceARN = pSourceARN_
    }

-- | The email template used when a detected risk event is allowed.
--
-- /Note:/ Consider using 'noActionEmail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nctNoActionEmail :: Lens.Lens' NotifyConfigurationType (Lude.Maybe NotifyEmailType)
nctNoActionEmail = Lens.lens (noActionEmail :: NotifyConfigurationType -> Lude.Maybe NotifyEmailType) (\s a -> s {noActionEmail = a} :: NotifyConfigurationType)
{-# DEPRECATED nctNoActionEmail "Use generic-lens or generic-optics with 'noActionEmail' instead." #-}

-- | The email address that is sending the email. It must be either individually verified with Amazon SES, or from a domain that has been verified with Amazon SES.
--
-- /Note:/ Consider using 'from' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nctFrom :: Lens.Lens' NotifyConfigurationType (Lude.Maybe Lude.Text)
nctFrom = Lens.lens (from :: NotifyConfigurationType -> Lude.Maybe Lude.Text) (\s a -> s {from = a} :: NotifyConfigurationType)
{-# DEPRECATED nctFrom "Use generic-lens or generic-optics with 'from' instead." #-}

-- | The destination to which the receiver of an email should reply to.
--
-- /Note:/ Consider using 'replyTo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nctReplyTo :: Lens.Lens' NotifyConfigurationType (Lude.Maybe Lude.Text)
nctReplyTo = Lens.lens (replyTo :: NotifyConfigurationType -> Lude.Maybe Lude.Text) (\s a -> s {replyTo = a} :: NotifyConfigurationType)
{-# DEPRECATED nctReplyTo "Use generic-lens or generic-optics with 'replyTo' instead." #-}

-- | Email template used when a detected risk event is blocked.
--
-- /Note:/ Consider using 'blockEmail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nctBlockEmail :: Lens.Lens' NotifyConfigurationType (Lude.Maybe NotifyEmailType)
nctBlockEmail = Lens.lens (blockEmail :: NotifyConfigurationType -> Lude.Maybe NotifyEmailType) (\s a -> s {blockEmail = a} :: NotifyConfigurationType)
{-# DEPRECATED nctBlockEmail "Use generic-lens or generic-optics with 'blockEmail' instead." #-}

-- | The MFA email template used when MFA is challenged as part of a detected risk.
--
-- /Note:/ Consider using 'mfaEmail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nctMFAEmail :: Lens.Lens' NotifyConfigurationType (Lude.Maybe NotifyEmailType)
nctMFAEmail = Lens.lens (mfaEmail :: NotifyConfigurationType -> Lude.Maybe NotifyEmailType) (\s a -> s {mfaEmail = a} :: NotifyConfigurationType)
{-# DEPRECATED nctMFAEmail "Use generic-lens or generic-optics with 'mfaEmail' instead." #-}

-- | The Amazon Resource Name (ARN) of the identity that is associated with the sending authorization policy. It permits Amazon Cognito to send for the email address specified in the @From@ parameter.
--
-- /Note:/ Consider using 'sourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nctSourceARN :: Lens.Lens' NotifyConfigurationType Lude.Text
nctSourceARN = Lens.lens (sourceARN :: NotifyConfigurationType -> Lude.Text) (\s a -> s {sourceARN = a} :: NotifyConfigurationType)
{-# DEPRECATED nctSourceARN "Use generic-lens or generic-optics with 'sourceARN' instead." #-}

instance Lude.FromJSON NotifyConfigurationType where
  parseJSON =
    Lude.withObject
      "NotifyConfigurationType"
      ( \x ->
          NotifyConfigurationType'
            Lude.<$> (x Lude..:? "NoActionEmail")
            Lude.<*> (x Lude..:? "From")
            Lude.<*> (x Lude..:? "ReplyTo")
            Lude.<*> (x Lude..:? "BlockEmail")
            Lude.<*> (x Lude..:? "MfaEmail")
            Lude.<*> (x Lude..: "SourceArn")
      )

instance Lude.ToJSON NotifyConfigurationType where
  toJSON NotifyConfigurationType' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NoActionEmail" Lude..=) Lude.<$> noActionEmail,
            ("From" Lude..=) Lude.<$> from,
            ("ReplyTo" Lude..=) Lude.<$> replyTo,
            ("BlockEmail" Lude..=) Lude.<$> blockEmail,
            ("MfaEmail" Lude..=) Lude.<$> mfaEmail,
            Lude.Just ("SourceArn" Lude..= sourceARN)
          ]
      )
