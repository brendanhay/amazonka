{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    nctSourceArn,
    nctBlockEmail,
    nctFrom,
    nctMfaEmail,
    nctNoActionEmail,
    nctReplyTo,
  )
where

import qualified Network.AWS.CognitoIdentityProvider.Types.From as Types
import qualified Network.AWS.CognitoIdentityProvider.Types.NotifyEmailType as Types
import qualified Network.AWS.CognitoIdentityProvider.Types.ReplyTo as Types
import qualified Network.AWS.CognitoIdentityProvider.Types.SourceArn as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The notify configuration type.
--
-- /See:/ 'mkNotifyConfigurationType' smart constructor.
data NotifyConfigurationType = NotifyConfigurationType'
  { -- | The Amazon Resource Name (ARN) of the identity that is associated with the sending authorization policy. It permits Amazon Cognito to send for the email address specified in the @From@ parameter.
    sourceArn :: Types.SourceArn,
    -- | Email template used when a detected risk event is blocked.
    blockEmail :: Core.Maybe Types.NotifyEmailType,
    -- | The email address that is sending the email. It must be either individually verified with Amazon SES, or from a domain that has been verified with Amazon SES.
    from :: Core.Maybe Types.From,
    -- | The MFA email template used when MFA is challenged as part of a detected risk.
    mfaEmail :: Core.Maybe Types.NotifyEmailType,
    -- | The email template used when a detected risk event is allowed.
    noActionEmail :: Core.Maybe Types.NotifyEmailType,
    -- | The destination to which the receiver of an email should reply to.
    replyTo :: Core.Maybe Types.ReplyTo
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'NotifyConfigurationType' value with any optional fields omitted.
mkNotifyConfigurationType ::
  -- | 'sourceArn'
  Types.SourceArn ->
  NotifyConfigurationType
mkNotifyConfigurationType sourceArn =
  NotifyConfigurationType'
    { sourceArn,
      blockEmail = Core.Nothing,
      from = Core.Nothing,
      mfaEmail = Core.Nothing,
      noActionEmail = Core.Nothing,
      replyTo = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the identity that is associated with the sending authorization policy. It permits Amazon Cognito to send for the email address specified in the @From@ parameter.
--
-- /Note:/ Consider using 'sourceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nctSourceArn :: Lens.Lens' NotifyConfigurationType Types.SourceArn
nctSourceArn = Lens.field @"sourceArn"
{-# DEPRECATED nctSourceArn "Use generic-lens or generic-optics with 'sourceArn' instead." #-}

-- | Email template used when a detected risk event is blocked.
--
-- /Note:/ Consider using 'blockEmail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nctBlockEmail :: Lens.Lens' NotifyConfigurationType (Core.Maybe Types.NotifyEmailType)
nctBlockEmail = Lens.field @"blockEmail"
{-# DEPRECATED nctBlockEmail "Use generic-lens or generic-optics with 'blockEmail' instead." #-}

-- | The email address that is sending the email. It must be either individually verified with Amazon SES, or from a domain that has been verified with Amazon SES.
--
-- /Note:/ Consider using 'from' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nctFrom :: Lens.Lens' NotifyConfigurationType (Core.Maybe Types.From)
nctFrom = Lens.field @"from"
{-# DEPRECATED nctFrom "Use generic-lens or generic-optics with 'from' instead." #-}

-- | The MFA email template used when MFA is challenged as part of a detected risk.
--
-- /Note:/ Consider using 'mfaEmail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nctMfaEmail :: Lens.Lens' NotifyConfigurationType (Core.Maybe Types.NotifyEmailType)
nctMfaEmail = Lens.field @"mfaEmail"
{-# DEPRECATED nctMfaEmail "Use generic-lens or generic-optics with 'mfaEmail' instead." #-}

-- | The email template used when a detected risk event is allowed.
--
-- /Note:/ Consider using 'noActionEmail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nctNoActionEmail :: Lens.Lens' NotifyConfigurationType (Core.Maybe Types.NotifyEmailType)
nctNoActionEmail = Lens.field @"noActionEmail"
{-# DEPRECATED nctNoActionEmail "Use generic-lens or generic-optics with 'noActionEmail' instead." #-}

-- | The destination to which the receiver of an email should reply to.
--
-- /Note:/ Consider using 'replyTo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nctReplyTo :: Lens.Lens' NotifyConfigurationType (Core.Maybe Types.ReplyTo)
nctReplyTo = Lens.field @"replyTo"
{-# DEPRECATED nctReplyTo "Use generic-lens or generic-optics with 'replyTo' instead." #-}

instance Core.FromJSON NotifyConfigurationType where
  toJSON NotifyConfigurationType {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("SourceArn" Core..= sourceArn),
            ("BlockEmail" Core..=) Core.<$> blockEmail,
            ("From" Core..=) Core.<$> from,
            ("MfaEmail" Core..=) Core.<$> mfaEmail,
            ("NoActionEmail" Core..=) Core.<$> noActionEmail,
            ("ReplyTo" Core..=) Core.<$> replyTo
          ]
      )

instance Core.FromJSON NotifyConfigurationType where
  parseJSON =
    Core.withObject "NotifyConfigurationType" Core.$
      \x ->
        NotifyConfigurationType'
          Core.<$> (x Core..: "SourceArn")
          Core.<*> (x Core..:? "BlockEmail")
          Core.<*> (x Core..:? "From")
          Core.<*> (x Core..:? "MfaEmail")
          Core.<*> (x Core..:? "NoActionEmail")
          Core.<*> (x Core..:? "ReplyTo")
