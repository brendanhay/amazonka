-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types.BouncedRecipientInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SES.Types.BouncedRecipientInfo
  ( BouncedRecipientInfo (..),

    -- * Smart constructor
    mkBouncedRecipientInfo,

    -- * Lenses
    briBounceType,
    briRecipientDsnFields,
    briRecipientARN,
    briRecipient,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SES.Types.BounceType
import Network.AWS.SES.Types.RecipientDsnFields

-- | Recipient-related information to include in the Delivery Status Notification (DSN) when an email that Amazon SES receives on your behalf bounces.
--
-- For information about receiving email through Amazon SES, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email.html Amazon SES Developer Guide> .
--
-- /See:/ 'mkBouncedRecipientInfo' smart constructor.
data BouncedRecipientInfo = BouncedRecipientInfo'
  { bounceType ::
      Lude.Maybe BounceType,
    recipientDsnFields ::
      Lude.Maybe RecipientDsnFields,
    recipientARN :: Lude.Maybe Lude.Text,
    recipient :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BouncedRecipientInfo' with the minimum fields required to make a request.
--
-- * 'bounceType' - The reason for the bounce. You must provide either this parameter or @RecipientDsnFields@ .
-- * 'recipient' - The email address of the recipient of the bounced email.
-- * 'recipientARN' - This parameter is used only for sending authorization. It is the ARN of the identity that is associated with the sending authorization policy that permits you to receive email for the recipient of the bounced email. For more information about sending authorization, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html Amazon SES Developer Guide> .
-- * 'recipientDsnFields' - Recipient-related DSN fields, most of which would normally be filled in automatically when provided with a @BounceType@ . You must provide either this parameter or @BounceType@ .
mkBouncedRecipientInfo ::
  -- | 'recipient'
  Lude.Text ->
  BouncedRecipientInfo
mkBouncedRecipientInfo pRecipient_ =
  BouncedRecipientInfo'
    { bounceType = Lude.Nothing,
      recipientDsnFields = Lude.Nothing,
      recipientARN = Lude.Nothing,
      recipient = pRecipient_
    }

-- | The reason for the bounce. You must provide either this parameter or @RecipientDsnFields@ .
--
-- /Note:/ Consider using 'bounceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
briBounceType :: Lens.Lens' BouncedRecipientInfo (Lude.Maybe BounceType)
briBounceType = Lens.lens (bounceType :: BouncedRecipientInfo -> Lude.Maybe BounceType) (\s a -> s {bounceType = a} :: BouncedRecipientInfo)
{-# DEPRECATED briBounceType "Use generic-lens or generic-optics with 'bounceType' instead." #-}

-- | Recipient-related DSN fields, most of which would normally be filled in automatically when provided with a @BounceType@ . You must provide either this parameter or @BounceType@ .
--
-- /Note:/ Consider using 'recipientDsnFields' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
briRecipientDsnFields :: Lens.Lens' BouncedRecipientInfo (Lude.Maybe RecipientDsnFields)
briRecipientDsnFields = Lens.lens (recipientDsnFields :: BouncedRecipientInfo -> Lude.Maybe RecipientDsnFields) (\s a -> s {recipientDsnFields = a} :: BouncedRecipientInfo)
{-# DEPRECATED briRecipientDsnFields "Use generic-lens or generic-optics with 'recipientDsnFields' instead." #-}

-- | This parameter is used only for sending authorization. It is the ARN of the identity that is associated with the sending authorization policy that permits you to receive email for the recipient of the bounced email. For more information about sending authorization, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html Amazon SES Developer Guide> .
--
-- /Note:/ Consider using 'recipientARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
briRecipientARN :: Lens.Lens' BouncedRecipientInfo (Lude.Maybe Lude.Text)
briRecipientARN = Lens.lens (recipientARN :: BouncedRecipientInfo -> Lude.Maybe Lude.Text) (\s a -> s {recipientARN = a} :: BouncedRecipientInfo)
{-# DEPRECATED briRecipientARN "Use generic-lens or generic-optics with 'recipientARN' instead." #-}

-- | The email address of the recipient of the bounced email.
--
-- /Note:/ Consider using 'recipient' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
briRecipient :: Lens.Lens' BouncedRecipientInfo Lude.Text
briRecipient = Lens.lens (recipient :: BouncedRecipientInfo -> Lude.Text) (\s a -> s {recipient = a} :: BouncedRecipientInfo)
{-# DEPRECATED briRecipient "Use generic-lens or generic-optics with 'recipient' instead." #-}

instance Lude.ToQuery BouncedRecipientInfo where
  toQuery BouncedRecipientInfo' {..} =
    Lude.mconcat
      [ "BounceType" Lude.=: bounceType,
        "RecipientDsnFields" Lude.=: recipientDsnFields,
        "RecipientArn" Lude.=: recipientARN,
        "Recipient" Lude.=: recipient
      ]
