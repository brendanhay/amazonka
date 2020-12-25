{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    briRecipient,
    briBounceType,
    briRecipientArn,
    briRecipientDsnFields,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SES.Types.Address as Types
import qualified Network.AWS.SES.Types.BounceType as Types
import qualified Network.AWS.SES.Types.RecipientArn as Types
import qualified Network.AWS.SES.Types.RecipientDsnFields as Types

-- | Recipient-related information to include in the Delivery Status Notification (DSN) when an email that Amazon SES receives on your behalf bounces.
--
-- For information about receiving email through Amazon SES, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email.html Amazon SES Developer Guide> .
--
-- /See:/ 'mkBouncedRecipientInfo' smart constructor.
data BouncedRecipientInfo = BouncedRecipientInfo'
  { -- | The email address of the recipient of the bounced email.
    recipient :: Types.Address,
    -- | The reason for the bounce. You must provide either this parameter or @RecipientDsnFields@ .
    bounceType :: Core.Maybe Types.BounceType,
    -- | This parameter is used only for sending authorization. It is the ARN of the identity that is associated with the sending authorization policy that permits you to receive email for the recipient of the bounced email. For more information about sending authorization, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html Amazon SES Developer Guide> .
    recipientArn :: Core.Maybe Types.RecipientArn,
    -- | Recipient-related DSN fields, most of which would normally be filled in automatically when provided with a @BounceType@ . You must provide either this parameter or @BounceType@ .
    recipientDsnFields :: Core.Maybe Types.RecipientDsnFields
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'BouncedRecipientInfo' value with any optional fields omitted.
mkBouncedRecipientInfo ::
  -- | 'recipient'
  Types.Address ->
  BouncedRecipientInfo
mkBouncedRecipientInfo recipient =
  BouncedRecipientInfo'
    { recipient,
      bounceType = Core.Nothing,
      recipientArn = Core.Nothing,
      recipientDsnFields = Core.Nothing
    }

-- | The email address of the recipient of the bounced email.
--
-- /Note:/ Consider using 'recipient' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
briRecipient :: Lens.Lens' BouncedRecipientInfo Types.Address
briRecipient = Lens.field @"recipient"
{-# DEPRECATED briRecipient "Use generic-lens or generic-optics with 'recipient' instead." #-}

-- | The reason for the bounce. You must provide either this parameter or @RecipientDsnFields@ .
--
-- /Note:/ Consider using 'bounceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
briBounceType :: Lens.Lens' BouncedRecipientInfo (Core.Maybe Types.BounceType)
briBounceType = Lens.field @"bounceType"
{-# DEPRECATED briBounceType "Use generic-lens or generic-optics with 'bounceType' instead." #-}

-- | This parameter is used only for sending authorization. It is the ARN of the identity that is associated with the sending authorization policy that permits you to receive email for the recipient of the bounced email. For more information about sending authorization, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html Amazon SES Developer Guide> .
--
-- /Note:/ Consider using 'recipientArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
briRecipientArn :: Lens.Lens' BouncedRecipientInfo (Core.Maybe Types.RecipientArn)
briRecipientArn = Lens.field @"recipientArn"
{-# DEPRECATED briRecipientArn "Use generic-lens or generic-optics with 'recipientArn' instead." #-}

-- | Recipient-related DSN fields, most of which would normally be filled in automatically when provided with a @BounceType@ . You must provide either this parameter or @BounceType@ .
--
-- /Note:/ Consider using 'recipientDsnFields' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
briRecipientDsnFields :: Lens.Lens' BouncedRecipientInfo (Core.Maybe Types.RecipientDsnFields)
briRecipientDsnFields = Lens.field @"recipientDsnFields"
{-# DEPRECATED briRecipientDsnFields "Use generic-lens or generic-optics with 'recipientDsnFields' instead." #-}
