{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.SMSChannelResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.SMSChannelResponse
  ( SMSChannelResponse (..),

    -- * Smart constructor
    mkSMSChannelResponse,

    -- * Lenses
    smscrPlatform,
    smscrApplicationId,
    smscrCreationDate,
    smscrEnabled,
    smscrHasCredential,
    smscrId,
    smscrIsArchived,
    smscrLastModifiedBy,
    smscrLastModifiedDate,
    smscrPromotionalMessagesPerSecond,
    smscrSenderId,
    smscrShortCode,
    smscrTransactionalMessagesPerSecond,
    smscrVersion,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Provides information about the status and settings of the SMS channel for an application.
--
-- /See:/ 'mkSMSChannelResponse' smart constructor.
data SMSChannelResponse = SMSChannelResponse'
  { -- | The type of messaging or notification platform for the channel. For the SMS channel, this value is SMS.
    platform :: Core.Text,
    -- | The unique identifier for the application that the SMS channel applies to.
    applicationId :: Core.Maybe Core.Text,
    -- | The date and time, in ISO 8601 format, when the SMS channel was enabled.
    creationDate :: Core.Maybe Core.Text,
    -- | Specifies whether the SMS channel is enabled for the application.
    enabled :: Core.Maybe Core.Bool,
    -- | (Not used) This property is retained only for backward compatibility.
    hasCredential :: Core.Maybe Core.Bool,
    -- | (Deprecated) An identifier for the SMS channel. This property is retained only for backward compatibility.
    id :: Core.Maybe Core.Text,
    -- | Specifies whether the SMS channel is archived.
    isArchived :: Core.Maybe Core.Bool,
    -- | The user who last modified the SMS channel.
    lastModifiedBy :: Core.Maybe Core.Text,
    -- | The date and time, in ISO 8601 format, when the SMS channel was last modified.
    lastModifiedDate :: Core.Maybe Core.Text,
    -- | The maximum number of promotional messages that you can send through the SMS channel each second.
    promotionalMessagesPerSecond :: Core.Maybe Core.Int,
    -- | The identity that displays on recipients' devices when they receive messages from the SMS channel.
    senderId :: Core.Maybe Core.Text,
    -- | The registered short code to use when you send messages through the SMS channel.
    shortCode :: Core.Maybe Core.Text,
    -- | The maximum number of transactional messages that you can send through the SMS channel each second.
    transactionalMessagesPerSecond :: Core.Maybe Core.Int,
    -- | The current version of the SMS channel.
    version :: Core.Maybe Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SMSChannelResponse' value with any optional fields omitted.
mkSMSChannelResponse ::
  -- | 'platform'
  Core.Text ->
  SMSChannelResponse
mkSMSChannelResponse platform =
  SMSChannelResponse'
    { platform,
      applicationId = Core.Nothing,
      creationDate = Core.Nothing,
      enabled = Core.Nothing,
      hasCredential = Core.Nothing,
      id = Core.Nothing,
      isArchived = Core.Nothing,
      lastModifiedBy = Core.Nothing,
      lastModifiedDate = Core.Nothing,
      promotionalMessagesPerSecond = Core.Nothing,
      senderId = Core.Nothing,
      shortCode = Core.Nothing,
      transactionalMessagesPerSecond = Core.Nothing,
      version = Core.Nothing
    }

-- | The type of messaging or notification platform for the channel. For the SMS channel, this value is SMS.
--
-- /Note:/ Consider using 'platform' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smscrPlatform :: Lens.Lens' SMSChannelResponse Core.Text
smscrPlatform = Lens.field @"platform"
{-# DEPRECATED smscrPlatform "Use generic-lens or generic-optics with 'platform' instead." #-}

-- | The unique identifier for the application that the SMS channel applies to.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smscrApplicationId :: Lens.Lens' SMSChannelResponse (Core.Maybe Core.Text)
smscrApplicationId = Lens.field @"applicationId"
{-# DEPRECATED smscrApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

-- | The date and time, in ISO 8601 format, when the SMS channel was enabled.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smscrCreationDate :: Lens.Lens' SMSChannelResponse (Core.Maybe Core.Text)
smscrCreationDate = Lens.field @"creationDate"
{-# DEPRECATED smscrCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

-- | Specifies whether the SMS channel is enabled for the application.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smscrEnabled :: Lens.Lens' SMSChannelResponse (Core.Maybe Core.Bool)
smscrEnabled = Lens.field @"enabled"
{-# DEPRECATED smscrEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

-- | (Not used) This property is retained only for backward compatibility.
--
-- /Note:/ Consider using 'hasCredential' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smscrHasCredential :: Lens.Lens' SMSChannelResponse (Core.Maybe Core.Bool)
smscrHasCredential = Lens.field @"hasCredential"
{-# DEPRECATED smscrHasCredential "Use generic-lens or generic-optics with 'hasCredential' instead." #-}

-- | (Deprecated) An identifier for the SMS channel. This property is retained only for backward compatibility.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smscrId :: Lens.Lens' SMSChannelResponse (Core.Maybe Core.Text)
smscrId = Lens.field @"id"
{-# DEPRECATED smscrId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | Specifies whether the SMS channel is archived.
--
-- /Note:/ Consider using 'isArchived' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smscrIsArchived :: Lens.Lens' SMSChannelResponse (Core.Maybe Core.Bool)
smscrIsArchived = Lens.field @"isArchived"
{-# DEPRECATED smscrIsArchived "Use generic-lens or generic-optics with 'isArchived' instead." #-}

-- | The user who last modified the SMS channel.
--
-- /Note:/ Consider using 'lastModifiedBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smscrLastModifiedBy :: Lens.Lens' SMSChannelResponse (Core.Maybe Core.Text)
smscrLastModifiedBy = Lens.field @"lastModifiedBy"
{-# DEPRECATED smscrLastModifiedBy "Use generic-lens or generic-optics with 'lastModifiedBy' instead." #-}

-- | The date and time, in ISO 8601 format, when the SMS channel was last modified.
--
-- /Note:/ Consider using 'lastModifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smscrLastModifiedDate :: Lens.Lens' SMSChannelResponse (Core.Maybe Core.Text)
smscrLastModifiedDate = Lens.field @"lastModifiedDate"
{-# DEPRECATED smscrLastModifiedDate "Use generic-lens or generic-optics with 'lastModifiedDate' instead." #-}

-- | The maximum number of promotional messages that you can send through the SMS channel each second.
--
-- /Note:/ Consider using 'promotionalMessagesPerSecond' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smscrPromotionalMessagesPerSecond :: Lens.Lens' SMSChannelResponse (Core.Maybe Core.Int)
smscrPromotionalMessagesPerSecond = Lens.field @"promotionalMessagesPerSecond"
{-# DEPRECATED smscrPromotionalMessagesPerSecond "Use generic-lens or generic-optics with 'promotionalMessagesPerSecond' instead." #-}

-- | The identity that displays on recipients' devices when they receive messages from the SMS channel.
--
-- /Note:/ Consider using 'senderId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smscrSenderId :: Lens.Lens' SMSChannelResponse (Core.Maybe Core.Text)
smscrSenderId = Lens.field @"senderId"
{-# DEPRECATED smscrSenderId "Use generic-lens or generic-optics with 'senderId' instead." #-}

-- | The registered short code to use when you send messages through the SMS channel.
--
-- /Note:/ Consider using 'shortCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smscrShortCode :: Lens.Lens' SMSChannelResponse (Core.Maybe Core.Text)
smscrShortCode = Lens.field @"shortCode"
{-# DEPRECATED smscrShortCode "Use generic-lens or generic-optics with 'shortCode' instead." #-}

-- | The maximum number of transactional messages that you can send through the SMS channel each second.
--
-- /Note:/ Consider using 'transactionalMessagesPerSecond' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smscrTransactionalMessagesPerSecond :: Lens.Lens' SMSChannelResponse (Core.Maybe Core.Int)
smscrTransactionalMessagesPerSecond = Lens.field @"transactionalMessagesPerSecond"
{-# DEPRECATED smscrTransactionalMessagesPerSecond "Use generic-lens or generic-optics with 'transactionalMessagesPerSecond' instead." #-}

-- | The current version of the SMS channel.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smscrVersion :: Lens.Lens' SMSChannelResponse (Core.Maybe Core.Int)
smscrVersion = Lens.field @"version"
{-# DEPRECATED smscrVersion "Use generic-lens or generic-optics with 'version' instead." #-}

instance Core.FromJSON SMSChannelResponse where
  parseJSON =
    Core.withObject "SMSChannelResponse" Core.$
      \x ->
        SMSChannelResponse'
          Core.<$> (x Core..: "Platform")
          Core.<*> (x Core..:? "ApplicationId")
          Core.<*> (x Core..:? "CreationDate")
          Core.<*> (x Core..:? "Enabled")
          Core.<*> (x Core..:? "HasCredential")
          Core.<*> (x Core..:? "Id")
          Core.<*> (x Core..:? "IsArchived")
          Core.<*> (x Core..:? "LastModifiedBy")
          Core.<*> (x Core..:? "LastModifiedDate")
          Core.<*> (x Core..:? "PromotionalMessagesPerSecond")
          Core.<*> (x Core..:? "SenderId")
          Core.<*> (x Core..:? "ShortCode")
          Core.<*> (x Core..:? "TransactionalMessagesPerSecond")
          Core.<*> (x Core..:? "Version")
