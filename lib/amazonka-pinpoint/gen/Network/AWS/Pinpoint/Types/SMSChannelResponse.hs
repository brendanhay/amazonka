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
    smscPlatform,
    smscShortCode,
    smscLastModifiedDate,
    smscEnabled,
    smscSenderId,
    smscTransactionalMessagesPerSecond,
    smscPromotionalMessagesPerSecond,
    smscIsArchived,
    smscApplicationId,
    smscVersion,
    smscId,
    smscCreationDate,
    smscLastModifiedBy,
    smscHasCredential,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides information about the status and settings of the SMS channel for an application.
--
-- /See:/ 'mkSMSChannelResponse' smart constructor.
data SMSChannelResponse = SMSChannelResponse'
  { -- | The type of messaging or notification platform for the channel. For the SMS channel, this value is SMS.
    platform :: Lude.Text,
    -- | The registered short code to use when you send messages through the SMS channel.
    shortCode :: Lude.Maybe Lude.Text,
    -- | The date and time, in ISO 8601 format, when the SMS channel was last modified.
    lastModifiedDate :: Lude.Maybe Lude.Text,
    -- | Specifies whether the SMS channel is enabled for the application.
    enabled :: Lude.Maybe Lude.Bool,
    -- | The identity that displays on recipients' devices when they receive messages from the SMS channel.
    senderId :: Lude.Maybe Lude.Text,
    -- | The maximum number of transactional messages that you can send through the SMS channel each second.
    transactionalMessagesPerSecond :: Lude.Maybe Lude.Int,
    -- | The maximum number of promotional messages that you can send through the SMS channel each second.
    promotionalMessagesPerSecond :: Lude.Maybe Lude.Int,
    -- | Specifies whether the SMS channel is archived.
    isArchived :: Lude.Maybe Lude.Bool,
    -- | The unique identifier for the application that the SMS channel applies to.
    applicationId :: Lude.Maybe Lude.Text,
    -- | The current version of the SMS channel.
    version :: Lude.Maybe Lude.Int,
    -- | (Deprecated) An identifier for the SMS channel. This property is retained only for backward compatibility.
    id :: Lude.Maybe Lude.Text,
    -- | The date and time, in ISO 8601 format, when the SMS channel was enabled.
    creationDate :: Lude.Maybe Lude.Text,
    -- | The user who last modified the SMS channel.
    lastModifiedBy :: Lude.Maybe Lude.Text,
    -- | (Not used) This property is retained only for backward compatibility.
    hasCredential :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SMSChannelResponse' with the minimum fields required to make a request.
--
-- * 'platform' - The type of messaging or notification platform for the channel. For the SMS channel, this value is SMS.
-- * 'shortCode' - The registered short code to use when you send messages through the SMS channel.
-- * 'lastModifiedDate' - The date and time, in ISO 8601 format, when the SMS channel was last modified.
-- * 'enabled' - Specifies whether the SMS channel is enabled for the application.
-- * 'senderId' - The identity that displays on recipients' devices when they receive messages from the SMS channel.
-- * 'transactionalMessagesPerSecond' - The maximum number of transactional messages that you can send through the SMS channel each second.
-- * 'promotionalMessagesPerSecond' - The maximum number of promotional messages that you can send through the SMS channel each second.
-- * 'isArchived' - Specifies whether the SMS channel is archived.
-- * 'applicationId' - The unique identifier for the application that the SMS channel applies to.
-- * 'version' - The current version of the SMS channel.
-- * 'id' - (Deprecated) An identifier for the SMS channel. This property is retained only for backward compatibility.
-- * 'creationDate' - The date and time, in ISO 8601 format, when the SMS channel was enabled.
-- * 'lastModifiedBy' - The user who last modified the SMS channel.
-- * 'hasCredential' - (Not used) This property is retained only for backward compatibility.
mkSMSChannelResponse ::
  -- | 'platform'
  Lude.Text ->
  SMSChannelResponse
mkSMSChannelResponse pPlatform_ =
  SMSChannelResponse'
    { platform = pPlatform_,
      shortCode = Lude.Nothing,
      lastModifiedDate = Lude.Nothing,
      enabled = Lude.Nothing,
      senderId = Lude.Nothing,
      transactionalMessagesPerSecond = Lude.Nothing,
      promotionalMessagesPerSecond = Lude.Nothing,
      isArchived = Lude.Nothing,
      applicationId = Lude.Nothing,
      version = Lude.Nothing,
      id = Lude.Nothing,
      creationDate = Lude.Nothing,
      lastModifiedBy = Lude.Nothing,
      hasCredential = Lude.Nothing
    }

-- | The type of messaging or notification platform for the channel. For the SMS channel, this value is SMS.
--
-- /Note:/ Consider using 'platform' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smscPlatform :: Lens.Lens' SMSChannelResponse Lude.Text
smscPlatform = Lens.lens (platform :: SMSChannelResponse -> Lude.Text) (\s a -> s {platform = a} :: SMSChannelResponse)
{-# DEPRECATED smscPlatform "Use generic-lens or generic-optics with 'platform' instead." #-}

-- | The registered short code to use when you send messages through the SMS channel.
--
-- /Note:/ Consider using 'shortCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smscShortCode :: Lens.Lens' SMSChannelResponse (Lude.Maybe Lude.Text)
smscShortCode = Lens.lens (shortCode :: SMSChannelResponse -> Lude.Maybe Lude.Text) (\s a -> s {shortCode = a} :: SMSChannelResponse)
{-# DEPRECATED smscShortCode "Use generic-lens or generic-optics with 'shortCode' instead." #-}

-- | The date and time, in ISO 8601 format, when the SMS channel was last modified.
--
-- /Note:/ Consider using 'lastModifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smscLastModifiedDate :: Lens.Lens' SMSChannelResponse (Lude.Maybe Lude.Text)
smscLastModifiedDate = Lens.lens (lastModifiedDate :: SMSChannelResponse -> Lude.Maybe Lude.Text) (\s a -> s {lastModifiedDate = a} :: SMSChannelResponse)
{-# DEPRECATED smscLastModifiedDate "Use generic-lens or generic-optics with 'lastModifiedDate' instead." #-}

-- | Specifies whether the SMS channel is enabled for the application.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smscEnabled :: Lens.Lens' SMSChannelResponse (Lude.Maybe Lude.Bool)
smscEnabled = Lens.lens (enabled :: SMSChannelResponse -> Lude.Maybe Lude.Bool) (\s a -> s {enabled = a} :: SMSChannelResponse)
{-# DEPRECATED smscEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

-- | The identity that displays on recipients' devices when they receive messages from the SMS channel.
--
-- /Note:/ Consider using 'senderId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smscSenderId :: Lens.Lens' SMSChannelResponse (Lude.Maybe Lude.Text)
smscSenderId = Lens.lens (senderId :: SMSChannelResponse -> Lude.Maybe Lude.Text) (\s a -> s {senderId = a} :: SMSChannelResponse)
{-# DEPRECATED smscSenderId "Use generic-lens or generic-optics with 'senderId' instead." #-}

-- | The maximum number of transactional messages that you can send through the SMS channel each second.
--
-- /Note:/ Consider using 'transactionalMessagesPerSecond' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smscTransactionalMessagesPerSecond :: Lens.Lens' SMSChannelResponse (Lude.Maybe Lude.Int)
smscTransactionalMessagesPerSecond = Lens.lens (transactionalMessagesPerSecond :: SMSChannelResponse -> Lude.Maybe Lude.Int) (\s a -> s {transactionalMessagesPerSecond = a} :: SMSChannelResponse)
{-# DEPRECATED smscTransactionalMessagesPerSecond "Use generic-lens or generic-optics with 'transactionalMessagesPerSecond' instead." #-}

-- | The maximum number of promotional messages that you can send through the SMS channel each second.
--
-- /Note:/ Consider using 'promotionalMessagesPerSecond' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smscPromotionalMessagesPerSecond :: Lens.Lens' SMSChannelResponse (Lude.Maybe Lude.Int)
smscPromotionalMessagesPerSecond = Lens.lens (promotionalMessagesPerSecond :: SMSChannelResponse -> Lude.Maybe Lude.Int) (\s a -> s {promotionalMessagesPerSecond = a} :: SMSChannelResponse)
{-# DEPRECATED smscPromotionalMessagesPerSecond "Use generic-lens or generic-optics with 'promotionalMessagesPerSecond' instead." #-}

-- | Specifies whether the SMS channel is archived.
--
-- /Note:/ Consider using 'isArchived' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smscIsArchived :: Lens.Lens' SMSChannelResponse (Lude.Maybe Lude.Bool)
smscIsArchived = Lens.lens (isArchived :: SMSChannelResponse -> Lude.Maybe Lude.Bool) (\s a -> s {isArchived = a} :: SMSChannelResponse)
{-# DEPRECATED smscIsArchived "Use generic-lens or generic-optics with 'isArchived' instead." #-}

-- | The unique identifier for the application that the SMS channel applies to.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smscApplicationId :: Lens.Lens' SMSChannelResponse (Lude.Maybe Lude.Text)
smscApplicationId = Lens.lens (applicationId :: SMSChannelResponse -> Lude.Maybe Lude.Text) (\s a -> s {applicationId = a} :: SMSChannelResponse)
{-# DEPRECATED smscApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

-- | The current version of the SMS channel.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smscVersion :: Lens.Lens' SMSChannelResponse (Lude.Maybe Lude.Int)
smscVersion = Lens.lens (version :: SMSChannelResponse -> Lude.Maybe Lude.Int) (\s a -> s {version = a} :: SMSChannelResponse)
{-# DEPRECATED smscVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | (Deprecated) An identifier for the SMS channel. This property is retained only for backward compatibility.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smscId :: Lens.Lens' SMSChannelResponse (Lude.Maybe Lude.Text)
smscId = Lens.lens (id :: SMSChannelResponse -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: SMSChannelResponse)
{-# DEPRECATED smscId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The date and time, in ISO 8601 format, when the SMS channel was enabled.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smscCreationDate :: Lens.Lens' SMSChannelResponse (Lude.Maybe Lude.Text)
smscCreationDate = Lens.lens (creationDate :: SMSChannelResponse -> Lude.Maybe Lude.Text) (\s a -> s {creationDate = a} :: SMSChannelResponse)
{-# DEPRECATED smscCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

-- | The user who last modified the SMS channel.
--
-- /Note:/ Consider using 'lastModifiedBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smscLastModifiedBy :: Lens.Lens' SMSChannelResponse (Lude.Maybe Lude.Text)
smscLastModifiedBy = Lens.lens (lastModifiedBy :: SMSChannelResponse -> Lude.Maybe Lude.Text) (\s a -> s {lastModifiedBy = a} :: SMSChannelResponse)
{-# DEPRECATED smscLastModifiedBy "Use generic-lens or generic-optics with 'lastModifiedBy' instead." #-}

-- | (Not used) This property is retained only for backward compatibility.
--
-- /Note:/ Consider using 'hasCredential' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smscHasCredential :: Lens.Lens' SMSChannelResponse (Lude.Maybe Lude.Bool)
smscHasCredential = Lens.lens (hasCredential :: SMSChannelResponse -> Lude.Maybe Lude.Bool) (\s a -> s {hasCredential = a} :: SMSChannelResponse)
{-# DEPRECATED smscHasCredential "Use generic-lens or generic-optics with 'hasCredential' instead." #-}

instance Lude.FromJSON SMSChannelResponse where
  parseJSON =
    Lude.withObject
      "SMSChannelResponse"
      ( \x ->
          SMSChannelResponse'
            Lude.<$> (x Lude..: "Platform")
            Lude.<*> (x Lude..:? "ShortCode")
            Lude.<*> (x Lude..:? "LastModifiedDate")
            Lude.<*> (x Lude..:? "Enabled")
            Lude.<*> (x Lude..:? "SenderId")
            Lude.<*> (x Lude..:? "TransactionalMessagesPerSecond")
            Lude.<*> (x Lude..:? "PromotionalMessagesPerSecond")
            Lude.<*> (x Lude..:? "IsArchived")
            Lude.<*> (x Lude..:? "ApplicationId")
            Lude.<*> (x Lude..:? "Version")
            Lude.<*> (x Lude..:? "Id")
            Lude.<*> (x Lude..:? "CreationDate")
            Lude.<*> (x Lude..:? "LastModifiedBy")
            Lude.<*> (x Lude..:? "HasCredential")
      )
