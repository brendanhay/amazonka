-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.EmailChannelResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.EmailChannelResponse
  ( EmailChannelResponse (..),

    -- * Smart constructor
    mkEmailChannelResponse,

    -- * Lenses
    ecMessagesPerSecond,
    ecLastModifiedDate,
    ecEnabled,
    ecFromAddress,
    ecIsArchived,
    ecApplicationId,
    ecVersion,
    ecConfigurationSet,
    ecId,
    ecCreationDate,
    ecLastModifiedBy,
    ecIdentity,
    ecHasCredential,
    ecRoleARN,
    ecPlatform,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides information about the status and settings of the email channel for an application.
--
-- /See:/ 'mkEmailChannelResponse' smart constructor.
data EmailChannelResponse = EmailChannelResponse'
  { messagesPerSecond ::
      Lude.Maybe Lude.Int,
    lastModifiedDate :: Lude.Maybe Lude.Text,
    enabled :: Lude.Maybe Lude.Bool,
    fromAddress :: Lude.Maybe Lude.Text,
    isArchived :: Lude.Maybe Lude.Bool,
    applicationId :: Lude.Maybe Lude.Text,
    version :: Lude.Maybe Lude.Int,
    configurationSet :: Lude.Maybe Lude.Text,
    id :: Lude.Maybe Lude.Text,
    creationDate :: Lude.Maybe Lude.Text,
    lastModifiedBy :: Lude.Maybe Lude.Text,
    identity :: Lude.Maybe Lude.Text,
    hasCredential :: Lude.Maybe Lude.Bool,
    roleARN :: Lude.Maybe Lude.Text,
    platform :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EmailChannelResponse' with the minimum fields required to make a request.
--
-- * 'applicationId' - The unique identifier for the application that the email channel applies to.
-- * 'configurationSet' - The <https://docs.aws.amazon.com/ses/latest/APIReference/API_ConfigurationSet.html Amazon SES configuration set> that's applied to messages that are sent through the channel.
-- * 'creationDate' - The date and time, in ISO 8601 format, when the email channel was enabled.
-- * 'enabled' - Specifies whether the email channel is enabled for the application.
-- * 'fromAddress' - The verified email address that email is sent from when you send email through the channel.
-- * 'hasCredential' - (Not used) This property is retained only for backward compatibility.
-- * 'id' - (Deprecated) An identifier for the email channel. This property is retained only for backward compatibility.
-- * 'identity' - The Amazon Resource Name (ARN) of the identity, verified with Amazon Simple Email Service (Amazon SES), that's used when you send email through the channel.
-- * 'isArchived' - Specifies whether the email channel is archived.
-- * 'lastModifiedBy' - The user who last modified the email channel.
-- * 'lastModifiedDate' - The date and time, in ISO 8601 format, when the email channel was last modified.
-- * 'messagesPerSecond' - The maximum number of emails that can be sent through the channel each second.
-- * 'platform' - The type of messaging or notification platform for the channel. For the email channel, this value is EMAIL.
-- * 'roleARN' - The ARN of the AWS Identity and Access Management (IAM) role that Amazon Pinpoint uses to submit email-related event data for the channel.
-- * 'version' - The current version of the email channel.
mkEmailChannelResponse ::
  -- | 'platform'
  Lude.Text ->
  EmailChannelResponse
mkEmailChannelResponse pPlatform_ =
  EmailChannelResponse'
    { messagesPerSecond = Lude.Nothing,
      lastModifiedDate = Lude.Nothing,
      enabled = Lude.Nothing,
      fromAddress = Lude.Nothing,
      isArchived = Lude.Nothing,
      applicationId = Lude.Nothing,
      version = Lude.Nothing,
      configurationSet = Lude.Nothing,
      id = Lude.Nothing,
      creationDate = Lude.Nothing,
      lastModifiedBy = Lude.Nothing,
      identity = Lude.Nothing,
      hasCredential = Lude.Nothing,
      roleARN = Lude.Nothing,
      platform = pPlatform_
    }

-- | The maximum number of emails that can be sent through the channel each second.
--
-- /Note:/ Consider using 'messagesPerSecond' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecMessagesPerSecond :: Lens.Lens' EmailChannelResponse (Lude.Maybe Lude.Int)
ecMessagesPerSecond = Lens.lens (messagesPerSecond :: EmailChannelResponse -> Lude.Maybe Lude.Int) (\s a -> s {messagesPerSecond = a} :: EmailChannelResponse)
{-# DEPRECATED ecMessagesPerSecond "Use generic-lens or generic-optics with 'messagesPerSecond' instead." #-}

-- | The date and time, in ISO 8601 format, when the email channel was last modified.
--
-- /Note:/ Consider using 'lastModifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecLastModifiedDate :: Lens.Lens' EmailChannelResponse (Lude.Maybe Lude.Text)
ecLastModifiedDate = Lens.lens (lastModifiedDate :: EmailChannelResponse -> Lude.Maybe Lude.Text) (\s a -> s {lastModifiedDate = a} :: EmailChannelResponse)
{-# DEPRECATED ecLastModifiedDate "Use generic-lens or generic-optics with 'lastModifiedDate' instead." #-}

-- | Specifies whether the email channel is enabled for the application.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecEnabled :: Lens.Lens' EmailChannelResponse (Lude.Maybe Lude.Bool)
ecEnabled = Lens.lens (enabled :: EmailChannelResponse -> Lude.Maybe Lude.Bool) (\s a -> s {enabled = a} :: EmailChannelResponse)
{-# DEPRECATED ecEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

-- | The verified email address that email is sent from when you send email through the channel.
--
-- /Note:/ Consider using 'fromAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecFromAddress :: Lens.Lens' EmailChannelResponse (Lude.Maybe Lude.Text)
ecFromAddress = Lens.lens (fromAddress :: EmailChannelResponse -> Lude.Maybe Lude.Text) (\s a -> s {fromAddress = a} :: EmailChannelResponse)
{-# DEPRECATED ecFromAddress "Use generic-lens or generic-optics with 'fromAddress' instead." #-}

-- | Specifies whether the email channel is archived.
--
-- /Note:/ Consider using 'isArchived' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecIsArchived :: Lens.Lens' EmailChannelResponse (Lude.Maybe Lude.Bool)
ecIsArchived = Lens.lens (isArchived :: EmailChannelResponse -> Lude.Maybe Lude.Bool) (\s a -> s {isArchived = a} :: EmailChannelResponse)
{-# DEPRECATED ecIsArchived "Use generic-lens or generic-optics with 'isArchived' instead." #-}

-- | The unique identifier for the application that the email channel applies to.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecApplicationId :: Lens.Lens' EmailChannelResponse (Lude.Maybe Lude.Text)
ecApplicationId = Lens.lens (applicationId :: EmailChannelResponse -> Lude.Maybe Lude.Text) (\s a -> s {applicationId = a} :: EmailChannelResponse)
{-# DEPRECATED ecApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

-- | The current version of the email channel.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecVersion :: Lens.Lens' EmailChannelResponse (Lude.Maybe Lude.Int)
ecVersion = Lens.lens (version :: EmailChannelResponse -> Lude.Maybe Lude.Int) (\s a -> s {version = a} :: EmailChannelResponse)
{-# DEPRECATED ecVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | The <https://docs.aws.amazon.com/ses/latest/APIReference/API_ConfigurationSet.html Amazon SES configuration set> that's applied to messages that are sent through the channel.
--
-- /Note:/ Consider using 'configurationSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecConfigurationSet :: Lens.Lens' EmailChannelResponse (Lude.Maybe Lude.Text)
ecConfigurationSet = Lens.lens (configurationSet :: EmailChannelResponse -> Lude.Maybe Lude.Text) (\s a -> s {configurationSet = a} :: EmailChannelResponse)
{-# DEPRECATED ecConfigurationSet "Use generic-lens or generic-optics with 'configurationSet' instead." #-}

-- | (Deprecated) An identifier for the email channel. This property is retained only for backward compatibility.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecId :: Lens.Lens' EmailChannelResponse (Lude.Maybe Lude.Text)
ecId = Lens.lens (id :: EmailChannelResponse -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: EmailChannelResponse)
{-# DEPRECATED ecId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The date and time, in ISO 8601 format, when the email channel was enabled.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecCreationDate :: Lens.Lens' EmailChannelResponse (Lude.Maybe Lude.Text)
ecCreationDate = Lens.lens (creationDate :: EmailChannelResponse -> Lude.Maybe Lude.Text) (\s a -> s {creationDate = a} :: EmailChannelResponse)
{-# DEPRECATED ecCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

-- | The user who last modified the email channel.
--
-- /Note:/ Consider using 'lastModifiedBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecLastModifiedBy :: Lens.Lens' EmailChannelResponse (Lude.Maybe Lude.Text)
ecLastModifiedBy = Lens.lens (lastModifiedBy :: EmailChannelResponse -> Lude.Maybe Lude.Text) (\s a -> s {lastModifiedBy = a} :: EmailChannelResponse)
{-# DEPRECATED ecLastModifiedBy "Use generic-lens or generic-optics with 'lastModifiedBy' instead." #-}

-- | The Amazon Resource Name (ARN) of the identity, verified with Amazon Simple Email Service (Amazon SES), that's used when you send email through the channel.
--
-- /Note:/ Consider using 'identity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecIdentity :: Lens.Lens' EmailChannelResponse (Lude.Maybe Lude.Text)
ecIdentity = Lens.lens (identity :: EmailChannelResponse -> Lude.Maybe Lude.Text) (\s a -> s {identity = a} :: EmailChannelResponse)
{-# DEPRECATED ecIdentity "Use generic-lens or generic-optics with 'identity' instead." #-}

-- | (Not used) This property is retained only for backward compatibility.
--
-- /Note:/ Consider using 'hasCredential' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecHasCredential :: Lens.Lens' EmailChannelResponse (Lude.Maybe Lude.Bool)
ecHasCredential = Lens.lens (hasCredential :: EmailChannelResponse -> Lude.Maybe Lude.Bool) (\s a -> s {hasCredential = a} :: EmailChannelResponse)
{-# DEPRECATED ecHasCredential "Use generic-lens or generic-optics with 'hasCredential' instead." #-}

-- | The ARN of the AWS Identity and Access Management (IAM) role that Amazon Pinpoint uses to submit email-related event data for the channel.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecRoleARN :: Lens.Lens' EmailChannelResponse (Lude.Maybe Lude.Text)
ecRoleARN = Lens.lens (roleARN :: EmailChannelResponse -> Lude.Maybe Lude.Text) (\s a -> s {roleARN = a} :: EmailChannelResponse)
{-# DEPRECATED ecRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

-- | The type of messaging or notification platform for the channel. For the email channel, this value is EMAIL.
--
-- /Note:/ Consider using 'platform' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecPlatform :: Lens.Lens' EmailChannelResponse Lude.Text
ecPlatform = Lens.lens (platform :: EmailChannelResponse -> Lude.Text) (\s a -> s {platform = a} :: EmailChannelResponse)
{-# DEPRECATED ecPlatform "Use generic-lens or generic-optics with 'platform' instead." #-}

instance Lude.FromJSON EmailChannelResponse where
  parseJSON =
    Lude.withObject
      "EmailChannelResponse"
      ( \x ->
          EmailChannelResponse'
            Lude.<$> (x Lude..:? "MessagesPerSecond")
            Lude.<*> (x Lude..:? "LastModifiedDate")
            Lude.<*> (x Lude..:? "Enabled")
            Lude.<*> (x Lude..:? "FromAddress")
            Lude.<*> (x Lude..:? "IsArchived")
            Lude.<*> (x Lude..:? "ApplicationId")
            Lude.<*> (x Lude..:? "Version")
            Lude.<*> (x Lude..:? "ConfigurationSet")
            Lude.<*> (x Lude..:? "Id")
            Lude.<*> (x Lude..:? "CreationDate")
            Lude.<*> (x Lude..:? "LastModifiedBy")
            Lude.<*> (x Lude..:? "Identity")
            Lude.<*> (x Lude..:? "HasCredential")
            Lude.<*> (x Lude..:? "RoleArn")
            Lude.<*> (x Lude..: "Platform")
      )
