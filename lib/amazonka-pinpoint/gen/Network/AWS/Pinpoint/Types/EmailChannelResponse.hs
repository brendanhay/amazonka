{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.EmailChannelResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Pinpoint.Types.EmailChannelResponse
  ( EmailChannelResponse (..)
  -- * Smart constructor
  , mkEmailChannelResponse
  -- * Lenses
  , ecrPlatform
  , ecrApplicationId
  , ecrConfigurationSet
  , ecrCreationDate
  , ecrEnabled
  , ecrFromAddress
  , ecrHasCredential
  , ecrId
  , ecrIdentity
  , ecrIsArchived
  , ecrLastModifiedBy
  , ecrLastModifiedDate
  , ecrMessagesPerSecond
  , ecrRoleArn
  , ecrVersion
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Provides information about the status and settings of the email channel for an application.
--
-- /See:/ 'mkEmailChannelResponse' smart constructor.
data EmailChannelResponse = EmailChannelResponse'
  { platform :: Core.Text
    -- ^ The type of messaging or notification platform for the channel. For the email channel, this value is EMAIL.
  , applicationId :: Core.Maybe Core.Text
    -- ^ The unique identifier for the application that the email channel applies to.
  , configurationSet :: Core.Maybe Core.Text
    -- ^ The <https://docs.aws.amazon.com/ses/latest/APIReference/API_ConfigurationSet.html Amazon SES configuration set> that's applied to messages that are sent through the channel.
  , creationDate :: Core.Maybe Core.Text
    -- ^ The date and time, in ISO 8601 format, when the email channel was enabled.
  , enabled :: Core.Maybe Core.Bool
    -- ^ Specifies whether the email channel is enabled for the application.
  , fromAddress :: Core.Maybe Core.Text
    -- ^ The verified email address that email is sent from when you send email through the channel.
  , hasCredential :: Core.Maybe Core.Bool
    -- ^ (Not used) This property is retained only for backward compatibility.
  , id :: Core.Maybe Core.Text
    -- ^ (Deprecated) An identifier for the email channel. This property is retained only for backward compatibility.
  , identity :: Core.Maybe Core.Text
    -- ^ The Amazon Resource Name (ARN) of the identity, verified with Amazon Simple Email Service (Amazon SES), that's used when you send email through the channel.
  , isArchived :: Core.Maybe Core.Bool
    -- ^ Specifies whether the email channel is archived.
  , lastModifiedBy :: Core.Maybe Core.Text
    -- ^ The user who last modified the email channel.
  , lastModifiedDate :: Core.Maybe Core.Text
    -- ^ The date and time, in ISO 8601 format, when the email channel was last modified.
  , messagesPerSecond :: Core.Maybe Core.Int
    -- ^ The maximum number of emails that can be sent through the channel each second.
  , roleArn :: Core.Maybe Core.Text
    -- ^ The ARN of the AWS Identity and Access Management (IAM) role that Amazon Pinpoint uses to submit email-related event data for the channel.
  , version :: Core.Maybe Core.Int
    -- ^ The current version of the email channel.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EmailChannelResponse' value with any optional fields omitted.
mkEmailChannelResponse
    :: Core.Text -- ^ 'platform'
    -> EmailChannelResponse
mkEmailChannelResponse platform
  = EmailChannelResponse'{platform, applicationId = Core.Nothing,
                          configurationSet = Core.Nothing, creationDate = Core.Nothing,
                          enabled = Core.Nothing, fromAddress = Core.Nothing,
                          hasCredential = Core.Nothing, id = Core.Nothing,
                          identity = Core.Nothing, isArchived = Core.Nothing,
                          lastModifiedBy = Core.Nothing, lastModifiedDate = Core.Nothing,
                          messagesPerSecond = Core.Nothing, roleArn = Core.Nothing,
                          version = Core.Nothing}

-- | The type of messaging or notification platform for the channel. For the email channel, this value is EMAIL.
--
-- /Note:/ Consider using 'platform' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecrPlatform :: Lens.Lens' EmailChannelResponse Core.Text
ecrPlatform = Lens.field @"platform"
{-# INLINEABLE ecrPlatform #-}
{-# DEPRECATED platform "Use generic-lens or generic-optics with 'platform' instead"  #-}

-- | The unique identifier for the application that the email channel applies to.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecrApplicationId :: Lens.Lens' EmailChannelResponse (Core.Maybe Core.Text)
ecrApplicationId = Lens.field @"applicationId"
{-# INLINEABLE ecrApplicationId #-}
{-# DEPRECATED applicationId "Use generic-lens or generic-optics with 'applicationId' instead"  #-}

-- | The <https://docs.aws.amazon.com/ses/latest/APIReference/API_ConfigurationSet.html Amazon SES configuration set> that's applied to messages that are sent through the channel.
--
-- /Note:/ Consider using 'configurationSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecrConfigurationSet :: Lens.Lens' EmailChannelResponse (Core.Maybe Core.Text)
ecrConfigurationSet = Lens.field @"configurationSet"
{-# INLINEABLE ecrConfigurationSet #-}
{-# DEPRECATED configurationSet "Use generic-lens or generic-optics with 'configurationSet' instead"  #-}

-- | The date and time, in ISO 8601 format, when the email channel was enabled.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecrCreationDate :: Lens.Lens' EmailChannelResponse (Core.Maybe Core.Text)
ecrCreationDate = Lens.field @"creationDate"
{-# INLINEABLE ecrCreationDate #-}
{-# DEPRECATED creationDate "Use generic-lens or generic-optics with 'creationDate' instead"  #-}

-- | Specifies whether the email channel is enabled for the application.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecrEnabled :: Lens.Lens' EmailChannelResponse (Core.Maybe Core.Bool)
ecrEnabled = Lens.field @"enabled"
{-# INLINEABLE ecrEnabled #-}
{-# DEPRECATED enabled "Use generic-lens or generic-optics with 'enabled' instead"  #-}

-- | The verified email address that email is sent from when you send email through the channel.
--
-- /Note:/ Consider using 'fromAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecrFromAddress :: Lens.Lens' EmailChannelResponse (Core.Maybe Core.Text)
ecrFromAddress = Lens.field @"fromAddress"
{-# INLINEABLE ecrFromAddress #-}
{-# DEPRECATED fromAddress "Use generic-lens or generic-optics with 'fromAddress' instead"  #-}

-- | (Not used) This property is retained only for backward compatibility.
--
-- /Note:/ Consider using 'hasCredential' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecrHasCredential :: Lens.Lens' EmailChannelResponse (Core.Maybe Core.Bool)
ecrHasCredential = Lens.field @"hasCredential"
{-# INLINEABLE ecrHasCredential #-}
{-# DEPRECATED hasCredential "Use generic-lens or generic-optics with 'hasCredential' instead"  #-}

-- | (Deprecated) An identifier for the email channel. This property is retained only for backward compatibility.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecrId :: Lens.Lens' EmailChannelResponse (Core.Maybe Core.Text)
ecrId = Lens.field @"id"
{-# INLINEABLE ecrId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The Amazon Resource Name (ARN) of the identity, verified with Amazon Simple Email Service (Amazon SES), that's used when you send email through the channel.
--
-- /Note:/ Consider using 'identity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecrIdentity :: Lens.Lens' EmailChannelResponse (Core.Maybe Core.Text)
ecrIdentity = Lens.field @"identity"
{-# INLINEABLE ecrIdentity #-}
{-# DEPRECATED identity "Use generic-lens or generic-optics with 'identity' instead"  #-}

-- | Specifies whether the email channel is archived.
--
-- /Note:/ Consider using 'isArchived' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecrIsArchived :: Lens.Lens' EmailChannelResponse (Core.Maybe Core.Bool)
ecrIsArchived = Lens.field @"isArchived"
{-# INLINEABLE ecrIsArchived #-}
{-# DEPRECATED isArchived "Use generic-lens or generic-optics with 'isArchived' instead"  #-}

-- | The user who last modified the email channel.
--
-- /Note:/ Consider using 'lastModifiedBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecrLastModifiedBy :: Lens.Lens' EmailChannelResponse (Core.Maybe Core.Text)
ecrLastModifiedBy = Lens.field @"lastModifiedBy"
{-# INLINEABLE ecrLastModifiedBy #-}
{-# DEPRECATED lastModifiedBy "Use generic-lens or generic-optics with 'lastModifiedBy' instead"  #-}

-- | The date and time, in ISO 8601 format, when the email channel was last modified.
--
-- /Note:/ Consider using 'lastModifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecrLastModifiedDate :: Lens.Lens' EmailChannelResponse (Core.Maybe Core.Text)
ecrLastModifiedDate = Lens.field @"lastModifiedDate"
{-# INLINEABLE ecrLastModifiedDate #-}
{-# DEPRECATED lastModifiedDate "Use generic-lens or generic-optics with 'lastModifiedDate' instead"  #-}

-- | The maximum number of emails that can be sent through the channel each second.
--
-- /Note:/ Consider using 'messagesPerSecond' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecrMessagesPerSecond :: Lens.Lens' EmailChannelResponse (Core.Maybe Core.Int)
ecrMessagesPerSecond = Lens.field @"messagesPerSecond"
{-# INLINEABLE ecrMessagesPerSecond #-}
{-# DEPRECATED messagesPerSecond "Use generic-lens or generic-optics with 'messagesPerSecond' instead"  #-}

-- | The ARN of the AWS Identity and Access Management (IAM) role that Amazon Pinpoint uses to submit email-related event data for the channel.
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecrRoleArn :: Lens.Lens' EmailChannelResponse (Core.Maybe Core.Text)
ecrRoleArn = Lens.field @"roleArn"
{-# INLINEABLE ecrRoleArn #-}
{-# DEPRECATED roleArn "Use generic-lens or generic-optics with 'roleArn' instead"  #-}

-- | The current version of the email channel.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecrVersion :: Lens.Lens' EmailChannelResponse (Core.Maybe Core.Int)
ecrVersion = Lens.field @"version"
{-# INLINEABLE ecrVersion #-}
{-# DEPRECATED version "Use generic-lens or generic-optics with 'version' instead"  #-}

instance Core.FromJSON EmailChannelResponse where
        parseJSON
          = Core.withObject "EmailChannelResponse" Core.$
              \ x ->
                EmailChannelResponse' Core.<$>
                  (x Core..: "Platform") Core.<*> x Core..:? "ApplicationId" Core.<*>
                    x Core..:? "ConfigurationSet"
                    Core.<*> x Core..:? "CreationDate"
                    Core.<*> x Core..:? "Enabled"
                    Core.<*> x Core..:? "FromAddress"
                    Core.<*> x Core..:? "HasCredential"
                    Core.<*> x Core..:? "Id"
                    Core.<*> x Core..:? "Identity"
                    Core.<*> x Core..:? "IsArchived"
                    Core.<*> x Core..:? "LastModifiedBy"
                    Core.<*> x Core..:? "LastModifiedDate"
                    Core.<*> x Core..:? "MessagesPerSecond"
                    Core.<*> x Core..:? "RoleArn"
                    Core.<*> x Core..:? "Version"
