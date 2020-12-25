{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.APNSVoipChannelResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.APNSVoipChannelResponse
  ( APNSVoipChannelResponse (..),

    -- * Smart constructor
    mkAPNSVoipChannelResponse,

    -- * Lenses
    apnsvcrPlatform,
    apnsvcrApplicationId,
    apnsvcrCreationDate,
    apnsvcrDefaultAuthenticationMethod,
    apnsvcrEnabled,
    apnsvcrHasCredential,
    apnsvcrHasTokenKey,
    apnsvcrId,
    apnsvcrIsArchived,
    apnsvcrLastModifiedBy,
    apnsvcrLastModifiedDate,
    apnsvcrVersion,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Provides information about the status and settings of the APNs (Apple Push Notification service) VoIP channel for an application.
--
-- /See:/ 'mkAPNSVoipChannelResponse' smart constructor.
data APNSVoipChannelResponse = APNSVoipChannelResponse'
  { -- | The type of messaging or notification platform for the channel. For the APNs VoIP channel, this value is APNS_VOIP.
    platform :: Core.Text,
    -- | The unique identifier for the application that the APNs VoIP channel applies to.
    applicationId :: Core.Maybe Core.Text,
    -- | The date and time when the APNs VoIP channel was enabled.
    creationDate :: Core.Maybe Core.Text,
    -- | The default authentication method that Amazon Pinpoint uses to authenticate with APNs for this channel, key or certificate.
    defaultAuthenticationMethod :: Core.Maybe Core.Text,
    -- | Specifies whether the APNs VoIP channel is enabled for the application.
    enabled :: Core.Maybe Core.Bool,
    -- | (Not used) This property is retained only for backward compatibility.
    hasCredential :: Core.Maybe Core.Bool,
    -- | Specifies whether the APNs VoIP channel is configured to communicate with APNs by using APNs tokens. To provide an authentication key for APNs tokens, set the TokenKey property of the channel.
    hasTokenKey :: Core.Maybe Core.Bool,
    -- | (Deprecated) An identifier for the APNs VoIP channel. This property is retained only for backward compatibility.
    id :: Core.Maybe Core.Text,
    -- | Specifies whether the APNs VoIP channel is archived.
    isArchived :: Core.Maybe Core.Bool,
    -- | The user who last modified the APNs VoIP channel.
    lastModifiedBy :: Core.Maybe Core.Text,
    -- | The date and time when the APNs VoIP channel was last modified.
    lastModifiedDate :: Core.Maybe Core.Text,
    -- | The current version of the APNs VoIP channel.
    version :: Core.Maybe Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'APNSVoipChannelResponse' value with any optional fields omitted.
mkAPNSVoipChannelResponse ::
  -- | 'platform'
  Core.Text ->
  APNSVoipChannelResponse
mkAPNSVoipChannelResponse platform =
  APNSVoipChannelResponse'
    { platform,
      applicationId = Core.Nothing,
      creationDate = Core.Nothing,
      defaultAuthenticationMethod = Core.Nothing,
      enabled = Core.Nothing,
      hasCredential = Core.Nothing,
      hasTokenKey = Core.Nothing,
      id = Core.Nothing,
      isArchived = Core.Nothing,
      lastModifiedBy = Core.Nothing,
      lastModifiedDate = Core.Nothing,
      version = Core.Nothing
    }

-- | The type of messaging or notification platform for the channel. For the APNs VoIP channel, this value is APNS_VOIP.
--
-- /Note:/ Consider using 'platform' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apnsvcrPlatform :: Lens.Lens' APNSVoipChannelResponse Core.Text
apnsvcrPlatform = Lens.field @"platform"
{-# DEPRECATED apnsvcrPlatform "Use generic-lens or generic-optics with 'platform' instead." #-}

-- | The unique identifier for the application that the APNs VoIP channel applies to.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apnsvcrApplicationId :: Lens.Lens' APNSVoipChannelResponse (Core.Maybe Core.Text)
apnsvcrApplicationId = Lens.field @"applicationId"
{-# DEPRECATED apnsvcrApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

-- | The date and time when the APNs VoIP channel was enabled.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apnsvcrCreationDate :: Lens.Lens' APNSVoipChannelResponse (Core.Maybe Core.Text)
apnsvcrCreationDate = Lens.field @"creationDate"
{-# DEPRECATED apnsvcrCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

-- | The default authentication method that Amazon Pinpoint uses to authenticate with APNs for this channel, key or certificate.
--
-- /Note:/ Consider using 'defaultAuthenticationMethod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apnsvcrDefaultAuthenticationMethod :: Lens.Lens' APNSVoipChannelResponse (Core.Maybe Core.Text)
apnsvcrDefaultAuthenticationMethod = Lens.field @"defaultAuthenticationMethod"
{-# DEPRECATED apnsvcrDefaultAuthenticationMethod "Use generic-lens or generic-optics with 'defaultAuthenticationMethod' instead." #-}

-- | Specifies whether the APNs VoIP channel is enabled for the application.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apnsvcrEnabled :: Lens.Lens' APNSVoipChannelResponse (Core.Maybe Core.Bool)
apnsvcrEnabled = Lens.field @"enabled"
{-# DEPRECATED apnsvcrEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

-- | (Not used) This property is retained only for backward compatibility.
--
-- /Note:/ Consider using 'hasCredential' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apnsvcrHasCredential :: Lens.Lens' APNSVoipChannelResponse (Core.Maybe Core.Bool)
apnsvcrHasCredential = Lens.field @"hasCredential"
{-# DEPRECATED apnsvcrHasCredential "Use generic-lens or generic-optics with 'hasCredential' instead." #-}

-- | Specifies whether the APNs VoIP channel is configured to communicate with APNs by using APNs tokens. To provide an authentication key for APNs tokens, set the TokenKey property of the channel.
--
-- /Note:/ Consider using 'hasTokenKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apnsvcrHasTokenKey :: Lens.Lens' APNSVoipChannelResponse (Core.Maybe Core.Bool)
apnsvcrHasTokenKey = Lens.field @"hasTokenKey"
{-# DEPRECATED apnsvcrHasTokenKey "Use generic-lens or generic-optics with 'hasTokenKey' instead." #-}

-- | (Deprecated) An identifier for the APNs VoIP channel. This property is retained only for backward compatibility.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apnsvcrId :: Lens.Lens' APNSVoipChannelResponse (Core.Maybe Core.Text)
apnsvcrId = Lens.field @"id"
{-# DEPRECATED apnsvcrId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | Specifies whether the APNs VoIP channel is archived.
--
-- /Note:/ Consider using 'isArchived' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apnsvcrIsArchived :: Lens.Lens' APNSVoipChannelResponse (Core.Maybe Core.Bool)
apnsvcrIsArchived = Lens.field @"isArchived"
{-# DEPRECATED apnsvcrIsArchived "Use generic-lens or generic-optics with 'isArchived' instead." #-}

-- | The user who last modified the APNs VoIP channel.
--
-- /Note:/ Consider using 'lastModifiedBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apnsvcrLastModifiedBy :: Lens.Lens' APNSVoipChannelResponse (Core.Maybe Core.Text)
apnsvcrLastModifiedBy = Lens.field @"lastModifiedBy"
{-# DEPRECATED apnsvcrLastModifiedBy "Use generic-lens or generic-optics with 'lastModifiedBy' instead." #-}

-- | The date and time when the APNs VoIP channel was last modified.
--
-- /Note:/ Consider using 'lastModifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apnsvcrLastModifiedDate :: Lens.Lens' APNSVoipChannelResponse (Core.Maybe Core.Text)
apnsvcrLastModifiedDate = Lens.field @"lastModifiedDate"
{-# DEPRECATED apnsvcrLastModifiedDate "Use generic-lens or generic-optics with 'lastModifiedDate' instead." #-}

-- | The current version of the APNs VoIP channel.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apnsvcrVersion :: Lens.Lens' APNSVoipChannelResponse (Core.Maybe Core.Int)
apnsvcrVersion = Lens.field @"version"
{-# DEPRECATED apnsvcrVersion "Use generic-lens or generic-optics with 'version' instead." #-}

instance Core.FromJSON APNSVoipChannelResponse where
  parseJSON =
    Core.withObject "APNSVoipChannelResponse" Core.$
      \x ->
        APNSVoipChannelResponse'
          Core.<$> (x Core..: "Platform")
          Core.<*> (x Core..:? "ApplicationId")
          Core.<*> (x Core..:? "CreationDate")
          Core.<*> (x Core..:? "DefaultAuthenticationMethod")
          Core.<*> (x Core..:? "Enabled")
          Core.<*> (x Core..:? "HasCredential")
          Core.<*> (x Core..:? "HasTokenKey")
          Core.<*> (x Core..:? "Id")
          Core.<*> (x Core..:? "IsArchived")
          Core.<*> (x Core..:? "LastModifiedBy")
          Core.<*> (x Core..:? "LastModifiedDate")
          Core.<*> (x Core..:? "Version")
