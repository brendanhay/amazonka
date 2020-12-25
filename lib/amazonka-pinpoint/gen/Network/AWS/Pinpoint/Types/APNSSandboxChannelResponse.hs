{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.APNSSandboxChannelResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.APNSSandboxChannelResponse
  ( APNSSandboxChannelResponse (..),

    -- * Smart constructor
    mkAPNSSandboxChannelResponse,

    -- * Lenses
    apnsscrPlatform,
    apnsscrApplicationId,
    apnsscrCreationDate,
    apnsscrDefaultAuthenticationMethod,
    apnsscrEnabled,
    apnsscrHasCredential,
    apnsscrHasTokenKey,
    apnsscrId,
    apnsscrIsArchived,
    apnsscrLastModifiedBy,
    apnsscrLastModifiedDate,
    apnsscrVersion,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Provides information about the status and settings of the APNs (Apple Push Notification service) sandbox channel for an application.
--
-- /See:/ 'mkAPNSSandboxChannelResponse' smart constructor.
data APNSSandboxChannelResponse = APNSSandboxChannelResponse'
  { -- | The type of messaging or notification platform for the channel. For the APNs sandbox channel, this value is APNS_SANDBOX.
    platform :: Core.Text,
    -- | The unique identifier for the application that the APNs sandbox channel applies to.
    applicationId :: Core.Maybe Core.Text,
    -- | The date and time when the APNs sandbox channel was enabled.
    creationDate :: Core.Maybe Core.Text,
    -- | The default authentication method that Amazon Pinpoint uses to authenticate with the APNs sandbox environment for this channel, key or certificate.
    defaultAuthenticationMethod :: Core.Maybe Core.Text,
    -- | Specifies whether the APNs sandbox channel is enabled for the application.
    enabled :: Core.Maybe Core.Bool,
    -- | (Not used) This property is retained only for backward compatibility.
    hasCredential :: Core.Maybe Core.Bool,
    -- | Specifies whether the APNs sandbox channel is configured to communicate with APNs by using APNs tokens. To provide an authentication key for APNs tokens, set the TokenKey property of the channel.
    hasTokenKey :: Core.Maybe Core.Bool,
    -- | (Deprecated) An identifier for the APNs sandbox channel. This property is retained only for backward compatibility.
    id :: Core.Maybe Core.Text,
    -- | Specifies whether the APNs sandbox channel is archived.
    isArchived :: Core.Maybe Core.Bool,
    -- | The user who last modified the APNs sandbox channel.
    lastModifiedBy :: Core.Maybe Core.Text,
    -- | The date and time when the APNs sandbox channel was last modified.
    lastModifiedDate :: Core.Maybe Core.Text,
    -- | The current version of the APNs sandbox channel.
    version :: Core.Maybe Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'APNSSandboxChannelResponse' value with any optional fields omitted.
mkAPNSSandboxChannelResponse ::
  -- | 'platform'
  Core.Text ->
  APNSSandboxChannelResponse
mkAPNSSandboxChannelResponse platform =
  APNSSandboxChannelResponse'
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

-- | The type of messaging or notification platform for the channel. For the APNs sandbox channel, this value is APNS_SANDBOX.
--
-- /Note:/ Consider using 'platform' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apnsscrPlatform :: Lens.Lens' APNSSandboxChannelResponse Core.Text
apnsscrPlatform = Lens.field @"platform"
{-# DEPRECATED apnsscrPlatform "Use generic-lens or generic-optics with 'platform' instead." #-}

-- | The unique identifier for the application that the APNs sandbox channel applies to.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apnsscrApplicationId :: Lens.Lens' APNSSandboxChannelResponse (Core.Maybe Core.Text)
apnsscrApplicationId = Lens.field @"applicationId"
{-# DEPRECATED apnsscrApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

-- | The date and time when the APNs sandbox channel was enabled.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apnsscrCreationDate :: Lens.Lens' APNSSandboxChannelResponse (Core.Maybe Core.Text)
apnsscrCreationDate = Lens.field @"creationDate"
{-# DEPRECATED apnsscrCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

-- | The default authentication method that Amazon Pinpoint uses to authenticate with the APNs sandbox environment for this channel, key or certificate.
--
-- /Note:/ Consider using 'defaultAuthenticationMethod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apnsscrDefaultAuthenticationMethod :: Lens.Lens' APNSSandboxChannelResponse (Core.Maybe Core.Text)
apnsscrDefaultAuthenticationMethod = Lens.field @"defaultAuthenticationMethod"
{-# DEPRECATED apnsscrDefaultAuthenticationMethod "Use generic-lens or generic-optics with 'defaultAuthenticationMethod' instead." #-}

-- | Specifies whether the APNs sandbox channel is enabled for the application.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apnsscrEnabled :: Lens.Lens' APNSSandboxChannelResponse (Core.Maybe Core.Bool)
apnsscrEnabled = Lens.field @"enabled"
{-# DEPRECATED apnsscrEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

-- | (Not used) This property is retained only for backward compatibility.
--
-- /Note:/ Consider using 'hasCredential' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apnsscrHasCredential :: Lens.Lens' APNSSandboxChannelResponse (Core.Maybe Core.Bool)
apnsscrHasCredential = Lens.field @"hasCredential"
{-# DEPRECATED apnsscrHasCredential "Use generic-lens or generic-optics with 'hasCredential' instead." #-}

-- | Specifies whether the APNs sandbox channel is configured to communicate with APNs by using APNs tokens. To provide an authentication key for APNs tokens, set the TokenKey property of the channel.
--
-- /Note:/ Consider using 'hasTokenKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apnsscrHasTokenKey :: Lens.Lens' APNSSandboxChannelResponse (Core.Maybe Core.Bool)
apnsscrHasTokenKey = Lens.field @"hasTokenKey"
{-# DEPRECATED apnsscrHasTokenKey "Use generic-lens or generic-optics with 'hasTokenKey' instead." #-}

-- | (Deprecated) An identifier for the APNs sandbox channel. This property is retained only for backward compatibility.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apnsscrId :: Lens.Lens' APNSSandboxChannelResponse (Core.Maybe Core.Text)
apnsscrId = Lens.field @"id"
{-# DEPRECATED apnsscrId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | Specifies whether the APNs sandbox channel is archived.
--
-- /Note:/ Consider using 'isArchived' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apnsscrIsArchived :: Lens.Lens' APNSSandboxChannelResponse (Core.Maybe Core.Bool)
apnsscrIsArchived = Lens.field @"isArchived"
{-# DEPRECATED apnsscrIsArchived "Use generic-lens or generic-optics with 'isArchived' instead." #-}

-- | The user who last modified the APNs sandbox channel.
--
-- /Note:/ Consider using 'lastModifiedBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apnsscrLastModifiedBy :: Lens.Lens' APNSSandboxChannelResponse (Core.Maybe Core.Text)
apnsscrLastModifiedBy = Lens.field @"lastModifiedBy"
{-# DEPRECATED apnsscrLastModifiedBy "Use generic-lens or generic-optics with 'lastModifiedBy' instead." #-}

-- | The date and time when the APNs sandbox channel was last modified.
--
-- /Note:/ Consider using 'lastModifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apnsscrLastModifiedDate :: Lens.Lens' APNSSandboxChannelResponse (Core.Maybe Core.Text)
apnsscrLastModifiedDate = Lens.field @"lastModifiedDate"
{-# DEPRECATED apnsscrLastModifiedDate "Use generic-lens or generic-optics with 'lastModifiedDate' instead." #-}

-- | The current version of the APNs sandbox channel.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apnsscrVersion :: Lens.Lens' APNSSandboxChannelResponse (Core.Maybe Core.Int)
apnsscrVersion = Lens.field @"version"
{-# DEPRECATED apnsscrVersion "Use generic-lens or generic-optics with 'version' instead." #-}

instance Core.FromJSON APNSSandboxChannelResponse where
  parseJSON =
    Core.withObject "APNSSandboxChannelResponse" Core.$
      \x ->
        APNSSandboxChannelResponse'
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
