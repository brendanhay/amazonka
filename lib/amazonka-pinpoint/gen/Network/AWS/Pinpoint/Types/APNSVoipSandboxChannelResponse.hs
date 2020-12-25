{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.APNSVoipSandboxChannelResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.APNSVoipSandboxChannelResponse
  ( APNSVoipSandboxChannelResponse (..),

    -- * Smart constructor
    mkAPNSVoipSandboxChannelResponse,

    -- * Lenses
    apnsvscrPlatform,
    apnsvscrApplicationId,
    apnsvscrCreationDate,
    apnsvscrDefaultAuthenticationMethod,
    apnsvscrEnabled,
    apnsvscrHasCredential,
    apnsvscrHasTokenKey,
    apnsvscrId,
    apnsvscrIsArchived,
    apnsvscrLastModifiedBy,
    apnsvscrLastModifiedDate,
    apnsvscrVersion,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Provides information about the status and settings of the APNs (Apple Push Notification service) VoIP sandbox channel for an application.
--
-- /See:/ 'mkAPNSVoipSandboxChannelResponse' smart constructor.
data APNSVoipSandboxChannelResponse = APNSVoipSandboxChannelResponse'
  { -- | The type of messaging or notification platform for the channel. For the APNs VoIP sandbox channel, this value is APNS_VOIP_SANDBOX.
    platform :: Core.Text,
    -- | The unique identifier for the application that the APNs VoIP sandbox channel applies to.
    applicationId :: Core.Maybe Core.Text,
    -- | The date and time when the APNs VoIP sandbox channel was enabled.
    creationDate :: Core.Maybe Core.Text,
    -- | The default authentication method that Amazon Pinpoint uses to authenticate with the APNs sandbox environment for this channel, key or certificate.
    defaultAuthenticationMethod :: Core.Maybe Core.Text,
    -- | Specifies whether the APNs VoIP sandbox channel is enabled for the application.
    enabled :: Core.Maybe Core.Bool,
    -- | (Not used) This property is retained only for backward compatibility.
    hasCredential :: Core.Maybe Core.Bool,
    -- | Specifies whether the APNs VoIP sandbox channel is configured to communicate with APNs by using APNs tokens. To provide an authentication key for APNs tokens, set the TokenKey property of the channel.
    hasTokenKey :: Core.Maybe Core.Bool,
    -- | (Deprecated) An identifier for the APNs VoIP sandbox channel. This property is retained only for backward compatibility.
    id :: Core.Maybe Core.Text,
    -- | Specifies whether the APNs VoIP sandbox channel is archived.
    isArchived :: Core.Maybe Core.Bool,
    -- | The user who last modified the APNs VoIP sandbox channel.
    lastModifiedBy :: Core.Maybe Core.Text,
    -- | The date and time when the APNs VoIP sandbox channel was last modified.
    lastModifiedDate :: Core.Maybe Core.Text,
    -- | The current version of the APNs VoIP sandbox channel.
    version :: Core.Maybe Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'APNSVoipSandboxChannelResponse' value with any optional fields omitted.
mkAPNSVoipSandboxChannelResponse ::
  -- | 'platform'
  Core.Text ->
  APNSVoipSandboxChannelResponse
mkAPNSVoipSandboxChannelResponse platform =
  APNSVoipSandboxChannelResponse'
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

-- | The type of messaging or notification platform for the channel. For the APNs VoIP sandbox channel, this value is APNS_VOIP_SANDBOX.
--
-- /Note:/ Consider using 'platform' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apnsvscrPlatform :: Lens.Lens' APNSVoipSandboxChannelResponse Core.Text
apnsvscrPlatform = Lens.field @"platform"
{-# DEPRECATED apnsvscrPlatform "Use generic-lens or generic-optics with 'platform' instead." #-}

-- | The unique identifier for the application that the APNs VoIP sandbox channel applies to.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apnsvscrApplicationId :: Lens.Lens' APNSVoipSandboxChannelResponse (Core.Maybe Core.Text)
apnsvscrApplicationId = Lens.field @"applicationId"
{-# DEPRECATED apnsvscrApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

-- | The date and time when the APNs VoIP sandbox channel was enabled.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apnsvscrCreationDate :: Lens.Lens' APNSVoipSandboxChannelResponse (Core.Maybe Core.Text)
apnsvscrCreationDate = Lens.field @"creationDate"
{-# DEPRECATED apnsvscrCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

-- | The default authentication method that Amazon Pinpoint uses to authenticate with the APNs sandbox environment for this channel, key or certificate.
--
-- /Note:/ Consider using 'defaultAuthenticationMethod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apnsvscrDefaultAuthenticationMethod :: Lens.Lens' APNSVoipSandboxChannelResponse (Core.Maybe Core.Text)
apnsvscrDefaultAuthenticationMethod = Lens.field @"defaultAuthenticationMethod"
{-# DEPRECATED apnsvscrDefaultAuthenticationMethod "Use generic-lens or generic-optics with 'defaultAuthenticationMethod' instead." #-}

-- | Specifies whether the APNs VoIP sandbox channel is enabled for the application.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apnsvscrEnabled :: Lens.Lens' APNSVoipSandboxChannelResponse (Core.Maybe Core.Bool)
apnsvscrEnabled = Lens.field @"enabled"
{-# DEPRECATED apnsvscrEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

-- | (Not used) This property is retained only for backward compatibility.
--
-- /Note:/ Consider using 'hasCredential' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apnsvscrHasCredential :: Lens.Lens' APNSVoipSandboxChannelResponse (Core.Maybe Core.Bool)
apnsvscrHasCredential = Lens.field @"hasCredential"
{-# DEPRECATED apnsvscrHasCredential "Use generic-lens or generic-optics with 'hasCredential' instead." #-}

-- | Specifies whether the APNs VoIP sandbox channel is configured to communicate with APNs by using APNs tokens. To provide an authentication key for APNs tokens, set the TokenKey property of the channel.
--
-- /Note:/ Consider using 'hasTokenKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apnsvscrHasTokenKey :: Lens.Lens' APNSVoipSandboxChannelResponse (Core.Maybe Core.Bool)
apnsvscrHasTokenKey = Lens.field @"hasTokenKey"
{-# DEPRECATED apnsvscrHasTokenKey "Use generic-lens or generic-optics with 'hasTokenKey' instead." #-}

-- | (Deprecated) An identifier for the APNs VoIP sandbox channel. This property is retained only for backward compatibility.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apnsvscrId :: Lens.Lens' APNSVoipSandboxChannelResponse (Core.Maybe Core.Text)
apnsvscrId = Lens.field @"id"
{-# DEPRECATED apnsvscrId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | Specifies whether the APNs VoIP sandbox channel is archived.
--
-- /Note:/ Consider using 'isArchived' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apnsvscrIsArchived :: Lens.Lens' APNSVoipSandboxChannelResponse (Core.Maybe Core.Bool)
apnsvscrIsArchived = Lens.field @"isArchived"
{-# DEPRECATED apnsvscrIsArchived "Use generic-lens or generic-optics with 'isArchived' instead." #-}

-- | The user who last modified the APNs VoIP sandbox channel.
--
-- /Note:/ Consider using 'lastModifiedBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apnsvscrLastModifiedBy :: Lens.Lens' APNSVoipSandboxChannelResponse (Core.Maybe Core.Text)
apnsvscrLastModifiedBy = Lens.field @"lastModifiedBy"
{-# DEPRECATED apnsvscrLastModifiedBy "Use generic-lens or generic-optics with 'lastModifiedBy' instead." #-}

-- | The date and time when the APNs VoIP sandbox channel was last modified.
--
-- /Note:/ Consider using 'lastModifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apnsvscrLastModifiedDate :: Lens.Lens' APNSVoipSandboxChannelResponse (Core.Maybe Core.Text)
apnsvscrLastModifiedDate = Lens.field @"lastModifiedDate"
{-# DEPRECATED apnsvscrLastModifiedDate "Use generic-lens or generic-optics with 'lastModifiedDate' instead." #-}

-- | The current version of the APNs VoIP sandbox channel.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apnsvscrVersion :: Lens.Lens' APNSVoipSandboxChannelResponse (Core.Maybe Core.Int)
apnsvscrVersion = Lens.field @"version"
{-# DEPRECATED apnsvscrVersion "Use generic-lens or generic-optics with 'version' instead." #-}

instance Core.FromJSON APNSVoipSandboxChannelResponse where
  parseJSON =
    Core.withObject "APNSVoipSandboxChannelResponse" Core.$
      \x ->
        APNSVoipSandboxChannelResponse'
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
