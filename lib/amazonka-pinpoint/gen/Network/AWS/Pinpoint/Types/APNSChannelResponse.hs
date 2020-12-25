{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.APNSChannelResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.APNSChannelResponse
  ( APNSChannelResponse (..),

    -- * Smart constructor
    mkAPNSChannelResponse,

    -- * Lenses
    apnscrPlatform,
    apnscrApplicationId,
    apnscrCreationDate,
    apnscrDefaultAuthenticationMethod,
    apnscrEnabled,
    apnscrHasCredential,
    apnscrHasTokenKey,
    apnscrId,
    apnscrIsArchived,
    apnscrLastModifiedBy,
    apnscrLastModifiedDate,
    apnscrVersion,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Provides information about the status and settings of the APNs (Apple Push Notification service) channel for an application.
--
-- /See:/ 'mkAPNSChannelResponse' smart constructor.
data APNSChannelResponse = APNSChannelResponse'
  { -- | The type of messaging or notification platform for the channel. For the APNs channel, this value is APNS.
    platform :: Core.Text,
    -- | The unique identifier for the application that the APNs channel applies to.
    applicationId :: Core.Maybe Core.Text,
    -- | The date and time when the APNs channel was enabled.
    creationDate :: Core.Maybe Core.Text,
    -- | The default authentication method that Amazon Pinpoint uses to authenticate with APNs for this channel, key or certificate.
    defaultAuthenticationMethod :: Core.Maybe Core.Text,
    -- | Specifies whether the APNs channel is enabled for the application.
    enabled :: Core.Maybe Core.Bool,
    -- | (Not used) This property is retained only for backward compatibility.
    hasCredential :: Core.Maybe Core.Bool,
    -- | Specifies whether the APNs channel is configured to communicate with APNs by using APNs tokens. To provide an authentication key for APNs tokens, set the TokenKey property of the channel.
    hasTokenKey :: Core.Maybe Core.Bool,
    -- | (Deprecated) An identifier for the APNs channel. This property is retained only for backward compatibility.
    id :: Core.Maybe Core.Text,
    -- | Specifies whether the APNs channel is archived.
    isArchived :: Core.Maybe Core.Bool,
    -- | The user who last modified the APNs channel.
    lastModifiedBy :: Core.Maybe Core.Text,
    -- | The date and time when the APNs channel was last modified.
    lastModifiedDate :: Core.Maybe Core.Text,
    -- | The current version of the APNs channel.
    version :: Core.Maybe Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'APNSChannelResponse' value with any optional fields omitted.
mkAPNSChannelResponse ::
  -- | 'platform'
  Core.Text ->
  APNSChannelResponse
mkAPNSChannelResponse platform =
  APNSChannelResponse'
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

-- | The type of messaging or notification platform for the channel. For the APNs channel, this value is APNS.
--
-- /Note:/ Consider using 'platform' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apnscrPlatform :: Lens.Lens' APNSChannelResponse Core.Text
apnscrPlatform = Lens.field @"platform"
{-# DEPRECATED apnscrPlatform "Use generic-lens or generic-optics with 'platform' instead." #-}

-- | The unique identifier for the application that the APNs channel applies to.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apnscrApplicationId :: Lens.Lens' APNSChannelResponse (Core.Maybe Core.Text)
apnscrApplicationId = Lens.field @"applicationId"
{-# DEPRECATED apnscrApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

-- | The date and time when the APNs channel was enabled.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apnscrCreationDate :: Lens.Lens' APNSChannelResponse (Core.Maybe Core.Text)
apnscrCreationDate = Lens.field @"creationDate"
{-# DEPRECATED apnscrCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

-- | The default authentication method that Amazon Pinpoint uses to authenticate with APNs for this channel, key or certificate.
--
-- /Note:/ Consider using 'defaultAuthenticationMethod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apnscrDefaultAuthenticationMethod :: Lens.Lens' APNSChannelResponse (Core.Maybe Core.Text)
apnscrDefaultAuthenticationMethod = Lens.field @"defaultAuthenticationMethod"
{-# DEPRECATED apnscrDefaultAuthenticationMethod "Use generic-lens or generic-optics with 'defaultAuthenticationMethod' instead." #-}

-- | Specifies whether the APNs channel is enabled for the application.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apnscrEnabled :: Lens.Lens' APNSChannelResponse (Core.Maybe Core.Bool)
apnscrEnabled = Lens.field @"enabled"
{-# DEPRECATED apnscrEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

-- | (Not used) This property is retained only for backward compatibility.
--
-- /Note:/ Consider using 'hasCredential' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apnscrHasCredential :: Lens.Lens' APNSChannelResponse (Core.Maybe Core.Bool)
apnscrHasCredential = Lens.field @"hasCredential"
{-# DEPRECATED apnscrHasCredential "Use generic-lens or generic-optics with 'hasCredential' instead." #-}

-- | Specifies whether the APNs channel is configured to communicate with APNs by using APNs tokens. To provide an authentication key for APNs tokens, set the TokenKey property of the channel.
--
-- /Note:/ Consider using 'hasTokenKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apnscrHasTokenKey :: Lens.Lens' APNSChannelResponse (Core.Maybe Core.Bool)
apnscrHasTokenKey = Lens.field @"hasTokenKey"
{-# DEPRECATED apnscrHasTokenKey "Use generic-lens or generic-optics with 'hasTokenKey' instead." #-}

-- | (Deprecated) An identifier for the APNs channel. This property is retained only for backward compatibility.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apnscrId :: Lens.Lens' APNSChannelResponse (Core.Maybe Core.Text)
apnscrId = Lens.field @"id"
{-# DEPRECATED apnscrId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | Specifies whether the APNs channel is archived.
--
-- /Note:/ Consider using 'isArchived' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apnscrIsArchived :: Lens.Lens' APNSChannelResponse (Core.Maybe Core.Bool)
apnscrIsArchived = Lens.field @"isArchived"
{-# DEPRECATED apnscrIsArchived "Use generic-lens or generic-optics with 'isArchived' instead." #-}

-- | The user who last modified the APNs channel.
--
-- /Note:/ Consider using 'lastModifiedBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apnscrLastModifiedBy :: Lens.Lens' APNSChannelResponse (Core.Maybe Core.Text)
apnscrLastModifiedBy = Lens.field @"lastModifiedBy"
{-# DEPRECATED apnscrLastModifiedBy "Use generic-lens or generic-optics with 'lastModifiedBy' instead." #-}

-- | The date and time when the APNs channel was last modified.
--
-- /Note:/ Consider using 'lastModifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apnscrLastModifiedDate :: Lens.Lens' APNSChannelResponse (Core.Maybe Core.Text)
apnscrLastModifiedDate = Lens.field @"lastModifiedDate"
{-# DEPRECATED apnscrLastModifiedDate "Use generic-lens or generic-optics with 'lastModifiedDate' instead." #-}

-- | The current version of the APNs channel.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apnscrVersion :: Lens.Lens' APNSChannelResponse (Core.Maybe Core.Int)
apnscrVersion = Lens.field @"version"
{-# DEPRECATED apnscrVersion "Use generic-lens or generic-optics with 'version' instead." #-}

instance Core.FromJSON APNSChannelResponse where
  parseJSON =
    Core.withObject "APNSChannelResponse" Core.$
      \x ->
        APNSChannelResponse'
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
