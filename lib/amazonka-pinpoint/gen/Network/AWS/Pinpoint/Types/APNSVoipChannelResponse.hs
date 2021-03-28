{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.APNSVoipChannelResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Pinpoint.Types.APNSVoipChannelResponse
  ( APNSVoipChannelResponse (..)
  -- * Smart constructor
  , mkAPNSVoipChannelResponse
  -- * Lenses
  , apnsvcrPlatform
  , apnsvcrApplicationId
  , apnsvcrCreationDate
  , apnsvcrDefaultAuthenticationMethod
  , apnsvcrEnabled
  , apnsvcrHasCredential
  , apnsvcrHasTokenKey
  , apnsvcrId
  , apnsvcrIsArchived
  , apnsvcrLastModifiedBy
  , apnsvcrLastModifiedDate
  , apnsvcrVersion
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Provides information about the status and settings of the APNs (Apple Push Notification service) VoIP channel for an application.
--
-- /See:/ 'mkAPNSVoipChannelResponse' smart constructor.
data APNSVoipChannelResponse = APNSVoipChannelResponse'
  { platform :: Core.Text
    -- ^ The type of messaging or notification platform for the channel. For the APNs VoIP channel, this value is APNS_VOIP.
  , applicationId :: Core.Maybe Core.Text
    -- ^ The unique identifier for the application that the APNs VoIP channel applies to.
  , creationDate :: Core.Maybe Core.Text
    -- ^ The date and time when the APNs VoIP channel was enabled.
  , defaultAuthenticationMethod :: Core.Maybe Core.Text
    -- ^ The default authentication method that Amazon Pinpoint uses to authenticate with APNs for this channel, key or certificate.
  , enabled :: Core.Maybe Core.Bool
    -- ^ Specifies whether the APNs VoIP channel is enabled for the application.
  , hasCredential :: Core.Maybe Core.Bool
    -- ^ (Not used) This property is retained only for backward compatibility.
  , hasTokenKey :: Core.Maybe Core.Bool
    -- ^ Specifies whether the APNs VoIP channel is configured to communicate with APNs by using APNs tokens. To provide an authentication key for APNs tokens, set the TokenKey property of the channel.
  , id :: Core.Maybe Core.Text
    -- ^ (Deprecated) An identifier for the APNs VoIP channel. This property is retained only for backward compatibility.
  , isArchived :: Core.Maybe Core.Bool
    -- ^ Specifies whether the APNs VoIP channel is archived.
  , lastModifiedBy :: Core.Maybe Core.Text
    -- ^ The user who last modified the APNs VoIP channel.
  , lastModifiedDate :: Core.Maybe Core.Text
    -- ^ The date and time when the APNs VoIP channel was last modified.
  , version :: Core.Maybe Core.Int
    -- ^ The current version of the APNs VoIP channel.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'APNSVoipChannelResponse' value with any optional fields omitted.
mkAPNSVoipChannelResponse
    :: Core.Text -- ^ 'platform'
    -> APNSVoipChannelResponse
mkAPNSVoipChannelResponse platform
  = APNSVoipChannelResponse'{platform, applicationId = Core.Nothing,
                             creationDate = Core.Nothing,
                             defaultAuthenticationMethod = Core.Nothing, enabled = Core.Nothing,
                             hasCredential = Core.Nothing, hasTokenKey = Core.Nothing,
                             id = Core.Nothing, isArchived = Core.Nothing,
                             lastModifiedBy = Core.Nothing, lastModifiedDate = Core.Nothing,
                             version = Core.Nothing}

-- | The type of messaging or notification platform for the channel. For the APNs VoIP channel, this value is APNS_VOIP.
--
-- /Note:/ Consider using 'platform' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apnsvcrPlatform :: Lens.Lens' APNSVoipChannelResponse Core.Text
apnsvcrPlatform = Lens.field @"platform"
{-# INLINEABLE apnsvcrPlatform #-}
{-# DEPRECATED platform "Use generic-lens or generic-optics with 'platform' instead"  #-}

-- | The unique identifier for the application that the APNs VoIP channel applies to.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apnsvcrApplicationId :: Lens.Lens' APNSVoipChannelResponse (Core.Maybe Core.Text)
apnsvcrApplicationId = Lens.field @"applicationId"
{-# INLINEABLE apnsvcrApplicationId #-}
{-# DEPRECATED applicationId "Use generic-lens or generic-optics with 'applicationId' instead"  #-}

-- | The date and time when the APNs VoIP channel was enabled.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apnsvcrCreationDate :: Lens.Lens' APNSVoipChannelResponse (Core.Maybe Core.Text)
apnsvcrCreationDate = Lens.field @"creationDate"
{-# INLINEABLE apnsvcrCreationDate #-}
{-# DEPRECATED creationDate "Use generic-lens or generic-optics with 'creationDate' instead"  #-}

-- | The default authentication method that Amazon Pinpoint uses to authenticate with APNs for this channel, key or certificate.
--
-- /Note:/ Consider using 'defaultAuthenticationMethod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apnsvcrDefaultAuthenticationMethod :: Lens.Lens' APNSVoipChannelResponse (Core.Maybe Core.Text)
apnsvcrDefaultAuthenticationMethod = Lens.field @"defaultAuthenticationMethod"
{-# INLINEABLE apnsvcrDefaultAuthenticationMethod #-}
{-# DEPRECATED defaultAuthenticationMethod "Use generic-lens or generic-optics with 'defaultAuthenticationMethod' instead"  #-}

-- | Specifies whether the APNs VoIP channel is enabled for the application.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apnsvcrEnabled :: Lens.Lens' APNSVoipChannelResponse (Core.Maybe Core.Bool)
apnsvcrEnabled = Lens.field @"enabled"
{-# INLINEABLE apnsvcrEnabled #-}
{-# DEPRECATED enabled "Use generic-lens or generic-optics with 'enabled' instead"  #-}

-- | (Not used) This property is retained only for backward compatibility.
--
-- /Note:/ Consider using 'hasCredential' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apnsvcrHasCredential :: Lens.Lens' APNSVoipChannelResponse (Core.Maybe Core.Bool)
apnsvcrHasCredential = Lens.field @"hasCredential"
{-# INLINEABLE apnsvcrHasCredential #-}
{-# DEPRECATED hasCredential "Use generic-lens or generic-optics with 'hasCredential' instead"  #-}

-- | Specifies whether the APNs VoIP channel is configured to communicate with APNs by using APNs tokens. To provide an authentication key for APNs tokens, set the TokenKey property of the channel.
--
-- /Note:/ Consider using 'hasTokenKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apnsvcrHasTokenKey :: Lens.Lens' APNSVoipChannelResponse (Core.Maybe Core.Bool)
apnsvcrHasTokenKey = Lens.field @"hasTokenKey"
{-# INLINEABLE apnsvcrHasTokenKey #-}
{-# DEPRECATED hasTokenKey "Use generic-lens or generic-optics with 'hasTokenKey' instead"  #-}

-- | (Deprecated) An identifier for the APNs VoIP channel. This property is retained only for backward compatibility.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apnsvcrId :: Lens.Lens' APNSVoipChannelResponse (Core.Maybe Core.Text)
apnsvcrId = Lens.field @"id"
{-# INLINEABLE apnsvcrId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | Specifies whether the APNs VoIP channel is archived.
--
-- /Note:/ Consider using 'isArchived' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apnsvcrIsArchived :: Lens.Lens' APNSVoipChannelResponse (Core.Maybe Core.Bool)
apnsvcrIsArchived = Lens.field @"isArchived"
{-# INLINEABLE apnsvcrIsArchived #-}
{-# DEPRECATED isArchived "Use generic-lens or generic-optics with 'isArchived' instead"  #-}

-- | The user who last modified the APNs VoIP channel.
--
-- /Note:/ Consider using 'lastModifiedBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apnsvcrLastModifiedBy :: Lens.Lens' APNSVoipChannelResponse (Core.Maybe Core.Text)
apnsvcrLastModifiedBy = Lens.field @"lastModifiedBy"
{-# INLINEABLE apnsvcrLastModifiedBy #-}
{-# DEPRECATED lastModifiedBy "Use generic-lens or generic-optics with 'lastModifiedBy' instead"  #-}

-- | The date and time when the APNs VoIP channel was last modified.
--
-- /Note:/ Consider using 'lastModifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apnsvcrLastModifiedDate :: Lens.Lens' APNSVoipChannelResponse (Core.Maybe Core.Text)
apnsvcrLastModifiedDate = Lens.field @"lastModifiedDate"
{-# INLINEABLE apnsvcrLastModifiedDate #-}
{-# DEPRECATED lastModifiedDate "Use generic-lens or generic-optics with 'lastModifiedDate' instead"  #-}

-- | The current version of the APNs VoIP channel.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apnsvcrVersion :: Lens.Lens' APNSVoipChannelResponse (Core.Maybe Core.Int)
apnsvcrVersion = Lens.field @"version"
{-# INLINEABLE apnsvcrVersion #-}
{-# DEPRECATED version "Use generic-lens or generic-optics with 'version' instead"  #-}

instance Core.FromJSON APNSVoipChannelResponse where
        parseJSON
          = Core.withObject "APNSVoipChannelResponse" Core.$
              \ x ->
                APNSVoipChannelResponse' Core.<$>
                  (x Core..: "Platform") Core.<*> x Core..:? "ApplicationId" Core.<*>
                    x Core..:? "CreationDate"
                    Core.<*> x Core..:? "DefaultAuthenticationMethod"
                    Core.<*> x Core..:? "Enabled"
                    Core.<*> x Core..:? "HasCredential"
                    Core.<*> x Core..:? "HasTokenKey"
                    Core.<*> x Core..:? "Id"
                    Core.<*> x Core..:? "IsArchived"
                    Core.<*> x Core..:? "LastModifiedBy"
                    Core.<*> x Core..:? "LastModifiedDate"
                    Core.<*> x Core..:? "Version"
