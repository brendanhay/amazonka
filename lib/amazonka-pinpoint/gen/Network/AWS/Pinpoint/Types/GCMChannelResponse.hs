{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.GCMChannelResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Pinpoint.Types.GCMChannelResponse
  ( GCMChannelResponse (..)
  -- * Smart constructor
  , mkGCMChannelResponse
  -- * Lenses
  , gcmcrCredential
  , gcmcrPlatform
  , gcmcrApplicationId
  , gcmcrCreationDate
  , gcmcrEnabled
  , gcmcrHasCredential
  , gcmcrId
  , gcmcrIsArchived
  , gcmcrLastModifiedBy
  , gcmcrLastModifiedDate
  , gcmcrVersion
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Provides information about the status and settings of the GCM channel for an application. The GCM channel enables Amazon Pinpoint to send push notifications through the Firebase Cloud Messaging (FCM), formerly Google Cloud Messaging (GCM), service.
--
-- /See:/ 'mkGCMChannelResponse' smart constructor.
data GCMChannelResponse = GCMChannelResponse'
  { credential :: Core.Text
    -- ^ The Web API Key, also referred to as an /API_KEY/ or /server key/ , that you received from Google to communicate with Google services.
  , platform :: Core.Text
    -- ^ The type of messaging or notification platform for the channel. For the GCM channel, this value is GCM.
  , applicationId :: Core.Maybe Core.Text
    -- ^ The unique identifier for the application that the GCM channel applies to.
  , creationDate :: Core.Maybe Core.Text
    -- ^ The date and time when the GCM channel was enabled.
  , enabled :: Core.Maybe Core.Bool
    -- ^ Specifies whether the GCM channel is enabled for the application.
  , hasCredential :: Core.Maybe Core.Bool
    -- ^ (Not used) This property is retained only for backward compatibility.
  , id :: Core.Maybe Core.Text
    -- ^ (Deprecated) An identifier for the GCM channel. This property is retained only for backward compatibility.
  , isArchived :: Core.Maybe Core.Bool
    -- ^ Specifies whether the GCM channel is archived.
  , lastModifiedBy :: Core.Maybe Core.Text
    -- ^ The user who last modified the GCM channel.
  , lastModifiedDate :: Core.Maybe Core.Text
    -- ^ The date and time when the GCM channel was last modified.
  , version :: Core.Maybe Core.Int
    -- ^ The current version of the GCM channel.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GCMChannelResponse' value with any optional fields omitted.
mkGCMChannelResponse
    :: Core.Text -- ^ 'credential'
    -> Core.Text -- ^ 'platform'
    -> GCMChannelResponse
mkGCMChannelResponse credential platform
  = GCMChannelResponse'{credential, platform,
                        applicationId = Core.Nothing, creationDate = Core.Nothing,
                        enabled = Core.Nothing, hasCredential = Core.Nothing,
                        id = Core.Nothing, isArchived = Core.Nothing,
                        lastModifiedBy = Core.Nothing, lastModifiedDate = Core.Nothing,
                        version = Core.Nothing}

-- | The Web API Key, also referred to as an /API_KEY/ or /server key/ , that you received from Google to communicate with Google services.
--
-- /Note:/ Consider using 'credential' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcmcrCredential :: Lens.Lens' GCMChannelResponse Core.Text
gcmcrCredential = Lens.field @"credential"
{-# INLINEABLE gcmcrCredential #-}
{-# DEPRECATED credential "Use generic-lens or generic-optics with 'credential' instead"  #-}

-- | The type of messaging or notification platform for the channel. For the GCM channel, this value is GCM.
--
-- /Note:/ Consider using 'platform' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcmcrPlatform :: Lens.Lens' GCMChannelResponse Core.Text
gcmcrPlatform = Lens.field @"platform"
{-# INLINEABLE gcmcrPlatform #-}
{-# DEPRECATED platform "Use generic-lens or generic-optics with 'platform' instead"  #-}

-- | The unique identifier for the application that the GCM channel applies to.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcmcrApplicationId :: Lens.Lens' GCMChannelResponse (Core.Maybe Core.Text)
gcmcrApplicationId = Lens.field @"applicationId"
{-# INLINEABLE gcmcrApplicationId #-}
{-# DEPRECATED applicationId "Use generic-lens or generic-optics with 'applicationId' instead"  #-}

-- | The date and time when the GCM channel was enabled.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcmcrCreationDate :: Lens.Lens' GCMChannelResponse (Core.Maybe Core.Text)
gcmcrCreationDate = Lens.field @"creationDate"
{-# INLINEABLE gcmcrCreationDate #-}
{-# DEPRECATED creationDate "Use generic-lens or generic-optics with 'creationDate' instead"  #-}

-- | Specifies whether the GCM channel is enabled for the application.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcmcrEnabled :: Lens.Lens' GCMChannelResponse (Core.Maybe Core.Bool)
gcmcrEnabled = Lens.field @"enabled"
{-# INLINEABLE gcmcrEnabled #-}
{-# DEPRECATED enabled "Use generic-lens or generic-optics with 'enabled' instead"  #-}

-- | (Not used) This property is retained only for backward compatibility.
--
-- /Note:/ Consider using 'hasCredential' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcmcrHasCredential :: Lens.Lens' GCMChannelResponse (Core.Maybe Core.Bool)
gcmcrHasCredential = Lens.field @"hasCredential"
{-# INLINEABLE gcmcrHasCredential #-}
{-# DEPRECATED hasCredential "Use generic-lens or generic-optics with 'hasCredential' instead"  #-}

-- | (Deprecated) An identifier for the GCM channel. This property is retained only for backward compatibility.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcmcrId :: Lens.Lens' GCMChannelResponse (Core.Maybe Core.Text)
gcmcrId = Lens.field @"id"
{-# INLINEABLE gcmcrId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | Specifies whether the GCM channel is archived.
--
-- /Note:/ Consider using 'isArchived' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcmcrIsArchived :: Lens.Lens' GCMChannelResponse (Core.Maybe Core.Bool)
gcmcrIsArchived = Lens.field @"isArchived"
{-# INLINEABLE gcmcrIsArchived #-}
{-# DEPRECATED isArchived "Use generic-lens or generic-optics with 'isArchived' instead"  #-}

-- | The user who last modified the GCM channel.
--
-- /Note:/ Consider using 'lastModifiedBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcmcrLastModifiedBy :: Lens.Lens' GCMChannelResponse (Core.Maybe Core.Text)
gcmcrLastModifiedBy = Lens.field @"lastModifiedBy"
{-# INLINEABLE gcmcrLastModifiedBy #-}
{-# DEPRECATED lastModifiedBy "Use generic-lens or generic-optics with 'lastModifiedBy' instead"  #-}

-- | The date and time when the GCM channel was last modified.
--
-- /Note:/ Consider using 'lastModifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcmcrLastModifiedDate :: Lens.Lens' GCMChannelResponse (Core.Maybe Core.Text)
gcmcrLastModifiedDate = Lens.field @"lastModifiedDate"
{-# INLINEABLE gcmcrLastModifiedDate #-}
{-# DEPRECATED lastModifiedDate "Use generic-lens or generic-optics with 'lastModifiedDate' instead"  #-}

-- | The current version of the GCM channel.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcmcrVersion :: Lens.Lens' GCMChannelResponse (Core.Maybe Core.Int)
gcmcrVersion = Lens.field @"version"
{-# INLINEABLE gcmcrVersion #-}
{-# DEPRECATED version "Use generic-lens or generic-optics with 'version' instead"  #-}

instance Core.FromJSON GCMChannelResponse where
        parseJSON
          = Core.withObject "GCMChannelResponse" Core.$
              \ x ->
                GCMChannelResponse' Core.<$>
                  (x Core..: "Credential") Core.<*> x Core..: "Platform" Core.<*>
                    x Core..:? "ApplicationId"
                    Core.<*> x Core..:? "CreationDate"
                    Core.<*> x Core..:? "Enabled"
                    Core.<*> x Core..:? "HasCredential"
                    Core.<*> x Core..:? "Id"
                    Core.<*> x Core..:? "IsArchived"
                    Core.<*> x Core..:? "LastModifiedBy"
                    Core.<*> x Core..:? "LastModifiedDate"
                    Core.<*> x Core..:? "Version"
