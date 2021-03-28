{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.VoiceChannelResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Pinpoint.Types.VoiceChannelResponse
  ( VoiceChannelResponse (..)
  -- * Smart constructor
  , mkVoiceChannelResponse
  -- * Lenses
  , vcrfPlatform
  , vcrfApplicationId
  , vcrfCreationDate
  , vcrfEnabled
  , vcrfHasCredential
  , vcrfId
  , vcrfIsArchived
  , vcrfLastModifiedBy
  , vcrfLastModifiedDate
  , vcrfVersion
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Provides information about the status and settings of the voice channel for an application.
--
-- /See:/ 'mkVoiceChannelResponse' smart constructor.
data VoiceChannelResponse = VoiceChannelResponse'
  { platform :: Core.Text
    -- ^ The type of messaging or notification platform for the channel. For the voice channel, this value is VOICE.
  , applicationId :: Core.Maybe Core.Text
    -- ^ The unique identifier for the application that the voice channel applies to.
  , creationDate :: Core.Maybe Core.Text
    -- ^ The date and time, in ISO 8601 format, when the voice channel was enabled.
  , enabled :: Core.Maybe Core.Bool
    -- ^ Specifies whether the voice channel is enabled for the application.
  , hasCredential :: Core.Maybe Core.Bool
    -- ^ (Not used) This property is retained only for backward compatibility.
  , id :: Core.Maybe Core.Text
    -- ^ (Deprecated) An identifier for the voice channel. This property is retained only for backward compatibility.
  , isArchived :: Core.Maybe Core.Bool
    -- ^ Specifies whether the voice channel is archived.
  , lastModifiedBy :: Core.Maybe Core.Text
    -- ^ The user who last modified the voice channel.
  , lastModifiedDate :: Core.Maybe Core.Text
    -- ^ The date and time, in ISO 8601 format, when the voice channel was last modified.
  , version :: Core.Maybe Core.Int
    -- ^ The current version of the voice channel.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'VoiceChannelResponse' value with any optional fields omitted.
mkVoiceChannelResponse
    :: Core.Text -- ^ 'platform'
    -> VoiceChannelResponse
mkVoiceChannelResponse platform
  = VoiceChannelResponse'{platform, applicationId = Core.Nothing,
                          creationDate = Core.Nothing, enabled = Core.Nothing,
                          hasCredential = Core.Nothing, id = Core.Nothing,
                          isArchived = Core.Nothing, lastModifiedBy = Core.Nothing,
                          lastModifiedDate = Core.Nothing, version = Core.Nothing}

-- | The type of messaging or notification platform for the channel. For the voice channel, this value is VOICE.
--
-- /Note:/ Consider using 'platform' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcrfPlatform :: Lens.Lens' VoiceChannelResponse Core.Text
vcrfPlatform = Lens.field @"platform"
{-# INLINEABLE vcrfPlatform #-}
{-# DEPRECATED platform "Use generic-lens or generic-optics with 'platform' instead"  #-}

-- | The unique identifier for the application that the voice channel applies to.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcrfApplicationId :: Lens.Lens' VoiceChannelResponse (Core.Maybe Core.Text)
vcrfApplicationId = Lens.field @"applicationId"
{-# INLINEABLE vcrfApplicationId #-}
{-# DEPRECATED applicationId "Use generic-lens or generic-optics with 'applicationId' instead"  #-}

-- | The date and time, in ISO 8601 format, when the voice channel was enabled.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcrfCreationDate :: Lens.Lens' VoiceChannelResponse (Core.Maybe Core.Text)
vcrfCreationDate = Lens.field @"creationDate"
{-# INLINEABLE vcrfCreationDate #-}
{-# DEPRECATED creationDate "Use generic-lens or generic-optics with 'creationDate' instead"  #-}

-- | Specifies whether the voice channel is enabled for the application.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcrfEnabled :: Lens.Lens' VoiceChannelResponse (Core.Maybe Core.Bool)
vcrfEnabled = Lens.field @"enabled"
{-# INLINEABLE vcrfEnabled #-}
{-# DEPRECATED enabled "Use generic-lens or generic-optics with 'enabled' instead"  #-}

-- | (Not used) This property is retained only for backward compatibility.
--
-- /Note:/ Consider using 'hasCredential' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcrfHasCredential :: Lens.Lens' VoiceChannelResponse (Core.Maybe Core.Bool)
vcrfHasCredential = Lens.field @"hasCredential"
{-# INLINEABLE vcrfHasCredential #-}
{-# DEPRECATED hasCredential "Use generic-lens or generic-optics with 'hasCredential' instead"  #-}

-- | (Deprecated) An identifier for the voice channel. This property is retained only for backward compatibility.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcrfId :: Lens.Lens' VoiceChannelResponse (Core.Maybe Core.Text)
vcrfId = Lens.field @"id"
{-# INLINEABLE vcrfId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | Specifies whether the voice channel is archived.
--
-- /Note:/ Consider using 'isArchived' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcrfIsArchived :: Lens.Lens' VoiceChannelResponse (Core.Maybe Core.Bool)
vcrfIsArchived = Lens.field @"isArchived"
{-# INLINEABLE vcrfIsArchived #-}
{-# DEPRECATED isArchived "Use generic-lens or generic-optics with 'isArchived' instead"  #-}

-- | The user who last modified the voice channel.
--
-- /Note:/ Consider using 'lastModifiedBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcrfLastModifiedBy :: Lens.Lens' VoiceChannelResponse (Core.Maybe Core.Text)
vcrfLastModifiedBy = Lens.field @"lastModifiedBy"
{-# INLINEABLE vcrfLastModifiedBy #-}
{-# DEPRECATED lastModifiedBy "Use generic-lens or generic-optics with 'lastModifiedBy' instead"  #-}

-- | The date and time, in ISO 8601 format, when the voice channel was last modified.
--
-- /Note:/ Consider using 'lastModifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcrfLastModifiedDate :: Lens.Lens' VoiceChannelResponse (Core.Maybe Core.Text)
vcrfLastModifiedDate = Lens.field @"lastModifiedDate"
{-# INLINEABLE vcrfLastModifiedDate #-}
{-# DEPRECATED lastModifiedDate "Use generic-lens or generic-optics with 'lastModifiedDate' instead"  #-}

-- | The current version of the voice channel.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcrfVersion :: Lens.Lens' VoiceChannelResponse (Core.Maybe Core.Int)
vcrfVersion = Lens.field @"version"
{-# INLINEABLE vcrfVersion #-}
{-# DEPRECATED version "Use generic-lens or generic-optics with 'version' instead"  #-}

instance Core.FromJSON VoiceChannelResponse where
        parseJSON
          = Core.withObject "VoiceChannelResponse" Core.$
              \ x ->
                VoiceChannelResponse' Core.<$>
                  (x Core..: "Platform") Core.<*> x Core..:? "ApplicationId" Core.<*>
                    x Core..:? "CreationDate"
                    Core.<*> x Core..:? "Enabled"
                    Core.<*> x Core..:? "HasCredential"
                    Core.<*> x Core..:? "Id"
                    Core.<*> x Core..:? "IsArchived"
                    Core.<*> x Core..:? "LastModifiedBy"
                    Core.<*> x Core..:? "LastModifiedDate"
                    Core.<*> x Core..:? "Version"
