{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.ADMChannelResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Pinpoint.Types.ADMChannelResponse
  ( ADMChannelResponse (..)
  -- * Smart constructor
  , mkADMChannelResponse
  -- * Lenses
  , admcrPlatform
  , admcrApplicationId
  , admcrCreationDate
  , admcrEnabled
  , admcrHasCredential
  , admcrId
  , admcrIsArchived
  , admcrLastModifiedBy
  , admcrLastModifiedDate
  , admcrVersion
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Provides information about the status and settings of the ADM (Amazon Device Messaging) channel for an application.
--
-- /See:/ 'mkADMChannelResponse' smart constructor.
data ADMChannelResponse = ADMChannelResponse'
  { platform :: Core.Text
    -- ^ The type of messaging or notification platform for the channel. For the ADM channel, this value is ADM.
  , applicationId :: Core.Maybe Core.Text
    -- ^ The unique identifier for the application that the ADM channel applies to.
  , creationDate :: Core.Maybe Core.Text
    -- ^ The date and time when the ADM channel was enabled.
  , enabled :: Core.Maybe Core.Bool
    -- ^ Specifies whether the ADM channel is enabled for the application.
  , hasCredential :: Core.Maybe Core.Bool
    -- ^ (Not used) This property is retained only for backward compatibility.
  , id :: Core.Maybe Core.Text
    -- ^ (Deprecated) An identifier for the ADM channel. This property is retained only for backward compatibility.
  , isArchived :: Core.Maybe Core.Bool
    -- ^ Specifies whether the ADM channel is archived.
  , lastModifiedBy :: Core.Maybe Core.Text
    -- ^ The user who last modified the ADM channel.
  , lastModifiedDate :: Core.Maybe Core.Text
    -- ^ The date and time when the ADM channel was last modified.
  , version :: Core.Maybe Core.Int
    -- ^ The current version of the ADM channel.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ADMChannelResponse' value with any optional fields omitted.
mkADMChannelResponse
    :: Core.Text -- ^ 'platform'
    -> ADMChannelResponse
mkADMChannelResponse platform
  = ADMChannelResponse'{platform, applicationId = Core.Nothing,
                        creationDate = Core.Nothing, enabled = Core.Nothing,
                        hasCredential = Core.Nothing, id = Core.Nothing,
                        isArchived = Core.Nothing, lastModifiedBy = Core.Nothing,
                        lastModifiedDate = Core.Nothing, version = Core.Nothing}

-- | The type of messaging or notification platform for the channel. For the ADM channel, this value is ADM.
--
-- /Note:/ Consider using 'platform' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
admcrPlatform :: Lens.Lens' ADMChannelResponse Core.Text
admcrPlatform = Lens.field @"platform"
{-# INLINEABLE admcrPlatform #-}
{-# DEPRECATED platform "Use generic-lens or generic-optics with 'platform' instead"  #-}

-- | The unique identifier for the application that the ADM channel applies to.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
admcrApplicationId :: Lens.Lens' ADMChannelResponse (Core.Maybe Core.Text)
admcrApplicationId = Lens.field @"applicationId"
{-# INLINEABLE admcrApplicationId #-}
{-# DEPRECATED applicationId "Use generic-lens or generic-optics with 'applicationId' instead"  #-}

-- | The date and time when the ADM channel was enabled.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
admcrCreationDate :: Lens.Lens' ADMChannelResponse (Core.Maybe Core.Text)
admcrCreationDate = Lens.field @"creationDate"
{-# INLINEABLE admcrCreationDate #-}
{-# DEPRECATED creationDate "Use generic-lens or generic-optics with 'creationDate' instead"  #-}

-- | Specifies whether the ADM channel is enabled for the application.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
admcrEnabled :: Lens.Lens' ADMChannelResponse (Core.Maybe Core.Bool)
admcrEnabled = Lens.field @"enabled"
{-# INLINEABLE admcrEnabled #-}
{-# DEPRECATED enabled "Use generic-lens or generic-optics with 'enabled' instead"  #-}

-- | (Not used) This property is retained only for backward compatibility.
--
-- /Note:/ Consider using 'hasCredential' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
admcrHasCredential :: Lens.Lens' ADMChannelResponse (Core.Maybe Core.Bool)
admcrHasCredential = Lens.field @"hasCredential"
{-# INLINEABLE admcrHasCredential #-}
{-# DEPRECATED hasCredential "Use generic-lens or generic-optics with 'hasCredential' instead"  #-}

-- | (Deprecated) An identifier for the ADM channel. This property is retained only for backward compatibility.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
admcrId :: Lens.Lens' ADMChannelResponse (Core.Maybe Core.Text)
admcrId = Lens.field @"id"
{-# INLINEABLE admcrId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | Specifies whether the ADM channel is archived.
--
-- /Note:/ Consider using 'isArchived' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
admcrIsArchived :: Lens.Lens' ADMChannelResponse (Core.Maybe Core.Bool)
admcrIsArchived = Lens.field @"isArchived"
{-# INLINEABLE admcrIsArchived #-}
{-# DEPRECATED isArchived "Use generic-lens or generic-optics with 'isArchived' instead"  #-}

-- | The user who last modified the ADM channel.
--
-- /Note:/ Consider using 'lastModifiedBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
admcrLastModifiedBy :: Lens.Lens' ADMChannelResponse (Core.Maybe Core.Text)
admcrLastModifiedBy = Lens.field @"lastModifiedBy"
{-# INLINEABLE admcrLastModifiedBy #-}
{-# DEPRECATED lastModifiedBy "Use generic-lens or generic-optics with 'lastModifiedBy' instead"  #-}

-- | The date and time when the ADM channel was last modified.
--
-- /Note:/ Consider using 'lastModifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
admcrLastModifiedDate :: Lens.Lens' ADMChannelResponse (Core.Maybe Core.Text)
admcrLastModifiedDate = Lens.field @"lastModifiedDate"
{-# INLINEABLE admcrLastModifiedDate #-}
{-# DEPRECATED lastModifiedDate "Use generic-lens or generic-optics with 'lastModifiedDate' instead"  #-}

-- | The current version of the ADM channel.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
admcrVersion :: Lens.Lens' ADMChannelResponse (Core.Maybe Core.Int)
admcrVersion = Lens.field @"version"
{-# INLINEABLE admcrVersion #-}
{-# DEPRECATED version "Use generic-lens or generic-optics with 'version' instead"  #-}

instance Core.FromJSON ADMChannelResponse where
        parseJSON
          = Core.withObject "ADMChannelResponse" Core.$
              \ x ->
                ADMChannelResponse' Core.<$>
                  (x Core..: "Platform") Core.<*> x Core..:? "ApplicationId" Core.<*>
                    x Core..:? "CreationDate"
                    Core.<*> x Core..:? "Enabled"
                    Core.<*> x Core..:? "HasCredential"
                    Core.<*> x Core..:? "Id"
                    Core.<*> x Core..:? "IsArchived"
                    Core.<*> x Core..:? "LastModifiedBy"
                    Core.<*> x Core..:? "LastModifiedDate"
                    Core.<*> x Core..:? "Version"
