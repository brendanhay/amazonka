{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.ADMChannelResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.ADMChannelResponse
  ( ADMChannelResponse (..),

    -- * Smart constructor
    mkADMChannelResponse,

    -- * Lenses
    admcrPlatform,
    admcrApplicationId,
    admcrCreationDate,
    admcrEnabled,
    admcrHasCredential,
    admcrId,
    admcrIsArchived,
    admcrLastModifiedBy,
    admcrLastModifiedDate,
    admcrVersion,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Provides information about the status and settings of the ADM (Amazon Device Messaging) channel for an application.
--
-- /See:/ 'mkADMChannelResponse' smart constructor.
data ADMChannelResponse = ADMChannelResponse'
  { -- | The type of messaging or notification platform for the channel. For the ADM channel, this value is ADM.
    platform :: Core.Text,
    -- | The unique identifier for the application that the ADM channel applies to.
    applicationId :: Core.Maybe Core.Text,
    -- | The date and time when the ADM channel was enabled.
    creationDate :: Core.Maybe Core.Text,
    -- | Specifies whether the ADM channel is enabled for the application.
    enabled :: Core.Maybe Core.Bool,
    -- | (Not used) This property is retained only for backward compatibility.
    hasCredential :: Core.Maybe Core.Bool,
    -- | (Deprecated) An identifier for the ADM channel. This property is retained only for backward compatibility.
    id :: Core.Maybe Core.Text,
    -- | Specifies whether the ADM channel is archived.
    isArchived :: Core.Maybe Core.Bool,
    -- | The user who last modified the ADM channel.
    lastModifiedBy :: Core.Maybe Core.Text,
    -- | The date and time when the ADM channel was last modified.
    lastModifiedDate :: Core.Maybe Core.Text,
    -- | The current version of the ADM channel.
    version :: Core.Maybe Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ADMChannelResponse' value with any optional fields omitted.
mkADMChannelResponse ::
  -- | 'platform'
  Core.Text ->
  ADMChannelResponse
mkADMChannelResponse platform =
  ADMChannelResponse'
    { platform,
      applicationId = Core.Nothing,
      creationDate = Core.Nothing,
      enabled = Core.Nothing,
      hasCredential = Core.Nothing,
      id = Core.Nothing,
      isArchived = Core.Nothing,
      lastModifiedBy = Core.Nothing,
      lastModifiedDate = Core.Nothing,
      version = Core.Nothing
    }

-- | The type of messaging or notification platform for the channel. For the ADM channel, this value is ADM.
--
-- /Note:/ Consider using 'platform' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
admcrPlatform :: Lens.Lens' ADMChannelResponse Core.Text
admcrPlatform = Lens.field @"platform"
{-# DEPRECATED admcrPlatform "Use generic-lens or generic-optics with 'platform' instead." #-}

-- | The unique identifier for the application that the ADM channel applies to.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
admcrApplicationId :: Lens.Lens' ADMChannelResponse (Core.Maybe Core.Text)
admcrApplicationId = Lens.field @"applicationId"
{-# DEPRECATED admcrApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

-- | The date and time when the ADM channel was enabled.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
admcrCreationDate :: Lens.Lens' ADMChannelResponse (Core.Maybe Core.Text)
admcrCreationDate = Lens.field @"creationDate"
{-# DEPRECATED admcrCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

-- | Specifies whether the ADM channel is enabled for the application.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
admcrEnabled :: Lens.Lens' ADMChannelResponse (Core.Maybe Core.Bool)
admcrEnabled = Lens.field @"enabled"
{-# DEPRECATED admcrEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

-- | (Not used) This property is retained only for backward compatibility.
--
-- /Note:/ Consider using 'hasCredential' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
admcrHasCredential :: Lens.Lens' ADMChannelResponse (Core.Maybe Core.Bool)
admcrHasCredential = Lens.field @"hasCredential"
{-# DEPRECATED admcrHasCredential "Use generic-lens or generic-optics with 'hasCredential' instead." #-}

-- | (Deprecated) An identifier for the ADM channel. This property is retained only for backward compatibility.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
admcrId :: Lens.Lens' ADMChannelResponse (Core.Maybe Core.Text)
admcrId = Lens.field @"id"
{-# DEPRECATED admcrId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | Specifies whether the ADM channel is archived.
--
-- /Note:/ Consider using 'isArchived' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
admcrIsArchived :: Lens.Lens' ADMChannelResponse (Core.Maybe Core.Bool)
admcrIsArchived = Lens.field @"isArchived"
{-# DEPRECATED admcrIsArchived "Use generic-lens or generic-optics with 'isArchived' instead." #-}

-- | The user who last modified the ADM channel.
--
-- /Note:/ Consider using 'lastModifiedBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
admcrLastModifiedBy :: Lens.Lens' ADMChannelResponse (Core.Maybe Core.Text)
admcrLastModifiedBy = Lens.field @"lastModifiedBy"
{-# DEPRECATED admcrLastModifiedBy "Use generic-lens or generic-optics with 'lastModifiedBy' instead." #-}

-- | The date and time when the ADM channel was last modified.
--
-- /Note:/ Consider using 'lastModifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
admcrLastModifiedDate :: Lens.Lens' ADMChannelResponse (Core.Maybe Core.Text)
admcrLastModifiedDate = Lens.field @"lastModifiedDate"
{-# DEPRECATED admcrLastModifiedDate "Use generic-lens or generic-optics with 'lastModifiedDate' instead." #-}

-- | The current version of the ADM channel.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
admcrVersion :: Lens.Lens' ADMChannelResponse (Core.Maybe Core.Int)
admcrVersion = Lens.field @"version"
{-# DEPRECATED admcrVersion "Use generic-lens or generic-optics with 'version' instead." #-}

instance Core.FromJSON ADMChannelResponse where
  parseJSON =
    Core.withObject "ADMChannelResponse" Core.$
      \x ->
        ADMChannelResponse'
          Core.<$> (x Core..: "Platform")
          Core.<*> (x Core..:? "ApplicationId")
          Core.<*> (x Core..:? "CreationDate")
          Core.<*> (x Core..:? "Enabled")
          Core.<*> (x Core..:? "HasCredential")
          Core.<*> (x Core..:? "Id")
          Core.<*> (x Core..:? "IsArchived")
          Core.<*> (x Core..:? "LastModifiedBy")
          Core.<*> (x Core..:? "LastModifiedDate")
          Core.<*> (x Core..:? "Version")
