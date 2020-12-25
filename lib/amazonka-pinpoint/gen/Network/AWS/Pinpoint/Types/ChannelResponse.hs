{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.ChannelResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.ChannelResponse
  ( ChannelResponse (..),

    -- * Smart constructor
    mkChannelResponse,

    -- * Lenses
    cApplicationId,
    cCreationDate,
    cEnabled,
    cHasCredential,
    cId,
    cIsArchived,
    cLastModifiedBy,
    cLastModifiedDate,
    cVersion,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Provides information about the general settings and status of a channel for an application.
--
-- /See:/ 'mkChannelResponse' smart constructor.
data ChannelResponse = ChannelResponse'
  { -- | The unique identifier for the application.
    applicationId :: Core.Maybe Core.Text,
    -- | The date and time, in ISO 8601 format, when the channel was enabled.
    creationDate :: Core.Maybe Core.Text,
    -- | Specifies whether the channel is enabled for the application.
    enabled :: Core.Maybe Core.Bool,
    -- | (Not used) This property is retained only for backward compatibility.
    hasCredential :: Core.Maybe Core.Bool,
    -- | (Deprecated) An identifier for the channel. This property is retained only for backward compatibility.
    id :: Core.Maybe Core.Text,
    -- | Specifies whether the channel is archived.
    isArchived :: Core.Maybe Core.Bool,
    -- | The user who last modified the channel.
    lastModifiedBy :: Core.Maybe Core.Text,
    -- | The date and time, in ISO 8601 format, when the channel was last modified.
    lastModifiedDate :: Core.Maybe Core.Text,
    -- | The current version of the channel.
    version :: Core.Maybe Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ChannelResponse' value with any optional fields omitted.
mkChannelResponse ::
  ChannelResponse
mkChannelResponse =
  ChannelResponse'
    { applicationId = Core.Nothing,
      creationDate = Core.Nothing,
      enabled = Core.Nothing,
      hasCredential = Core.Nothing,
      id = Core.Nothing,
      isArchived = Core.Nothing,
      lastModifiedBy = Core.Nothing,
      lastModifiedDate = Core.Nothing,
      version = Core.Nothing
    }

-- | The unique identifier for the application.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cApplicationId :: Lens.Lens' ChannelResponse (Core.Maybe Core.Text)
cApplicationId = Lens.field @"applicationId"
{-# DEPRECATED cApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

-- | The date and time, in ISO 8601 format, when the channel was enabled.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCreationDate :: Lens.Lens' ChannelResponse (Core.Maybe Core.Text)
cCreationDate = Lens.field @"creationDate"
{-# DEPRECATED cCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

-- | Specifies whether the channel is enabled for the application.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cEnabled :: Lens.Lens' ChannelResponse (Core.Maybe Core.Bool)
cEnabled = Lens.field @"enabled"
{-# DEPRECATED cEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

-- | (Not used) This property is retained only for backward compatibility.
--
-- /Note:/ Consider using 'hasCredential' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cHasCredential :: Lens.Lens' ChannelResponse (Core.Maybe Core.Bool)
cHasCredential = Lens.field @"hasCredential"
{-# DEPRECATED cHasCredential "Use generic-lens or generic-optics with 'hasCredential' instead." #-}

-- | (Deprecated) An identifier for the channel. This property is retained only for backward compatibility.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cId :: Lens.Lens' ChannelResponse (Core.Maybe Core.Text)
cId = Lens.field @"id"
{-# DEPRECATED cId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | Specifies whether the channel is archived.
--
-- /Note:/ Consider using 'isArchived' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cIsArchived :: Lens.Lens' ChannelResponse (Core.Maybe Core.Bool)
cIsArchived = Lens.field @"isArchived"
{-# DEPRECATED cIsArchived "Use generic-lens or generic-optics with 'isArchived' instead." #-}

-- | The user who last modified the channel.
--
-- /Note:/ Consider using 'lastModifiedBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cLastModifiedBy :: Lens.Lens' ChannelResponse (Core.Maybe Core.Text)
cLastModifiedBy = Lens.field @"lastModifiedBy"
{-# DEPRECATED cLastModifiedBy "Use generic-lens or generic-optics with 'lastModifiedBy' instead." #-}

-- | The date and time, in ISO 8601 format, when the channel was last modified.
--
-- /Note:/ Consider using 'lastModifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cLastModifiedDate :: Lens.Lens' ChannelResponse (Core.Maybe Core.Text)
cLastModifiedDate = Lens.field @"lastModifiedDate"
{-# DEPRECATED cLastModifiedDate "Use generic-lens or generic-optics with 'lastModifiedDate' instead." #-}

-- | The current version of the channel.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cVersion :: Lens.Lens' ChannelResponse (Core.Maybe Core.Int)
cVersion = Lens.field @"version"
{-# DEPRECATED cVersion "Use generic-lens or generic-optics with 'version' instead." #-}

instance Core.FromJSON ChannelResponse where
  parseJSON =
    Core.withObject "ChannelResponse" Core.$
      \x ->
        ChannelResponse'
          Core.<$> (x Core..:? "ApplicationId")
          Core.<*> (x Core..:? "CreationDate")
          Core.<*> (x Core..:? "Enabled")
          Core.<*> (x Core..:? "HasCredential")
          Core.<*> (x Core..:? "Id")
          Core.<*> (x Core..:? "IsArchived")
          Core.<*> (x Core..:? "LastModifiedBy")
          Core.<*> (x Core..:? "LastModifiedDate")
          Core.<*> (x Core..:? "Version")
