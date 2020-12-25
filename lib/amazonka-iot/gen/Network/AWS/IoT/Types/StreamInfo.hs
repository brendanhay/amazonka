{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.StreamInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.StreamInfo
  ( StreamInfo (..),

    -- * Smart constructor
    mkStreamInfo,

    -- * Lenses
    siCreatedAt,
    siDescription,
    siFiles,
    siLastUpdatedAt,
    siRoleArn,
    siStreamArn,
    siStreamId,
    siStreamVersion,
  )
where

import qualified Network.AWS.IoT.Types.RoleArn as Types
import qualified Network.AWS.IoT.Types.StreamArn as Types
import qualified Network.AWS.IoT.Types.StreamDescription as Types
import qualified Network.AWS.IoT.Types.StreamFile as Types
import qualified Network.AWS.IoT.Types.StreamId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about a stream.
--
-- /See:/ 'mkStreamInfo' smart constructor.
data StreamInfo = StreamInfo'
  { -- | The date when the stream was created.
    createdAt :: Core.Maybe Core.NominalDiffTime,
    -- | The description of the stream.
    description :: Core.Maybe Types.StreamDescription,
    -- | The files to stream.
    files :: Core.Maybe (Core.NonEmpty Types.StreamFile),
    -- | The date when the stream was last updated.
    lastUpdatedAt :: Core.Maybe Core.NominalDiffTime,
    -- | An IAM role AWS IoT assumes to access your S3 files.
    roleArn :: Core.Maybe Types.RoleArn,
    -- | The stream ARN.
    streamArn :: Core.Maybe Types.StreamArn,
    -- | The stream ID.
    streamId :: Core.Maybe Types.StreamId,
    -- | The stream version.
    streamVersion :: Core.Maybe Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'StreamInfo' value with any optional fields omitted.
mkStreamInfo ::
  StreamInfo
mkStreamInfo =
  StreamInfo'
    { createdAt = Core.Nothing,
      description = Core.Nothing,
      files = Core.Nothing,
      lastUpdatedAt = Core.Nothing,
      roleArn = Core.Nothing,
      streamArn = Core.Nothing,
      streamId = Core.Nothing,
      streamVersion = Core.Nothing
    }

-- | The date when the stream was created.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siCreatedAt :: Lens.Lens' StreamInfo (Core.Maybe Core.NominalDiffTime)
siCreatedAt = Lens.field @"createdAt"
{-# DEPRECATED siCreatedAt "Use generic-lens or generic-optics with 'createdAt' instead." #-}

-- | The description of the stream.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siDescription :: Lens.Lens' StreamInfo (Core.Maybe Types.StreamDescription)
siDescription = Lens.field @"description"
{-# DEPRECATED siDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The files to stream.
--
-- /Note:/ Consider using 'files' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siFiles :: Lens.Lens' StreamInfo (Core.Maybe (Core.NonEmpty Types.StreamFile))
siFiles = Lens.field @"files"
{-# DEPRECATED siFiles "Use generic-lens or generic-optics with 'files' instead." #-}

-- | The date when the stream was last updated.
--
-- /Note:/ Consider using 'lastUpdatedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siLastUpdatedAt :: Lens.Lens' StreamInfo (Core.Maybe Core.NominalDiffTime)
siLastUpdatedAt = Lens.field @"lastUpdatedAt"
{-# DEPRECATED siLastUpdatedAt "Use generic-lens or generic-optics with 'lastUpdatedAt' instead." #-}

-- | An IAM role AWS IoT assumes to access your S3 files.
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siRoleArn :: Lens.Lens' StreamInfo (Core.Maybe Types.RoleArn)
siRoleArn = Lens.field @"roleArn"
{-# DEPRECATED siRoleArn "Use generic-lens or generic-optics with 'roleArn' instead." #-}

-- | The stream ARN.
--
-- /Note:/ Consider using 'streamArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siStreamArn :: Lens.Lens' StreamInfo (Core.Maybe Types.StreamArn)
siStreamArn = Lens.field @"streamArn"
{-# DEPRECATED siStreamArn "Use generic-lens or generic-optics with 'streamArn' instead." #-}

-- | The stream ID.
--
-- /Note:/ Consider using 'streamId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siStreamId :: Lens.Lens' StreamInfo (Core.Maybe Types.StreamId)
siStreamId = Lens.field @"streamId"
{-# DEPRECATED siStreamId "Use generic-lens or generic-optics with 'streamId' instead." #-}

-- | The stream version.
--
-- /Note:/ Consider using 'streamVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siStreamVersion :: Lens.Lens' StreamInfo (Core.Maybe Core.Natural)
siStreamVersion = Lens.field @"streamVersion"
{-# DEPRECATED siStreamVersion "Use generic-lens or generic-optics with 'streamVersion' instead." #-}

instance Core.FromJSON StreamInfo where
  parseJSON =
    Core.withObject "StreamInfo" Core.$
      \x ->
        StreamInfo'
          Core.<$> (x Core..:? "createdAt")
          Core.<*> (x Core..:? "description")
          Core.<*> (x Core..:? "files")
          Core.<*> (x Core..:? "lastUpdatedAt")
          Core.<*> (x Core..:? "roleArn")
          Core.<*> (x Core..:? "streamArn")
          Core.<*> (x Core..:? "streamId")
          Core.<*> (x Core..:? "streamVersion")
