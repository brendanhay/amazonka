{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaPackage.Types.Channel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaPackage.Types.Channel
  ( Channel (..),

    -- * Smart constructor
    mkChannel,

    -- * Lenses
    cArn,
    cDescription,
    cEgressAccessLogs,
    cHlsIngest,
    cId,
    cIngressAccessLogs,
    cTags,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaPackage.Types.EgressAccessLogs as Types
import qualified Network.AWS.MediaPackage.Types.HlsIngest as Types
import qualified Network.AWS.MediaPackage.Types.IngressAccessLogs as Types
import qualified Network.AWS.Prelude as Core

-- | A Channel resource configuration.
--
-- /See:/ 'mkChannel' smart constructor.
data Channel = Channel'
  { -- | The Amazon Resource Name (ARN) assigned to the Channel.
    arn :: Core.Maybe Core.Text,
    -- | A short text description of the Channel.
    description :: Core.Maybe Core.Text,
    egressAccessLogs :: Core.Maybe Types.EgressAccessLogs,
    hlsIngest :: Core.Maybe Types.HlsIngest,
    -- | The ID of the Channel.
    id :: Core.Maybe Core.Text,
    ingressAccessLogs :: Core.Maybe Types.IngressAccessLogs,
    tags :: Core.Maybe (Core.HashMap Core.Text Core.Text)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Channel' value with any optional fields omitted.
mkChannel ::
  Channel
mkChannel =
  Channel'
    { arn = Core.Nothing,
      description = Core.Nothing,
      egressAccessLogs = Core.Nothing,
      hlsIngest = Core.Nothing,
      id = Core.Nothing,
      ingressAccessLogs = Core.Nothing,
      tags = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) assigned to the Channel.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cArn :: Lens.Lens' Channel (Core.Maybe Core.Text)
cArn = Lens.field @"arn"
{-# DEPRECATED cArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | A short text description of the Channel.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cDescription :: Lens.Lens' Channel (Core.Maybe Core.Text)
cDescription = Lens.field @"description"
{-# DEPRECATED cDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'egressAccessLogs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cEgressAccessLogs :: Lens.Lens' Channel (Core.Maybe Types.EgressAccessLogs)
cEgressAccessLogs = Lens.field @"egressAccessLogs"
{-# DEPRECATED cEgressAccessLogs "Use generic-lens or generic-optics with 'egressAccessLogs' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'hlsIngest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cHlsIngest :: Lens.Lens' Channel (Core.Maybe Types.HlsIngest)
cHlsIngest = Lens.field @"hlsIngest"
{-# DEPRECATED cHlsIngest "Use generic-lens or generic-optics with 'hlsIngest' instead." #-}

-- | The ID of the Channel.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cId :: Lens.Lens' Channel (Core.Maybe Core.Text)
cId = Lens.field @"id"
{-# DEPRECATED cId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'ingressAccessLogs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cIngressAccessLogs :: Lens.Lens' Channel (Core.Maybe Types.IngressAccessLogs)
cIngressAccessLogs = Lens.field @"ingressAccessLogs"
{-# DEPRECATED cIngressAccessLogs "Use generic-lens or generic-optics with 'ingressAccessLogs' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cTags :: Lens.Lens' Channel (Core.Maybe (Core.HashMap Core.Text Core.Text))
cTags = Lens.field @"tags"
{-# DEPRECATED cTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromJSON Channel where
  parseJSON =
    Core.withObject "Channel" Core.$
      \x ->
        Channel'
          Core.<$> (x Core..:? "arn")
          Core.<*> (x Core..:? "description")
          Core.<*> (x Core..:? "egressAccessLogs")
          Core.<*> (x Core..:? "hlsIngest")
          Core.<*> (x Core..:? "id")
          Core.<*> (x Core..:? "ingressAccessLogs")
          Core.<*> (x Core..:? "tags")
