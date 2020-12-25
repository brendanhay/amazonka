{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.Types.HostedZoneConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53.Types.HostedZoneConfig
  ( HostedZoneConfig (..),

    -- * Smart constructor
    mkHostedZoneConfig,

    -- * Lenses
    hzcComment,
    hzcPrivateZone,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Route53.Internal as Types
import qualified Network.AWS.Route53.Types.ResourceDescription as Types

-- | A complex type that contains an optional comment about your hosted zone. If you don't want to specify a comment, omit both the @HostedZoneConfig@ and @Comment@ elements.
--
-- /See:/ 'mkHostedZoneConfig' smart constructor.
data HostedZoneConfig = HostedZoneConfig'
  { -- | Any comments that you want to include about the hosted zone.
    comment :: Core.Maybe Types.ResourceDescription,
    -- | A value that indicates whether this is a private hosted zone.
    privateZone :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'HostedZoneConfig' value with any optional fields omitted.
mkHostedZoneConfig ::
  HostedZoneConfig
mkHostedZoneConfig =
  HostedZoneConfig'
    { comment = Core.Nothing,
      privateZone = Core.Nothing
    }

-- | Any comments that you want to include about the hosted zone.
--
-- /Note:/ Consider using 'comment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hzcComment :: Lens.Lens' HostedZoneConfig (Core.Maybe Types.ResourceDescription)
hzcComment = Lens.field @"comment"
{-# DEPRECATED hzcComment "Use generic-lens or generic-optics with 'comment' instead." #-}

-- | A value that indicates whether this is a private hosted zone.
--
-- /Note:/ Consider using 'privateZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hzcPrivateZone :: Lens.Lens' HostedZoneConfig (Core.Maybe Core.Bool)
hzcPrivateZone = Lens.field @"privateZone"
{-# DEPRECATED hzcPrivateZone "Use generic-lens or generic-optics with 'privateZone' instead." #-}

instance Core.ToXML HostedZoneConfig where
  toXML HostedZoneConfig {..} =
    Core.toXMLNode "Comment" Core.<$> comment
      Core.<> Core.toXMLNode "PrivateZone" Core.<$> privateZone

instance Core.FromXML HostedZoneConfig where
  parseXML x =
    HostedZoneConfig'
      Core.<$> (x Core..@? "Comment") Core.<*> (x Core..@? "PrivateZone")
