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
    hzcPrivateZone,
    hzcComment,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Route53.Internal

-- | A complex type that contains an optional comment about your hosted zone. If you don't want to specify a comment, omit both the @HostedZoneConfig@ and @Comment@ elements.
--
-- /See:/ 'mkHostedZoneConfig' smart constructor.
data HostedZoneConfig = HostedZoneConfig'
  { privateZone ::
      Lude.Maybe Lude.Bool,
    comment :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'HostedZoneConfig' with the minimum fields required to make a request.
--
-- * 'comment' - Any comments that you want to include about the hosted zone.
-- * 'privateZone' - A value that indicates whether this is a private hosted zone.
mkHostedZoneConfig ::
  HostedZoneConfig
mkHostedZoneConfig =
  HostedZoneConfig'
    { privateZone = Lude.Nothing,
      comment = Lude.Nothing
    }

-- | A value that indicates whether this is a private hosted zone.
--
-- /Note:/ Consider using 'privateZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hzcPrivateZone :: Lens.Lens' HostedZoneConfig (Lude.Maybe Lude.Bool)
hzcPrivateZone = Lens.lens (privateZone :: HostedZoneConfig -> Lude.Maybe Lude.Bool) (\s a -> s {privateZone = a} :: HostedZoneConfig)
{-# DEPRECATED hzcPrivateZone "Use generic-lens or generic-optics with 'privateZone' instead." #-}

-- | Any comments that you want to include about the hosted zone.
--
-- /Note:/ Consider using 'comment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hzcComment :: Lens.Lens' HostedZoneConfig (Lude.Maybe Lude.Text)
hzcComment = Lens.lens (comment :: HostedZoneConfig -> Lude.Maybe Lude.Text) (\s a -> s {comment = a} :: HostedZoneConfig)
{-# DEPRECATED hzcComment "Use generic-lens or generic-optics with 'comment' instead." #-}

instance Lude.FromXML HostedZoneConfig where
  parseXML x =
    HostedZoneConfig'
      Lude.<$> (x Lude..@? "PrivateZone") Lude.<*> (x Lude..@? "Comment")

instance Lude.ToXML HostedZoneConfig where
  toXML HostedZoneConfig' {..} =
    Lude.mconcat
      ["PrivateZone" Lude.@= privateZone, "Comment" Lude.@= comment]
