{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.HibernationOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.HibernationOptions
  ( HibernationOptions (..),

    -- * Smart constructor
    mkHibernationOptions,

    -- * Lenses
    hoConfigured,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Indicates whether your instance is configured for hibernation. This parameter is valid only if the instance meets the <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Hibernate.html#hibernating-prerequisites hibernation prerequisites> . For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Hibernate.html Hibernate your instance> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- /See:/ 'mkHibernationOptions' smart constructor.
newtype HibernationOptions = HibernationOptions'
  { -- | If this parameter is set to @true@ , your instance is enabled for hibernation; otherwise, it is not enabled for hibernation.
    configured :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'HibernationOptions' value with any optional fields omitted.
mkHibernationOptions ::
  HibernationOptions
mkHibernationOptions =
  HibernationOptions' {configured = Core.Nothing}

-- | If this parameter is set to @true@ , your instance is enabled for hibernation; otherwise, it is not enabled for hibernation.
--
-- /Note:/ Consider using 'configured' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hoConfigured :: Lens.Lens' HibernationOptions (Core.Maybe Core.Bool)
hoConfigured = Lens.field @"configured"
{-# DEPRECATED hoConfigured "Use generic-lens or generic-optics with 'configured' instead." #-}

instance Core.FromXML HibernationOptions where
  parseXML x = HibernationOptions' Core.<$> (x Core..@? "configured")
