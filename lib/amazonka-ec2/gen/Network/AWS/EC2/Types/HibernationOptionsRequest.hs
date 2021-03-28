{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.HibernationOptionsRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.HibernationOptionsRequest
  ( HibernationOptionsRequest (..)
  -- * Smart constructor
  , mkHibernationOptionsRequest
  -- * Lenses
  , horConfigured
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Indicates whether your instance is configured for hibernation. This parameter is valid only if the instance meets the <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Hibernate.html#hibernating-prerequisites hibernation prerequisites> . For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Hibernate.html Hibernate your instance> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- /See:/ 'mkHibernationOptionsRequest' smart constructor.
newtype HibernationOptionsRequest = HibernationOptionsRequest'
  { configured :: Core.Maybe Core.Bool
    -- ^ If you set this parameter to @true@ , your instance is enabled for hibernation.
--
-- Default: @false@ 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'HibernationOptionsRequest' value with any optional fields omitted.
mkHibernationOptionsRequest
    :: HibernationOptionsRequest
mkHibernationOptionsRequest
  = HibernationOptionsRequest'{configured = Core.Nothing}

-- | If you set this parameter to @true@ , your instance is enabled for hibernation.
--
-- Default: @false@ 
--
-- /Note:/ Consider using 'configured' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
horConfigured :: Lens.Lens' HibernationOptionsRequest (Core.Maybe Core.Bool)
horConfigured = Lens.field @"configured"
{-# INLINEABLE horConfigured #-}
{-# DEPRECATED configured "Use generic-lens or generic-optics with 'configured' instead"  #-}

instance Core.ToQuery HibernationOptionsRequest where
        toQuery HibernationOptionsRequest{..}
          = Core.maybe Core.mempty (Core.toQueryPair "Configured") configured
