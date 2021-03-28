{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.SourceIpConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.SourceIpConfig
  ( SourceIpConfig (..)
  -- * Smart constructor
  , mkSourceIpConfig
  -- * Lenses
  , sicCidrs
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.Cidr as Types

-- | A list of IP address ranges (<https://docs.aws.amazon.com/vpc/latest/userguide/VPC_Subnets.html CIDRs> ). Used to create an allow list of IP addresses for a private workforce. Workers will only be able to login to their worker portal from an IP address within this range. By default, a workforce isn't restricted to specific IP addresses.
--
-- /See:/ 'mkSourceIpConfig' smart constructor.
newtype SourceIpConfig = SourceIpConfig'
  { cidrs :: [Types.Cidr]
    -- ^ A list of one to ten <https://docs.aws.amazon.com/vpc/latest/userguide/VPC_Subnets.html Classless Inter-Domain Routing> (CIDR) values.
--
-- Maximum: Ten CIDR values
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'SourceIpConfig' value with any optional fields omitted.
mkSourceIpConfig
    :: SourceIpConfig
mkSourceIpConfig = SourceIpConfig'{cidrs = Core.mempty}

-- | A list of one to ten <https://docs.aws.amazon.com/vpc/latest/userguide/VPC_Subnets.html Classless Inter-Domain Routing> (CIDR) values.
--
-- Maximum: Ten CIDR values
--
-- /Note:/ Consider using 'cidrs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sicCidrs :: Lens.Lens' SourceIpConfig [Types.Cidr]
sicCidrs = Lens.field @"cidrs"
{-# INLINEABLE sicCidrs #-}
{-# DEPRECATED cidrs "Use generic-lens or generic-optics with 'cidrs' instead"  #-}

instance Core.FromJSON SourceIpConfig where
        toJSON SourceIpConfig{..}
          = Core.object (Core.catMaybes [Core.Just ("Cidrs" Core..= cidrs)])

instance Core.FromJSON SourceIpConfig where
        parseJSON
          = Core.withObject "SourceIpConfig" Core.$
              \ x ->
                SourceIpConfig' Core.<$> (x Core..:? "Cidrs" Core..!= Core.mempty)
