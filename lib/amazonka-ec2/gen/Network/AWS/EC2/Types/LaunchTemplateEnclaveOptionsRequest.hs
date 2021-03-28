{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LaunchTemplateEnclaveOptionsRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.LaunchTemplateEnclaveOptionsRequest
  ( LaunchTemplateEnclaveOptionsRequest (..)
  -- * Smart constructor
  , mkLaunchTemplateEnclaveOptionsRequest
  -- * Lenses
  , lteorEnabled
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Indicates whether the instance is enabled for AWS Nitro Enclaves. For more information, see <https://docs.aws.amazon.com/enclaves/latest/user/nitro-enclave.html What is AWS Nitro Enclaves?> in the /AWS Nitro Enclaves User Guide/ .
--
-- /See:/ 'mkLaunchTemplateEnclaveOptionsRequest' smart constructor.
newtype LaunchTemplateEnclaveOptionsRequest = LaunchTemplateEnclaveOptionsRequest'
  { enabled :: Core.Maybe Core.Bool
    -- ^ To enable the instance for AWS Nitro Enclaves, set this parameter to @true@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'LaunchTemplateEnclaveOptionsRequest' value with any optional fields omitted.
mkLaunchTemplateEnclaveOptionsRequest
    :: LaunchTemplateEnclaveOptionsRequest
mkLaunchTemplateEnclaveOptionsRequest
  = LaunchTemplateEnclaveOptionsRequest'{enabled = Core.Nothing}

-- | To enable the instance for AWS Nitro Enclaves, set this parameter to @true@ .
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lteorEnabled :: Lens.Lens' LaunchTemplateEnclaveOptionsRequest (Core.Maybe Core.Bool)
lteorEnabled = Lens.field @"enabled"
{-# INLINEABLE lteorEnabled #-}
{-# DEPRECATED enabled "Use generic-lens or generic-optics with 'enabled' instead"  #-}

instance Core.ToQuery LaunchTemplateEnclaveOptionsRequest where
        toQuery LaunchTemplateEnclaveOptionsRequest{..}
          = Core.maybe Core.mempty (Core.toQueryPair "Enabled") enabled
