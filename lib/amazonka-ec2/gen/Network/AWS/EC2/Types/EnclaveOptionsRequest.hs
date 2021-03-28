{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.EnclaveOptionsRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.EnclaveOptionsRequest
  ( EnclaveOptionsRequest (..)
  -- * Smart constructor
  , mkEnclaveOptionsRequest
  -- * Lenses
  , eorEnabled
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Indicates whether the instance is enabled for AWS Nitro Enclaves. For more information, see <https://docs.aws.amazon.com/enclaves/latest/user/nitro-enclave.html What is AWS Nitro Enclaves?> in the /AWS Nitro Enclaves User Guide/ .
--
-- /See:/ 'mkEnclaveOptionsRequest' smart constructor.
newtype EnclaveOptionsRequest = EnclaveOptionsRequest'
  { enabled :: Core.Maybe Core.Bool
    -- ^ To enable the instance for AWS Nitro Enclaves, set this parameter to @true@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'EnclaveOptionsRequest' value with any optional fields omitted.
mkEnclaveOptionsRequest
    :: EnclaveOptionsRequest
mkEnclaveOptionsRequest
  = EnclaveOptionsRequest'{enabled = Core.Nothing}

-- | To enable the instance for AWS Nitro Enclaves, set this parameter to @true@ .
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eorEnabled :: Lens.Lens' EnclaveOptionsRequest (Core.Maybe Core.Bool)
eorEnabled = Lens.field @"enabled"
{-# INLINEABLE eorEnabled #-}
{-# DEPRECATED enabled "Use generic-lens or generic-optics with 'enabled' instead"  #-}

instance Core.ToQuery EnclaveOptionsRequest where
        toQuery EnclaveOptionsRequest{..}
          = Core.maybe Core.mempty (Core.toQueryPair "Enabled") enabled
