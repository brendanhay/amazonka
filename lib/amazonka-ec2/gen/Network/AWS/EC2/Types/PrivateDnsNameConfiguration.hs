{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.PrivateDnsNameConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.PrivateDnsNameConfiguration
  ( PrivateDnsNameConfiguration (..)
  -- * Smart constructor
  , mkPrivateDnsNameConfiguration
  -- * Lenses
  , pdncName
  , pdncState
  , pdncType
  , pdncValue
  ) where

import qualified Network.AWS.EC2.Types.DnsNameState as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about the private DNS name for the service endpoint. For more information about these parameters, see <https://docs.aws.amazon.com/vpc/latest/userguide/ndpoint-services-dns-validation.html VPC Endpoint Service Private DNS Name Verification> in the /Amazon Virtual Private Cloud User Guide/ .
--
-- /See:/ 'mkPrivateDnsNameConfiguration' smart constructor.
data PrivateDnsNameConfiguration = PrivateDnsNameConfiguration'
  { name :: Core.Maybe Core.Text
    -- ^ The name of the record subdomain the service provider needs to create. The service provider adds the @value@ text to the @name@ .
  , state :: Core.Maybe Types.DnsNameState
    -- ^ The verification state of the VPC endpoint service.
--
-- >Consumers of the endpoint service can use the private name only when the state is @verified@ .
  , type' :: Core.Maybe Core.Text
    -- ^ The endpoint service verification type, for example TXT.
  , value :: Core.Maybe Core.Text
    -- ^ The value the service provider adds to the private DNS name domain record before verification.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PrivateDnsNameConfiguration' value with any optional fields omitted.
mkPrivateDnsNameConfiguration
    :: PrivateDnsNameConfiguration
mkPrivateDnsNameConfiguration
  = PrivateDnsNameConfiguration'{name = Core.Nothing,
                                 state = Core.Nothing, type' = Core.Nothing, value = Core.Nothing}

-- | The name of the record subdomain the service provider needs to create. The service provider adds the @value@ text to the @name@ .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdncName :: Lens.Lens' PrivateDnsNameConfiguration (Core.Maybe Core.Text)
pdncName = Lens.field @"name"
{-# INLINEABLE pdncName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The verification state of the VPC endpoint service.
--
-- >Consumers of the endpoint service can use the private name only when the state is @verified@ .
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdncState :: Lens.Lens' PrivateDnsNameConfiguration (Core.Maybe Types.DnsNameState)
pdncState = Lens.field @"state"
{-# INLINEABLE pdncState #-}
{-# DEPRECATED state "Use generic-lens or generic-optics with 'state' instead"  #-}

-- | The endpoint service verification type, for example TXT.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdncType :: Lens.Lens' PrivateDnsNameConfiguration (Core.Maybe Core.Text)
pdncType = Lens.field @"type'"
{-# INLINEABLE pdncType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

-- | The value the service provider adds to the private DNS name domain record before verification.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdncValue :: Lens.Lens' PrivateDnsNameConfiguration (Core.Maybe Core.Text)
pdncValue = Lens.field @"value"
{-# INLINEABLE pdncValue #-}
{-# DEPRECATED value "Use generic-lens or generic-optics with 'value' instead"  #-}

instance Core.FromXML PrivateDnsNameConfiguration where
        parseXML x
          = PrivateDnsNameConfiguration' Core.<$>
              (x Core..@? "name") Core.<*> x Core..@? "state" Core.<*>
                x Core..@? "type"
                Core.<*> x Core..@? "value"
