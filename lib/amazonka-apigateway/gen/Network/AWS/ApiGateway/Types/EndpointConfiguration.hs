{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.Types.EndpointConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ApiGateway.Types.EndpointConfiguration
  ( EndpointConfiguration (..)
  -- * Smart constructor
  , mkEndpointConfiguration
  -- * Lenses
  , ecTypes
  , ecVpcEndpointIds
  ) where

import qualified Network.AWS.ApiGateway.Types.EndpointType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The endpoint configuration to indicate the types of endpoints an API ('RestApi' ) or its custom domain name ('DomainName' ) has. 
--
-- /See:/ 'mkEndpointConfiguration' smart constructor.
data EndpointConfiguration = EndpointConfiguration'
  { types :: Core.Maybe [Types.EndpointType]
    -- ^ A list of endpoint types of an API ('RestApi' ) or its custom domain name ('DomainName' ). For an edge-optimized API and its custom domain name, the endpoint type is @"EDGE"@ . For a regional API and its custom domain name, the endpoint type is @REGIONAL@ . For a private API, the endpoint type is @PRIVATE@ .
  , vpcEndpointIds :: Core.Maybe [Core.Text]
    -- ^ A list of VpcEndpointIds of an API ('RestApi' ) against which to create Route53 ALIASes. It is only supported for @PRIVATE@ endpoint type.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EndpointConfiguration' value with any optional fields omitted.
mkEndpointConfiguration
    :: EndpointConfiguration
mkEndpointConfiguration
  = EndpointConfiguration'{types = Core.Nothing,
                           vpcEndpointIds = Core.Nothing}

-- | A list of endpoint types of an API ('RestApi' ) or its custom domain name ('DomainName' ). For an edge-optimized API and its custom domain name, the endpoint type is @"EDGE"@ . For a regional API and its custom domain name, the endpoint type is @REGIONAL@ . For a private API, the endpoint type is @PRIVATE@ .
--
-- /Note:/ Consider using 'types' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecTypes :: Lens.Lens' EndpointConfiguration (Core.Maybe [Types.EndpointType])
ecTypes = Lens.field @"types"
{-# INLINEABLE ecTypes #-}
{-# DEPRECATED types "Use generic-lens or generic-optics with 'types' instead"  #-}

-- | A list of VpcEndpointIds of an API ('RestApi' ) against which to create Route53 ALIASes. It is only supported for @PRIVATE@ endpoint type.
--
-- /Note:/ Consider using 'vpcEndpointIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecVpcEndpointIds :: Lens.Lens' EndpointConfiguration (Core.Maybe [Core.Text])
ecVpcEndpointIds = Lens.field @"vpcEndpointIds"
{-# INLINEABLE ecVpcEndpointIds #-}
{-# DEPRECATED vpcEndpointIds "Use generic-lens or generic-optics with 'vpcEndpointIds' instead"  #-}

instance Core.FromJSON EndpointConfiguration where
        toJSON EndpointConfiguration{..}
          = Core.object
              (Core.catMaybes
                 [("types" Core..=) Core.<$> types,
                  ("vpcEndpointIds" Core..=) Core.<$> vpcEndpointIds])

instance Core.FromJSON EndpointConfiguration where
        parseJSON
          = Core.withObject "EndpointConfiguration" Core.$
              \ x ->
                EndpointConfiguration' Core.<$>
                  (x Core..:? "types") Core.<*> x Core..:? "vpcEndpointIds"
