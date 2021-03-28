{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.Types.InstanceMetadataOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AutoScaling.Types.InstanceMetadataOptions
  ( InstanceMetadataOptions (..)
  -- * Smart constructor
  , mkInstanceMetadataOptions
  -- * Lenses
  , imoHttpEndpoint
  , imoHttpPutResponseHopLimit
  , imoHttpTokens
  ) where

import qualified Network.AWS.AutoScaling.Types.InstanceMetadataEndpointState as Types
import qualified Network.AWS.AutoScaling.Types.InstanceMetadataHttpTokensState as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The metadata options for the instances. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/create-launch-config.html#launch-configurations-imds Configuring the Instance Metadata Options> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- /See:/ 'mkInstanceMetadataOptions' smart constructor.
data InstanceMetadataOptions = InstanceMetadataOptions'
  { httpEndpoint :: Core.Maybe Types.InstanceMetadataEndpointState
    -- ^ This parameter enables or disables the HTTP metadata endpoint on your instances. If the parameter is not specified, the default state is @enabled@ .
  , httpPutResponseHopLimit :: Core.Maybe Core.Natural
    -- ^ The desired HTTP PUT response hop limit for instance metadata requests. The larger the number, the further instance metadata requests can travel.
--
-- Default: 1
-- Possible values: Integers from 1 to 64
  , httpTokens :: Core.Maybe Types.InstanceMetadataHttpTokensState
    -- ^ The state of token usage for your instance metadata requests. If the parameter is not specified in the request, the default state is @optional@ .
--
-- If the state is @optional@ , you can choose to retrieve instance metadata with or without a signed token header on your request. If you retrieve the IAM role credentials without a token, the version 1.0 role credentials are returned. If you retrieve the IAM role credentials using a valid signed token, the version 2.0 role credentials are returned.
-- If the state is @required@ , you must send a signed token header with any instance metadata retrieval requests. In this state, retrieving the IAM role credentials always returns the version 2.0 credentials; the version 1.0 credentials are not available.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InstanceMetadataOptions' value with any optional fields omitted.
mkInstanceMetadataOptions
    :: InstanceMetadataOptions
mkInstanceMetadataOptions
  = InstanceMetadataOptions'{httpEndpoint = Core.Nothing,
                             httpPutResponseHopLimit = Core.Nothing, httpTokens = Core.Nothing}

-- | This parameter enables or disables the HTTP metadata endpoint on your instances. If the parameter is not specified, the default state is @enabled@ .
--
-- /Note:/ Consider using 'httpEndpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
imoHttpEndpoint :: Lens.Lens' InstanceMetadataOptions (Core.Maybe Types.InstanceMetadataEndpointState)
imoHttpEndpoint = Lens.field @"httpEndpoint"
{-# INLINEABLE imoHttpEndpoint #-}
{-# DEPRECATED httpEndpoint "Use generic-lens or generic-optics with 'httpEndpoint' instead"  #-}

-- | The desired HTTP PUT response hop limit for instance metadata requests. The larger the number, the further instance metadata requests can travel.
--
-- Default: 1
-- Possible values: Integers from 1 to 64
--
-- /Note:/ Consider using 'httpPutResponseHopLimit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
imoHttpPutResponseHopLimit :: Lens.Lens' InstanceMetadataOptions (Core.Maybe Core.Natural)
imoHttpPutResponseHopLimit = Lens.field @"httpPutResponseHopLimit"
{-# INLINEABLE imoHttpPutResponseHopLimit #-}
{-# DEPRECATED httpPutResponseHopLimit "Use generic-lens or generic-optics with 'httpPutResponseHopLimit' instead"  #-}

-- | The state of token usage for your instance metadata requests. If the parameter is not specified in the request, the default state is @optional@ .
--
-- If the state is @optional@ , you can choose to retrieve instance metadata with or without a signed token header on your request. If you retrieve the IAM role credentials without a token, the version 1.0 role credentials are returned. If you retrieve the IAM role credentials using a valid signed token, the version 2.0 role credentials are returned.
-- If the state is @required@ , you must send a signed token header with any instance metadata retrieval requests. In this state, retrieving the IAM role credentials always returns the version 2.0 credentials; the version 1.0 credentials are not available.
--
-- /Note:/ Consider using 'httpTokens' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
imoHttpTokens :: Lens.Lens' InstanceMetadataOptions (Core.Maybe Types.InstanceMetadataHttpTokensState)
imoHttpTokens = Lens.field @"httpTokens"
{-# INLINEABLE imoHttpTokens #-}
{-# DEPRECATED httpTokens "Use generic-lens or generic-optics with 'httpTokens' instead"  #-}

instance Core.ToQuery InstanceMetadataOptions where
        toQuery InstanceMetadataOptions{..}
          = Core.maybe Core.mempty (Core.toQueryPair "HttpEndpoint")
              httpEndpoint
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "HttpPutResponseHopLimit")
                httpPutResponseHopLimit
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "HttpTokens") httpTokens

instance Core.FromXML InstanceMetadataOptions where
        parseXML x
          = InstanceMetadataOptions' Core.<$>
              (x Core..@? "HttpEndpoint") Core.<*>
                x Core..@? "HttpPutResponseHopLimit"
                Core.<*> x Core..@? "HttpTokens"
