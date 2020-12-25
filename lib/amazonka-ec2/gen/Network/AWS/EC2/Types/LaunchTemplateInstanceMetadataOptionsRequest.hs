{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LaunchTemplateInstanceMetadataOptionsRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LaunchTemplateInstanceMetadataOptionsRequest
  ( LaunchTemplateInstanceMetadataOptionsRequest (..),

    -- * Smart constructor
    mkLaunchTemplateInstanceMetadataOptionsRequest,

    -- * Lenses
    ltimorHttpEndpoint,
    ltimorHttpPutResponseHopLimit,
    ltimorHttpTokens,
  )
where

import qualified Network.AWS.EC2.Types.LaunchTemplateHttpTokensState as Types
import qualified Network.AWS.EC2.Types.LaunchTemplateInstanceMetadataEndpointState as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The metadata options for the instance. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-instance-metadata.html Instance Metadata and User Data> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- /See:/ 'mkLaunchTemplateInstanceMetadataOptionsRequest' smart constructor.
data LaunchTemplateInstanceMetadataOptionsRequest = LaunchTemplateInstanceMetadataOptionsRequest'
  { -- | This parameter enables or disables the HTTP metadata endpoint on your instances. If the parameter is not specified, the default state is @enabled@ .
    httpEndpoint :: Core.Maybe Types.LaunchTemplateInstanceMetadataEndpointState,
    -- | The desired HTTP PUT response hop limit for instance metadata requests. The larger the number, the further instance metadata requests can travel.
    --
    -- Default: 1
    -- Possible values: Integers from 1 to 64
    httpPutResponseHopLimit :: Core.Maybe Core.Int,
    -- | The state of token usage for your instance metadata requests. If the parameter is not specified in the request, the default state is @optional@ .
    --
    -- If the state is @optional@ , you can choose to retrieve instance metadata with or without a signed token header on your request. If you retrieve the IAM role credentials without a token, the version 1.0 role credentials are returned. If you retrieve the IAM role credentials using a valid signed token, the version 2.0 role credentials are returned.
    -- If the state is @required@ , you must send a signed token header with any instance metadata retrieval requests. In this state, retrieving the IAM role credentials always returns the version 2.0 credentials; the version 1.0 credentials are not available.
    httpTokens :: Core.Maybe Types.LaunchTemplateHttpTokensState
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'LaunchTemplateInstanceMetadataOptionsRequest' value with any optional fields omitted.
mkLaunchTemplateInstanceMetadataOptionsRequest ::
  LaunchTemplateInstanceMetadataOptionsRequest
mkLaunchTemplateInstanceMetadataOptionsRequest =
  LaunchTemplateInstanceMetadataOptionsRequest'
    { httpEndpoint =
        Core.Nothing,
      httpPutResponseHopLimit = Core.Nothing,
      httpTokens = Core.Nothing
    }

-- | This parameter enables or disables the HTTP metadata endpoint on your instances. If the parameter is not specified, the default state is @enabled@ .
--
-- /Note:/ Consider using 'httpEndpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltimorHttpEndpoint :: Lens.Lens' LaunchTemplateInstanceMetadataOptionsRequest (Core.Maybe Types.LaunchTemplateInstanceMetadataEndpointState)
ltimorHttpEndpoint = Lens.field @"httpEndpoint"
{-# DEPRECATED ltimorHttpEndpoint "Use generic-lens or generic-optics with 'httpEndpoint' instead." #-}

-- | The desired HTTP PUT response hop limit for instance metadata requests. The larger the number, the further instance metadata requests can travel.
--
-- Default: 1
-- Possible values: Integers from 1 to 64
--
-- /Note:/ Consider using 'httpPutResponseHopLimit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltimorHttpPutResponseHopLimit :: Lens.Lens' LaunchTemplateInstanceMetadataOptionsRequest (Core.Maybe Core.Int)
ltimorHttpPutResponseHopLimit = Lens.field @"httpPutResponseHopLimit"
{-# DEPRECATED ltimorHttpPutResponseHopLimit "Use generic-lens or generic-optics with 'httpPutResponseHopLimit' instead." #-}

-- | The state of token usage for your instance metadata requests. If the parameter is not specified in the request, the default state is @optional@ .
--
-- If the state is @optional@ , you can choose to retrieve instance metadata with or without a signed token header on your request. If you retrieve the IAM role credentials without a token, the version 1.0 role credentials are returned. If you retrieve the IAM role credentials using a valid signed token, the version 2.0 role credentials are returned.
-- If the state is @required@ , you must send a signed token header with any instance metadata retrieval requests. In this state, retrieving the IAM role credentials always returns the version 2.0 credentials; the version 1.0 credentials are not available.
--
-- /Note:/ Consider using 'httpTokens' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltimorHttpTokens :: Lens.Lens' LaunchTemplateInstanceMetadataOptionsRequest (Core.Maybe Types.LaunchTemplateHttpTokensState)
ltimorHttpTokens = Lens.field @"httpTokens"
{-# DEPRECATED ltimorHttpTokens "Use generic-lens or generic-optics with 'httpTokens' instead." #-}
