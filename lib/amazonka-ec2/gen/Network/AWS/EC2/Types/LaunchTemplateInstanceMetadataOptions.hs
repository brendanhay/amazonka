{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LaunchTemplateInstanceMetadataOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.LaunchTemplateInstanceMetadataOptions
  ( LaunchTemplateInstanceMetadataOptions (..)
  -- * Smart constructor
  , mkLaunchTemplateInstanceMetadataOptions
  -- * Lenses
  , ltimoHttpEndpoint
  , ltimoHttpPutResponseHopLimit
  , ltimoHttpTokens
  , ltimoState
  ) where

import qualified Network.AWS.EC2.Types.LaunchTemplateHttpTokensState as Types
import qualified Network.AWS.EC2.Types.LaunchTemplateInstanceMetadataEndpointState as Types
import qualified Network.AWS.EC2.Types.LaunchTemplateInstanceMetadataOptionsState as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The metadata options for the instance. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-instance-metadata.html Instance Metadata and User Data> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- /See:/ 'mkLaunchTemplateInstanceMetadataOptions' smart constructor.
data LaunchTemplateInstanceMetadataOptions = LaunchTemplateInstanceMetadataOptions'
  { httpEndpoint :: Core.Maybe Types.LaunchTemplateInstanceMetadataEndpointState
    -- ^ This parameter enables or disables the HTTP metadata endpoint on your instances. If the parameter is not specified, the default state is @enabled@ .
  , httpPutResponseHopLimit :: Core.Maybe Core.Int
    -- ^ The desired HTTP PUT response hop limit for instance metadata requests. The larger the number, the further instance metadata requests can travel.
--
-- Default: 1
-- Possible values: Integers from 1 to 64
  , httpTokens :: Core.Maybe Types.LaunchTemplateHttpTokensState
    -- ^ The state of token usage for your instance metadata requests. If the parameter is not specified in the request, the default state is @optional@ .
--
-- If the state is @optional@ , you can choose to retrieve instance metadata with or without a signed token header on your request. If you retrieve the IAM role credentials without a token, the version 1.0 role credentials are returned. If you retrieve the IAM role credentials using a valid signed token, the version 2.0 role credentials are returned.
-- If the state is @required@ , you must send a signed token header with any instance metadata retrieval requests. In this state, retrieving the IAM role credentials always returns the version 2.0 credentials; the version 1.0 credentials are not available.
  , state :: Core.Maybe Types.LaunchTemplateInstanceMetadataOptionsState
    -- ^ The state of the metadata option changes.
--
-- @pending@ - The metadata options are being updated and the instance is not ready to process metadata traffic with the new selection.
-- @applied@ - The metadata options have been successfully applied on the instance.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'LaunchTemplateInstanceMetadataOptions' value with any optional fields omitted.
mkLaunchTemplateInstanceMetadataOptions
    :: LaunchTemplateInstanceMetadataOptions
mkLaunchTemplateInstanceMetadataOptions
  = LaunchTemplateInstanceMetadataOptions'{httpEndpoint =
                                             Core.Nothing,
                                           httpPutResponseHopLimit = Core.Nothing,
                                           httpTokens = Core.Nothing, state = Core.Nothing}

-- | This parameter enables or disables the HTTP metadata endpoint on your instances. If the parameter is not specified, the default state is @enabled@ .
--
-- /Note:/ Consider using 'httpEndpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltimoHttpEndpoint :: Lens.Lens' LaunchTemplateInstanceMetadataOptions (Core.Maybe Types.LaunchTemplateInstanceMetadataEndpointState)
ltimoHttpEndpoint = Lens.field @"httpEndpoint"
{-# INLINEABLE ltimoHttpEndpoint #-}
{-# DEPRECATED httpEndpoint "Use generic-lens or generic-optics with 'httpEndpoint' instead"  #-}

-- | The desired HTTP PUT response hop limit for instance metadata requests. The larger the number, the further instance metadata requests can travel.
--
-- Default: 1
-- Possible values: Integers from 1 to 64
--
-- /Note:/ Consider using 'httpPutResponseHopLimit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltimoHttpPutResponseHopLimit :: Lens.Lens' LaunchTemplateInstanceMetadataOptions (Core.Maybe Core.Int)
ltimoHttpPutResponseHopLimit = Lens.field @"httpPutResponseHopLimit"
{-# INLINEABLE ltimoHttpPutResponseHopLimit #-}
{-# DEPRECATED httpPutResponseHopLimit "Use generic-lens or generic-optics with 'httpPutResponseHopLimit' instead"  #-}

-- | The state of token usage for your instance metadata requests. If the parameter is not specified in the request, the default state is @optional@ .
--
-- If the state is @optional@ , you can choose to retrieve instance metadata with or without a signed token header on your request. If you retrieve the IAM role credentials without a token, the version 1.0 role credentials are returned. If you retrieve the IAM role credentials using a valid signed token, the version 2.0 role credentials are returned.
-- If the state is @required@ , you must send a signed token header with any instance metadata retrieval requests. In this state, retrieving the IAM role credentials always returns the version 2.0 credentials; the version 1.0 credentials are not available.
--
-- /Note:/ Consider using 'httpTokens' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltimoHttpTokens :: Lens.Lens' LaunchTemplateInstanceMetadataOptions (Core.Maybe Types.LaunchTemplateHttpTokensState)
ltimoHttpTokens = Lens.field @"httpTokens"
{-# INLINEABLE ltimoHttpTokens #-}
{-# DEPRECATED httpTokens "Use generic-lens or generic-optics with 'httpTokens' instead"  #-}

-- | The state of the metadata option changes.
--
-- @pending@ - The metadata options are being updated and the instance is not ready to process metadata traffic with the new selection.
-- @applied@ - The metadata options have been successfully applied on the instance.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltimoState :: Lens.Lens' LaunchTemplateInstanceMetadataOptions (Core.Maybe Types.LaunchTemplateInstanceMetadataOptionsState)
ltimoState = Lens.field @"state"
{-# INLINEABLE ltimoState #-}
{-# DEPRECATED state "Use generic-lens or generic-optics with 'state' instead"  #-}

instance Core.FromXML LaunchTemplateInstanceMetadataOptions where
        parseXML x
          = LaunchTemplateInstanceMetadataOptions' Core.<$>
              (x Core..@? "httpEndpoint") Core.<*>
                x Core..@? "httpPutResponseHopLimit"
                Core.<*> x Core..@? "httpTokens"
                Core.<*> x Core..@? "state"
