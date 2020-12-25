{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.InstanceMetadataOptionsResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.InstanceMetadataOptionsResponse
  ( InstanceMetadataOptionsResponse (..),

    -- * Smart constructor
    mkInstanceMetadataOptionsResponse,

    -- * Lenses
    imorHttpEndpoint,
    imorHttpPutResponseHopLimit,
    imorHttpTokens,
    imorState,
  )
where

import qualified Network.AWS.EC2.Types.HttpTokensState as Types
import qualified Network.AWS.EC2.Types.InstanceMetadataEndpointState as Types
import qualified Network.AWS.EC2.Types.InstanceMetadataOptionsState as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The metadata options for the instance.
--
-- /See:/ 'mkInstanceMetadataOptionsResponse' smart constructor.
data InstanceMetadataOptionsResponse = InstanceMetadataOptionsResponse'
  { -- | This parameter enables or disables the HTTP metadata endpoint on your instances. If the parameter is not specified, the default state is @enabled@ .
    httpEndpoint :: Core.Maybe Types.InstanceMetadataEndpointState,
    -- | The desired HTTP PUT response hop limit for instance metadata requests. The larger the number, the further instance metadata requests can travel.
    --
    -- Default: 1
    -- Possible values: Integers from 1 to 64
    httpPutResponseHopLimit :: Core.Maybe Core.Int,
    -- | The state of token usage for your instance metadata requests. If the parameter is not specified in the request, the default state is @optional@ .
    --
    -- If the state is @optional@ , you can choose to retrieve instance metadata with or without a signed token header on your request. If you retrieve the IAM role credentials without a token, the version 1.0 role credentials are returned. If you retrieve the IAM role credentials using a valid signed token, the version 2.0 role credentials are returned.
    -- If the state is @required@ , you must send a signed token header with any instance metadata retrieval requests. In this state, retrieving the IAM role credential always returns the version 2.0 credentials; the version 1.0 credentials are not available.
    httpTokens :: Core.Maybe Types.HttpTokensState,
    -- | The state of the metadata option changes.
    --
    -- @pending@ - The metadata options are being updated and the instance is not ready to process metadata traffic with the new selection.
    -- @applied@ - The metadata options have been successfully applied on the instance.
    state :: Core.Maybe Types.InstanceMetadataOptionsState
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InstanceMetadataOptionsResponse' value with any optional fields omitted.
mkInstanceMetadataOptionsResponse ::
  InstanceMetadataOptionsResponse
mkInstanceMetadataOptionsResponse =
  InstanceMetadataOptionsResponse'
    { httpEndpoint = Core.Nothing,
      httpPutResponseHopLimit = Core.Nothing,
      httpTokens = Core.Nothing,
      state = Core.Nothing
    }

-- | This parameter enables or disables the HTTP metadata endpoint on your instances. If the parameter is not specified, the default state is @enabled@ .
--
-- /Note:/ Consider using 'httpEndpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
imorHttpEndpoint :: Lens.Lens' InstanceMetadataOptionsResponse (Core.Maybe Types.InstanceMetadataEndpointState)
imorHttpEndpoint = Lens.field @"httpEndpoint"
{-# DEPRECATED imorHttpEndpoint "Use generic-lens or generic-optics with 'httpEndpoint' instead." #-}

-- | The desired HTTP PUT response hop limit for instance metadata requests. The larger the number, the further instance metadata requests can travel.
--
-- Default: 1
-- Possible values: Integers from 1 to 64
--
-- /Note:/ Consider using 'httpPutResponseHopLimit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
imorHttpPutResponseHopLimit :: Lens.Lens' InstanceMetadataOptionsResponse (Core.Maybe Core.Int)
imorHttpPutResponseHopLimit = Lens.field @"httpPutResponseHopLimit"
{-# DEPRECATED imorHttpPutResponseHopLimit "Use generic-lens or generic-optics with 'httpPutResponseHopLimit' instead." #-}

-- | The state of token usage for your instance metadata requests. If the parameter is not specified in the request, the default state is @optional@ .
--
-- If the state is @optional@ , you can choose to retrieve instance metadata with or without a signed token header on your request. If you retrieve the IAM role credentials without a token, the version 1.0 role credentials are returned. If you retrieve the IAM role credentials using a valid signed token, the version 2.0 role credentials are returned.
-- If the state is @required@ , you must send a signed token header with any instance metadata retrieval requests. In this state, retrieving the IAM role credential always returns the version 2.0 credentials; the version 1.0 credentials are not available.
--
-- /Note:/ Consider using 'httpTokens' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
imorHttpTokens :: Lens.Lens' InstanceMetadataOptionsResponse (Core.Maybe Types.HttpTokensState)
imorHttpTokens = Lens.field @"httpTokens"
{-# DEPRECATED imorHttpTokens "Use generic-lens or generic-optics with 'httpTokens' instead." #-}

-- | The state of the metadata option changes.
--
-- @pending@ - The metadata options are being updated and the instance is not ready to process metadata traffic with the new selection.
-- @applied@ - The metadata options have been successfully applied on the instance.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
imorState :: Lens.Lens' InstanceMetadataOptionsResponse (Core.Maybe Types.InstanceMetadataOptionsState)
imorState = Lens.field @"state"
{-# DEPRECATED imorState "Use generic-lens or generic-optics with 'state' instead." #-}

instance Core.FromXML InstanceMetadataOptionsResponse where
  parseXML x =
    InstanceMetadataOptionsResponse'
      Core.<$> (x Core..@? "httpEndpoint")
      Core.<*> (x Core..@? "httpPutResponseHopLimit")
      Core.<*> (x Core..@? "httpTokens")
      Core.<*> (x Core..@? "state")
