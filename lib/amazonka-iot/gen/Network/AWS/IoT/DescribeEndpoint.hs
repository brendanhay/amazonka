{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.DescribeEndpoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a unique endpoint specific to the AWS account making the call.
module Network.AWS.IoT.DescribeEndpoint
  ( -- * Creating a request
    DescribeEndpoint (..),
    mkDescribeEndpoint,

    -- ** Request lenses
    deEndpointType,

    -- * Destructuring the response
    DescribeEndpointResponse (..),
    mkDescribeEndpointResponse,

    -- ** Response lenses
    derrsEndpointAddress,
    derrsResponseStatus,
  )
where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the DescribeEndpoint operation.
--
-- /See:/ 'mkDescribeEndpoint' smart constructor.
newtype DescribeEndpoint = DescribeEndpoint'
  { -- | The endpoint type. Valid endpoint types include:
    --
    --
    --     * @iot:Data@ - Returns a VeriSign signed data endpoint.
    --
    --
    --
    --     * @iot:Data-ATS@ - Returns an ATS signed data endpoint.
    --
    --
    --
    --     * @iot:CredentialProvider@ - Returns an AWS IoT credentials provider API endpoint.
    --
    --
    --
    --     * @iot:Jobs@ - Returns an AWS IoT device management Jobs API endpoint.
    --
    --
    -- We strongly recommend that customers use the newer @iot:Data-ATS@ endpoint type to avoid issues related to the widespread distrust of Symantec certificate authorities.
    endpointType :: Core.Maybe Types.EndpointType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeEndpoint' value with any optional fields omitted.
mkDescribeEndpoint ::
  DescribeEndpoint
mkDescribeEndpoint = DescribeEndpoint' {endpointType = Core.Nothing}

-- | The endpoint type. Valid endpoint types include:
--
--
--     * @iot:Data@ - Returns a VeriSign signed data endpoint.
--
--
--
--     * @iot:Data-ATS@ - Returns an ATS signed data endpoint.
--
--
--
--     * @iot:CredentialProvider@ - Returns an AWS IoT credentials provider API endpoint.
--
--
--
--     * @iot:Jobs@ - Returns an AWS IoT device management Jobs API endpoint.
--
--
-- We strongly recommend that customers use the newer @iot:Data-ATS@ endpoint type to avoid issues related to the widespread distrust of Symantec certificate authorities.
--
-- /Note:/ Consider using 'endpointType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deEndpointType :: Lens.Lens' DescribeEndpoint (Core.Maybe Types.EndpointType)
deEndpointType = Lens.field @"endpointType"
{-# DEPRECATED deEndpointType "Use generic-lens or generic-optics with 'endpointType' instead." #-}

instance Core.AWSRequest DescribeEndpoint where
  type Rs DescribeEndpoint = DescribeEndpointResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath = Core.rawPath "/endpoint",
        Core._rqQuery =
          Core.toQueryValue "endpointType" Core.<$> endpointType,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeEndpointResponse'
            Core.<$> (x Core..:? "endpointAddress")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | The output from the DescribeEndpoint operation.
--
-- /See:/ 'mkDescribeEndpointResponse' smart constructor.
data DescribeEndpointResponse = DescribeEndpointResponse'
  { -- | The endpoint. The format of the endpoint is as follows: /identifier/ .iot./region/ .amazonaws.com.
    endpointAddress :: Core.Maybe Types.EndpointAddress,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeEndpointResponse' value with any optional fields omitted.
mkDescribeEndpointResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeEndpointResponse
mkDescribeEndpointResponse responseStatus =
  DescribeEndpointResponse'
    { endpointAddress = Core.Nothing,
      responseStatus
    }

-- | The endpoint. The format of the endpoint is as follows: /identifier/ .iot./region/ .amazonaws.com.
--
-- /Note:/ Consider using 'endpointAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
derrsEndpointAddress :: Lens.Lens' DescribeEndpointResponse (Core.Maybe Types.EndpointAddress)
derrsEndpointAddress = Lens.field @"endpointAddress"
{-# DEPRECATED derrsEndpointAddress "Use generic-lens or generic-optics with 'endpointAddress' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
derrsResponseStatus :: Lens.Lens' DescribeEndpointResponse Core.Int
derrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED derrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
