{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.DescribeScalingParameters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the scaling parameters configured for a domain. A domain's scaling parameters specify the desired search instance type and replication count. For more information, see <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/configuring-scaling-options.html Configuring Scaling Options> in the /Amazon CloudSearch Developer Guide/ .
module Network.AWS.CloudSearch.DescribeScalingParameters
  ( -- * Creating a request
    DescribeScalingParameters (..),
    mkDescribeScalingParameters,

    -- ** Request lenses
    dspDomainName,

    -- * Destructuring the response
    DescribeScalingParametersResponse (..),
    mkDescribeScalingParametersResponse,

    -- ** Response lenses
    dsprrsScalingParameters,
    dsprrsResponseStatus,
  )
where

import qualified Network.AWS.CloudSearch.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Container for the parameters to the @'DescribeScalingParameters' @ operation. Specifies the name of the domain you want to describe.
--
-- /See:/ 'mkDescribeScalingParameters' smart constructor.
newtype DescribeScalingParameters = DescribeScalingParameters'
  { domainName :: Types.DomainName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeScalingParameters' value with any optional fields omitted.
mkDescribeScalingParameters ::
  -- | 'domainName'
  Types.DomainName ->
  DescribeScalingParameters
mkDescribeScalingParameters domainName =
  DescribeScalingParameters' {domainName}

-- | Undocumented field.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dspDomainName :: Lens.Lens' DescribeScalingParameters Types.DomainName
dspDomainName = Lens.field @"domainName"
{-# DEPRECATED dspDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

instance Core.AWSRequest DescribeScalingParameters where
  type
    Rs DescribeScalingParameters =
      DescribeScalingParametersResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "DescribeScalingParameters")
                Core.<> (Core.pure ("Version", "2013-01-01"))
                Core.<> (Core.toQueryValue "DomainName" domainName)
            )
      }
  response =
    Response.receiveXMLWrapper
      "DescribeScalingParametersResult"
      ( \s h x ->
          DescribeScalingParametersResponse'
            Core.<$> (x Core..@ "ScalingParameters")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | The result of a @DescribeScalingParameters@ request. Contains the scaling parameters configured for the domain specified in the request.
--
-- /See:/ 'mkDescribeScalingParametersResponse' smart constructor.
data DescribeScalingParametersResponse = DescribeScalingParametersResponse'
  { scalingParameters :: Types.ScalingParametersStatus,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeScalingParametersResponse' value with any optional fields omitted.
mkDescribeScalingParametersResponse ::
  -- | 'scalingParameters'
  Types.ScalingParametersStatus ->
  -- | 'responseStatus'
  Core.Int ->
  DescribeScalingParametersResponse
mkDescribeScalingParametersResponse
  scalingParameters
  responseStatus =
    DescribeScalingParametersResponse'
      { scalingParameters,
        responseStatus
      }

-- | Undocumented field.
--
-- /Note:/ Consider using 'scalingParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsprrsScalingParameters :: Lens.Lens' DescribeScalingParametersResponse Types.ScalingParametersStatus
dsprrsScalingParameters = Lens.field @"scalingParameters"
{-# DEPRECATED dsprrsScalingParameters "Use generic-lens or generic-optics with 'scalingParameters' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsprrsResponseStatus :: Lens.Lens' DescribeScalingParametersResponse Core.Int
dsprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dsprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
