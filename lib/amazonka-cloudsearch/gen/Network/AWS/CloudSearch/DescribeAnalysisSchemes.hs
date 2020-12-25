{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.DescribeAnalysisSchemes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the analysis schemes configured for a domain. An analysis scheme defines language-specific text processing options for a @text@ field. Can be limited to specific analysis schemes by name. By default, shows all analysis schemes and includes any pending changes to the configuration. Set the @Deployed@ option to @true@ to show the active configuration and exclude pending changes. For more information, see <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/configuring-analysis-schemes.html Configuring Analysis Schemes> in the /Amazon CloudSearch Developer Guide/ .
module Network.AWS.CloudSearch.DescribeAnalysisSchemes
  ( -- * Creating a request
    DescribeAnalysisSchemes (..),
    mkDescribeAnalysisSchemes,

    -- ** Request lenses
    dassDomainName,
    dassAnalysisSchemeNames,
    dassDeployed,

    -- * Destructuring the response
    DescribeAnalysisSchemesResponse (..),
    mkDescribeAnalysisSchemesResponse,

    -- ** Response lenses
    dasrrsAnalysisSchemes,
    dasrrsResponseStatus,
  )
where

import qualified Network.AWS.CloudSearch.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Container for the parameters to the @'DescribeAnalysisSchemes' @ operation. Specifies the name of the domain you want to describe. To limit the response to particular analysis schemes, specify the names of the analysis schemes you want to describe. To show the active configuration and exclude any pending changes, set the @Deployed@ option to @true@ .
--
-- /See:/ 'mkDescribeAnalysisSchemes' smart constructor.
data DescribeAnalysisSchemes = DescribeAnalysisSchemes'
  { -- | The name of the domain you want to describe.
    domainName :: Types.DomainName,
    -- | The analysis schemes you want to describe.
    analysisSchemeNames :: Core.Maybe [Types.StandardName],
    -- | Whether to display the deployed configuration (@true@ ) or include any pending changes (@false@ ). Defaults to @false@ .
    deployed :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeAnalysisSchemes' value with any optional fields omitted.
mkDescribeAnalysisSchemes ::
  -- | 'domainName'
  Types.DomainName ->
  DescribeAnalysisSchemes
mkDescribeAnalysisSchemes domainName =
  DescribeAnalysisSchemes'
    { domainName,
      analysisSchemeNames = Core.Nothing,
      deployed = Core.Nothing
    }

-- | The name of the domain you want to describe.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dassDomainName :: Lens.Lens' DescribeAnalysisSchemes Types.DomainName
dassDomainName = Lens.field @"domainName"
{-# DEPRECATED dassDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

-- | The analysis schemes you want to describe.
--
-- /Note:/ Consider using 'analysisSchemeNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dassAnalysisSchemeNames :: Lens.Lens' DescribeAnalysisSchemes (Core.Maybe [Types.StandardName])
dassAnalysisSchemeNames = Lens.field @"analysisSchemeNames"
{-# DEPRECATED dassAnalysisSchemeNames "Use generic-lens or generic-optics with 'analysisSchemeNames' instead." #-}

-- | Whether to display the deployed configuration (@true@ ) or include any pending changes (@false@ ). Defaults to @false@ .
--
-- /Note:/ Consider using 'deployed' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dassDeployed :: Lens.Lens' DescribeAnalysisSchemes (Core.Maybe Core.Bool)
dassDeployed = Lens.field @"deployed"
{-# DEPRECATED dassDeployed "Use generic-lens or generic-optics with 'deployed' instead." #-}

instance Core.AWSRequest DescribeAnalysisSchemes where
  type Rs DescribeAnalysisSchemes = DescribeAnalysisSchemesResponse
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
            ( Core.pure ("Action", "DescribeAnalysisSchemes")
                Core.<> (Core.pure ("Version", "2013-01-01"))
                Core.<> (Core.toQueryValue "DomainName" domainName)
                Core.<> ( Core.toQueryValue
                            "AnalysisSchemeNames"
                            (Core.toQueryList "member" Core.<$> analysisSchemeNames)
                        )
                Core.<> (Core.toQueryValue "Deployed" Core.<$> deployed)
            )
      }
  response =
    Response.receiveXMLWrapper
      "DescribeAnalysisSchemesResult"
      ( \s h x ->
          DescribeAnalysisSchemesResponse'
            Core.<$> ( x Core..@? "AnalysisSchemes" Core..@! Core.mempty
                         Core..<@> Core.parseXMLList "member"
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | The result of a @DescribeAnalysisSchemes@ request. Contains the analysis schemes configured for the domain specified in the request.
--
-- /See:/ 'mkDescribeAnalysisSchemesResponse' smart constructor.
data DescribeAnalysisSchemesResponse = DescribeAnalysisSchemesResponse'
  { -- | The analysis scheme descriptions.
    analysisSchemes :: [Types.AnalysisSchemeStatus],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeAnalysisSchemesResponse' value with any optional fields omitted.
mkDescribeAnalysisSchemesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeAnalysisSchemesResponse
mkDescribeAnalysisSchemesResponse responseStatus =
  DescribeAnalysisSchemesResponse'
    { analysisSchemes = Core.mempty,
      responseStatus
    }

-- | The analysis scheme descriptions.
--
-- /Note:/ Consider using 'analysisSchemes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dasrrsAnalysisSchemes :: Lens.Lens' DescribeAnalysisSchemesResponse [Types.AnalysisSchemeStatus]
dasrrsAnalysisSchemes = Lens.field @"analysisSchemes"
{-# DEPRECATED dasrrsAnalysisSchemes "Use generic-lens or generic-optics with 'analysisSchemes' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dasrrsResponseStatus :: Lens.Lens' DescribeAnalysisSchemesResponse Core.Int
dasrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dasrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
