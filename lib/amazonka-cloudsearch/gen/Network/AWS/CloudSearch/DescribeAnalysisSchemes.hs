{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      DescribeAnalysisSchemes (..)
    , mkDescribeAnalysisSchemes
    -- ** Request lenses
    , dassDomainName
    , dassAnalysisSchemeNames
    , dassDeployed

    -- * Destructuring the response
    , DescribeAnalysisSchemesResponse (..)
    , mkDescribeAnalysisSchemesResponse
    -- ** Response lenses
    , dasrrsAnalysisSchemes
    , dasrrsResponseStatus
    ) where

import qualified Network.AWS.CloudSearch.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Container for the parameters to the @'DescribeAnalysisSchemes' @ operation. Specifies the name of the domain you want to describe. To limit the response to particular analysis schemes, specify the names of the analysis schemes you want to describe. To show the active configuration and exclude any pending changes, set the @Deployed@ option to @true@ . 
--
-- /See:/ 'mkDescribeAnalysisSchemes' smart constructor.
data DescribeAnalysisSchemes = DescribeAnalysisSchemes'
  { domainName :: Types.DomainName
    -- ^ The name of the domain you want to describe.
  , analysisSchemeNames :: Core.Maybe [Types.StandardName]
    -- ^ The analysis schemes you want to describe.
  , deployed :: Core.Maybe Core.Bool
    -- ^ Whether to display the deployed configuration (@true@ ) or include any pending changes (@false@ ). Defaults to @false@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeAnalysisSchemes' value with any optional fields omitted.
mkDescribeAnalysisSchemes
    :: Types.DomainName -- ^ 'domainName'
    -> DescribeAnalysisSchemes
mkDescribeAnalysisSchemes domainName
  = DescribeAnalysisSchemes'{domainName,
                             analysisSchemeNames = Core.Nothing, deployed = Core.Nothing}

-- | The name of the domain you want to describe.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dassDomainName :: Lens.Lens' DescribeAnalysisSchemes Types.DomainName
dassDomainName = Lens.field @"domainName"
{-# INLINEABLE dassDomainName #-}
{-# DEPRECATED domainName "Use generic-lens or generic-optics with 'domainName' instead"  #-}

-- | The analysis schemes you want to describe.
--
-- /Note:/ Consider using 'analysisSchemeNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dassAnalysisSchemeNames :: Lens.Lens' DescribeAnalysisSchemes (Core.Maybe [Types.StandardName])
dassAnalysisSchemeNames = Lens.field @"analysisSchemeNames"
{-# INLINEABLE dassAnalysisSchemeNames #-}
{-# DEPRECATED analysisSchemeNames "Use generic-lens or generic-optics with 'analysisSchemeNames' instead"  #-}

-- | Whether to display the deployed configuration (@true@ ) or include any pending changes (@false@ ). Defaults to @false@ .
--
-- /Note:/ Consider using 'deployed' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dassDeployed :: Lens.Lens' DescribeAnalysisSchemes (Core.Maybe Core.Bool)
dassDeployed = Lens.field @"deployed"
{-# INLINEABLE dassDeployed #-}
{-# DEPRECATED deployed "Use generic-lens or generic-optics with 'deployed' instead"  #-}

instance Core.ToQuery DescribeAnalysisSchemes where
        toQuery DescribeAnalysisSchemes{..}
          = Core.toQueryPair "Action"
              ("DescribeAnalysisSchemes" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2013-01-01" :: Core.Text)
              Core.<> Core.toQueryPair "DomainName" domainName
              Core.<>
              Core.toQueryPair "AnalysisSchemeNames"
                (Core.maybe Core.mempty (Core.toQueryList "member")
                   analysisSchemeNames)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "Deployed") deployed

instance Core.ToHeaders DescribeAnalysisSchemes where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeAnalysisSchemes where
        type Rs DescribeAnalysisSchemes = DescribeAnalysisSchemesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXMLWrapper "DescribeAnalysisSchemesResult"
              (\ s h x ->
                 DescribeAnalysisSchemesResponse' Core.<$>
                   (x Core..@ "AnalysisSchemes" Core..@! Core.mempty Core..<@>
                      Core.parseXMLList "member")
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | The result of a @DescribeAnalysisSchemes@ request. Contains the analysis schemes configured for the domain specified in the request.
--
-- /See:/ 'mkDescribeAnalysisSchemesResponse' smart constructor.
data DescribeAnalysisSchemesResponse = DescribeAnalysisSchemesResponse'
  { analysisSchemes :: [Types.AnalysisSchemeStatus]
    -- ^ The analysis scheme descriptions.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeAnalysisSchemesResponse' value with any optional fields omitted.
mkDescribeAnalysisSchemesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeAnalysisSchemesResponse
mkDescribeAnalysisSchemesResponse responseStatus
  = DescribeAnalysisSchemesResponse'{analysisSchemes = Core.mempty,
                                     responseStatus}

-- | The analysis scheme descriptions.
--
-- /Note:/ Consider using 'analysisSchemes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dasrrsAnalysisSchemes :: Lens.Lens' DescribeAnalysisSchemesResponse [Types.AnalysisSchemeStatus]
dasrrsAnalysisSchemes = Lens.field @"analysisSchemes"
{-# INLINEABLE dasrrsAnalysisSchemes #-}
{-# DEPRECATED analysisSchemes "Use generic-lens or generic-optics with 'analysisSchemes' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dasrrsResponseStatus :: Lens.Lens' DescribeAnalysisSchemesResponse Core.Int
dasrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dasrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
