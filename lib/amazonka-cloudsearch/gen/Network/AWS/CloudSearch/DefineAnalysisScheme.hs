{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.DefineAnalysisScheme
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Configures an analysis scheme that can be applied to a @text@ or @text-array@ field to define language-specific text processing options. For more information, see <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/configuring-analysis-schemes.html Configuring Analysis Schemes> in the /Amazon CloudSearch Developer Guide/ .
module Network.AWS.CloudSearch.DefineAnalysisScheme
    (
    -- * Creating a request
      DefineAnalysisScheme (..)
    , mkDefineAnalysisScheme
    -- ** Request lenses
    , dasfDomainName
    , dasfAnalysisScheme

    -- * Destructuring the response
    , DefineAnalysisSchemeResponse (..)
    , mkDefineAnalysisSchemeResponse
    -- ** Response lenses
    , dasrgrsAnalysisScheme
    , dasrgrsResponseStatus
    ) where

import qualified Network.AWS.CloudSearch.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Container for the parameters to the @'DefineAnalysisScheme' @ operation. Specifies the name of the domain you want to update and the analysis scheme configuration.
--
-- /See:/ 'mkDefineAnalysisScheme' smart constructor.
data DefineAnalysisScheme = DefineAnalysisScheme'
  { domainName :: Types.DomainName
  , analysisScheme :: Types.AnalysisScheme
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DefineAnalysisScheme' value with any optional fields omitted.
mkDefineAnalysisScheme
    :: Types.DomainName -- ^ 'domainName'
    -> Types.AnalysisScheme -- ^ 'analysisScheme'
    -> DefineAnalysisScheme
mkDefineAnalysisScheme domainName analysisScheme
  = DefineAnalysisScheme'{domainName, analysisScheme}

-- | Undocumented field.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dasfDomainName :: Lens.Lens' DefineAnalysisScheme Types.DomainName
dasfDomainName = Lens.field @"domainName"
{-# INLINEABLE dasfDomainName #-}
{-# DEPRECATED domainName "Use generic-lens or generic-optics with 'domainName' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'analysisScheme' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dasfAnalysisScheme :: Lens.Lens' DefineAnalysisScheme Types.AnalysisScheme
dasfAnalysisScheme = Lens.field @"analysisScheme"
{-# INLINEABLE dasfAnalysisScheme #-}
{-# DEPRECATED analysisScheme "Use generic-lens or generic-optics with 'analysisScheme' instead"  #-}

instance Core.ToQuery DefineAnalysisScheme where
        toQuery DefineAnalysisScheme{..}
          = Core.toQueryPair "Action" ("DefineAnalysisScheme" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2013-01-01" :: Core.Text)
              Core.<> Core.toQueryPair "DomainName" domainName
              Core.<> Core.toQueryPair "AnalysisScheme" analysisScheme

instance Core.ToHeaders DefineAnalysisScheme where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DefineAnalysisScheme where
        type Rs DefineAnalysisScheme = DefineAnalysisSchemeResponse
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
          = Response.receiveXMLWrapper "DefineAnalysisSchemeResult"
              (\ s h x ->
                 DefineAnalysisSchemeResponse' Core.<$>
                   (x Core..@ "AnalysisScheme") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | The result of a @'DefineAnalysisScheme' @ request. Contains the status of the newly-configured analysis scheme.
--
-- /See:/ 'mkDefineAnalysisSchemeResponse' smart constructor.
data DefineAnalysisSchemeResponse = DefineAnalysisSchemeResponse'
  { analysisScheme :: Types.AnalysisSchemeStatus
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DefineAnalysisSchemeResponse' value with any optional fields omitted.
mkDefineAnalysisSchemeResponse
    :: Types.AnalysisSchemeStatus -- ^ 'analysisScheme'
    -> Core.Int -- ^ 'responseStatus'
    -> DefineAnalysisSchemeResponse
mkDefineAnalysisSchemeResponse analysisScheme responseStatus
  = DefineAnalysisSchemeResponse'{analysisScheme, responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'analysisScheme' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dasrgrsAnalysisScheme :: Lens.Lens' DefineAnalysisSchemeResponse Types.AnalysisSchemeStatus
dasrgrsAnalysisScheme = Lens.field @"analysisScheme"
{-# INLINEABLE dasrgrsAnalysisScheme #-}
{-# DEPRECATED analysisScheme "Use generic-lens or generic-optics with 'analysisScheme' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dasrgrsResponseStatus :: Lens.Lens' DefineAnalysisSchemeResponse Core.Int
dasrgrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dasrgrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
