{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.DeleteAnalysisScheme
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an analysis scheme. For more information, see <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/configuring-analysis-schemes.html Configuring Analysis Schemes> in the /Amazon CloudSearch Developer Guide/ .
module Network.AWS.CloudSearch.DeleteAnalysisScheme
  ( -- * Creating a request
    DeleteAnalysisScheme (..),
    mkDeleteAnalysisScheme,

    -- ** Request lenses
    dasDomainName,
    dasAnalysisSchemeName,

    -- * Destructuring the response
    DeleteAnalysisSchemeResponse (..),
    mkDeleteAnalysisSchemeResponse,

    -- ** Response lenses
    dasrfrsAnalysisScheme,
    dasrfrsResponseStatus,
  )
where

import qualified Network.AWS.CloudSearch.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Container for the parameters to the @'DeleteAnalysisScheme' @ operation. Specifies the name of the domain you want to update and the analysis scheme you want to delete.
--
-- /See:/ 'mkDeleteAnalysisScheme' smart constructor.
data DeleteAnalysisScheme = DeleteAnalysisScheme'
  { domainName :: Types.DomainName,
    -- | The name of the analysis scheme you want to delete.
    analysisSchemeName :: Types.AnalysisSchemeName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteAnalysisScheme' value with any optional fields omitted.
mkDeleteAnalysisScheme ::
  -- | 'domainName'
  Types.DomainName ->
  -- | 'analysisSchemeName'
  Types.AnalysisSchemeName ->
  DeleteAnalysisScheme
mkDeleteAnalysisScheme domainName analysisSchemeName =
  DeleteAnalysisScheme' {domainName, analysisSchemeName}

-- | Undocumented field.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dasDomainName :: Lens.Lens' DeleteAnalysisScheme Types.DomainName
dasDomainName = Lens.field @"domainName"
{-# DEPRECATED dasDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

-- | The name of the analysis scheme you want to delete.
--
-- /Note:/ Consider using 'analysisSchemeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dasAnalysisSchemeName :: Lens.Lens' DeleteAnalysisScheme Types.AnalysisSchemeName
dasAnalysisSchemeName = Lens.field @"analysisSchemeName"
{-# DEPRECATED dasAnalysisSchemeName "Use generic-lens or generic-optics with 'analysisSchemeName' instead." #-}

instance Core.AWSRequest DeleteAnalysisScheme where
  type Rs DeleteAnalysisScheme = DeleteAnalysisSchemeResponse
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
            ( Core.pure ("Action", "DeleteAnalysisScheme")
                Core.<> (Core.pure ("Version", "2013-01-01"))
                Core.<> (Core.toQueryValue "DomainName" domainName)
                Core.<> (Core.toQueryValue "AnalysisSchemeName" analysisSchemeName)
            )
      }
  response =
    Response.receiveXMLWrapper
      "DeleteAnalysisSchemeResult"
      ( \s h x ->
          DeleteAnalysisSchemeResponse'
            Core.<$> (x Core..@ "AnalysisScheme")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | The result of a @DeleteAnalysisScheme@ request. Contains the status of the deleted analysis scheme.
--
-- /See:/ 'mkDeleteAnalysisSchemeResponse' smart constructor.
data DeleteAnalysisSchemeResponse = DeleteAnalysisSchemeResponse'
  { -- | The status of the analysis scheme being deleted.
    analysisScheme :: Types.AnalysisSchemeStatus,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DeleteAnalysisSchemeResponse' value with any optional fields omitted.
mkDeleteAnalysisSchemeResponse ::
  -- | 'analysisScheme'
  Types.AnalysisSchemeStatus ->
  -- | 'responseStatus'
  Core.Int ->
  DeleteAnalysisSchemeResponse
mkDeleteAnalysisSchemeResponse analysisScheme responseStatus =
  DeleteAnalysisSchemeResponse' {analysisScheme, responseStatus}

-- | The status of the analysis scheme being deleted.
--
-- /Note:/ Consider using 'analysisScheme' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dasrfrsAnalysisScheme :: Lens.Lens' DeleteAnalysisSchemeResponse Types.AnalysisSchemeStatus
dasrfrsAnalysisScheme = Lens.field @"analysisScheme"
{-# DEPRECATED dasrfrsAnalysisScheme "Use generic-lens or generic-optics with 'analysisScheme' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dasrfrsResponseStatus :: Lens.Lens' DeleteAnalysisSchemeResponse Core.Int
dasrfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dasrfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
