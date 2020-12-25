{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.DeleteSuggester
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a suggester. For more information, see <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/getting-suggestions.html Getting Search Suggestions> in the /Amazon CloudSearch Developer Guide/ .
module Network.AWS.CloudSearch.DeleteSuggester
  ( -- * Creating a request
    DeleteSuggester (..),
    mkDeleteSuggester,

    -- ** Request lenses
    dsgDomainName,
    dsgSuggesterName,

    -- * Destructuring the response
    DeleteSuggesterResponse (..),
    mkDeleteSuggesterResponse,

    -- ** Response lenses
    dsrgrsSuggester,
    dsrgrsResponseStatus,
  )
where

import qualified Network.AWS.CloudSearch.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Container for the parameters to the @'DeleteSuggester' @ operation. Specifies the name of the domain you want to update and name of the suggester you want to delete.
--
-- /See:/ 'mkDeleteSuggester' smart constructor.
data DeleteSuggester = DeleteSuggester'
  { domainName :: Types.DomainName,
    -- | Specifies the name of the suggester you want to delete.
    suggesterName :: Types.SuggesterName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteSuggester' value with any optional fields omitted.
mkDeleteSuggester ::
  -- | 'domainName'
  Types.DomainName ->
  -- | 'suggesterName'
  Types.SuggesterName ->
  DeleteSuggester
mkDeleteSuggester domainName suggesterName =
  DeleteSuggester' {domainName, suggesterName}

-- | Undocumented field.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsgDomainName :: Lens.Lens' DeleteSuggester Types.DomainName
dsgDomainName = Lens.field @"domainName"
{-# DEPRECATED dsgDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

-- | Specifies the name of the suggester you want to delete.
--
-- /Note:/ Consider using 'suggesterName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsgSuggesterName :: Lens.Lens' DeleteSuggester Types.SuggesterName
dsgSuggesterName = Lens.field @"suggesterName"
{-# DEPRECATED dsgSuggesterName "Use generic-lens or generic-optics with 'suggesterName' instead." #-}

instance Core.AWSRequest DeleteSuggester where
  type Rs DeleteSuggester = DeleteSuggesterResponse
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
            ( Core.pure ("Action", "DeleteSuggester")
                Core.<> (Core.pure ("Version", "2013-01-01"))
                Core.<> (Core.toQueryValue "DomainName" domainName)
                Core.<> (Core.toQueryValue "SuggesterName" suggesterName)
            )
      }
  response =
    Response.receiveXMLWrapper
      "DeleteSuggesterResult"
      ( \s h x ->
          DeleteSuggesterResponse'
            Core.<$> (x Core..@ "Suggester") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | The result of a @DeleteSuggester@ request. Contains the status of the deleted suggester.
--
-- /See:/ 'mkDeleteSuggesterResponse' smart constructor.
data DeleteSuggesterResponse = DeleteSuggesterResponse'
  { -- | The status of the suggester being deleted.
    suggester :: Types.SuggesterStatus,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DeleteSuggesterResponse' value with any optional fields omitted.
mkDeleteSuggesterResponse ::
  -- | 'suggester'
  Types.SuggesterStatus ->
  -- | 'responseStatus'
  Core.Int ->
  DeleteSuggesterResponse
mkDeleteSuggesterResponse suggester responseStatus =
  DeleteSuggesterResponse' {suggester, responseStatus}

-- | The status of the suggester being deleted.
--
-- /Note:/ Consider using 'suggester' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrgrsSuggester :: Lens.Lens' DeleteSuggesterResponse Types.SuggesterStatus
dsrgrsSuggester = Lens.field @"suggester"
{-# DEPRECATED dsrgrsSuggester "Use generic-lens or generic-optics with 'suggester' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrgrsResponseStatus :: Lens.Lens' DeleteSuggesterResponse Core.Int
dsrgrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dsrgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
