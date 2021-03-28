{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      DeleteSuggester (..)
    , mkDeleteSuggester
    -- ** Request lenses
    , dsgDomainName
    , dsgSuggesterName

    -- * Destructuring the response
    , DeleteSuggesterResponse (..)
    , mkDeleteSuggesterResponse
    -- ** Response lenses
    , dsrgrsSuggester
    , dsrgrsResponseStatus
    ) where

import qualified Network.AWS.CloudSearch.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Container for the parameters to the @'DeleteSuggester' @ operation. Specifies the name of the domain you want to update and name of the suggester you want to delete.
--
-- /See:/ 'mkDeleteSuggester' smart constructor.
data DeleteSuggester = DeleteSuggester'
  { domainName :: Types.DomainName
  , suggesterName :: Types.SuggesterName
    -- ^ Specifies the name of the suggester you want to delete.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteSuggester' value with any optional fields omitted.
mkDeleteSuggester
    :: Types.DomainName -- ^ 'domainName'
    -> Types.SuggesterName -- ^ 'suggesterName'
    -> DeleteSuggester
mkDeleteSuggester domainName suggesterName
  = DeleteSuggester'{domainName, suggesterName}

-- | Undocumented field.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsgDomainName :: Lens.Lens' DeleteSuggester Types.DomainName
dsgDomainName = Lens.field @"domainName"
{-# INLINEABLE dsgDomainName #-}
{-# DEPRECATED domainName "Use generic-lens or generic-optics with 'domainName' instead"  #-}

-- | Specifies the name of the suggester you want to delete.
--
-- /Note:/ Consider using 'suggesterName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsgSuggesterName :: Lens.Lens' DeleteSuggester Types.SuggesterName
dsgSuggesterName = Lens.field @"suggesterName"
{-# INLINEABLE dsgSuggesterName #-}
{-# DEPRECATED suggesterName "Use generic-lens or generic-optics with 'suggesterName' instead"  #-}

instance Core.ToQuery DeleteSuggester where
        toQuery DeleteSuggester{..}
          = Core.toQueryPair "Action" ("DeleteSuggester" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2013-01-01" :: Core.Text)
              Core.<> Core.toQueryPair "DomainName" domainName
              Core.<> Core.toQueryPair "SuggesterName" suggesterName

instance Core.ToHeaders DeleteSuggester where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteSuggester where
        type Rs DeleteSuggester = DeleteSuggesterResponse
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
          = Response.receiveXMLWrapper "DeleteSuggesterResult"
              (\ s h x ->
                 DeleteSuggesterResponse' Core.<$>
                   (x Core..@ "Suggester") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | The result of a @DeleteSuggester@ request. Contains the status of the deleted suggester.
--
-- /See:/ 'mkDeleteSuggesterResponse' smart constructor.
data DeleteSuggesterResponse = DeleteSuggesterResponse'
  { suggester :: Types.SuggesterStatus
    -- ^ The status of the suggester being deleted.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DeleteSuggesterResponse' value with any optional fields omitted.
mkDeleteSuggesterResponse
    :: Types.SuggesterStatus -- ^ 'suggester'
    -> Core.Int -- ^ 'responseStatus'
    -> DeleteSuggesterResponse
mkDeleteSuggesterResponse suggester responseStatus
  = DeleteSuggesterResponse'{suggester, responseStatus}

-- | The status of the suggester being deleted.
--
-- /Note:/ Consider using 'suggester' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrgrsSuggester :: Lens.Lens' DeleteSuggesterResponse Types.SuggesterStatus
dsrgrsSuggester = Lens.field @"suggester"
{-# INLINEABLE dsrgrsSuggester #-}
{-# DEPRECATED suggester "Use generic-lens or generic-optics with 'suggester' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrgrsResponseStatus :: Lens.Lens' DeleteSuggesterResponse Core.Int
dsrgrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dsrgrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
