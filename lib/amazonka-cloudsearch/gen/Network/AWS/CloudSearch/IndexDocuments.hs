{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.IndexDocuments
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Tells the search domain to start indexing its documents using the latest indexing options. This operation must be invoked to activate options whose 'OptionStatus' is @RequiresIndexDocuments@ .
module Network.AWS.CloudSearch.IndexDocuments
    (
    -- * Creating a request
      IndexDocuments (..)
    , mkIndexDocuments
    -- ** Request lenses
    , idDomainName

    -- * Destructuring the response
    , IndexDocumentsResponse (..)
    , mkIndexDocumentsResponse
    -- ** Response lenses
    , idrrsFieldNames
    , idrrsResponseStatus
    ) where

import qualified Network.AWS.CloudSearch.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Container for the parameters to the @'IndexDocuments' @ operation. Specifies the name of the domain you want to re-index.
--
-- /See:/ 'mkIndexDocuments' smart constructor.
newtype IndexDocuments = IndexDocuments'
  { domainName :: Types.DomainName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'IndexDocuments' value with any optional fields omitted.
mkIndexDocuments
    :: Types.DomainName -- ^ 'domainName'
    -> IndexDocuments
mkIndexDocuments domainName = IndexDocuments'{domainName}

-- | Undocumented field.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idDomainName :: Lens.Lens' IndexDocuments Types.DomainName
idDomainName = Lens.field @"domainName"
{-# INLINEABLE idDomainName #-}
{-# DEPRECATED domainName "Use generic-lens or generic-optics with 'domainName' instead"  #-}

instance Core.ToQuery IndexDocuments where
        toQuery IndexDocuments{..}
          = Core.toQueryPair "Action" ("IndexDocuments" :: Core.Text) Core.<>
              Core.toQueryPair "Version" ("2013-01-01" :: Core.Text)
              Core.<> Core.toQueryPair "DomainName" domainName

instance Core.ToHeaders IndexDocuments where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest IndexDocuments where
        type Rs IndexDocuments = IndexDocumentsResponse
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
          = Response.receiveXMLWrapper "IndexDocumentsResult"
              (\ s h x ->
                 IndexDocumentsResponse' Core.<$>
                   (x Core..@? "FieldNames" Core..<@> Core.parseXMLList "member")
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | The result of an @IndexDocuments@ request. Contains the status of the indexing operation, including the fields being indexed.
--
-- /See:/ 'mkIndexDocumentsResponse' smart constructor.
data IndexDocumentsResponse = IndexDocumentsResponse'
  { fieldNames :: Core.Maybe [Types.FieldName]
    -- ^ The names of the fields that are currently being indexed.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'IndexDocumentsResponse' value with any optional fields omitted.
mkIndexDocumentsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> IndexDocumentsResponse
mkIndexDocumentsResponse responseStatus
  = IndexDocumentsResponse'{fieldNames = Core.Nothing,
                            responseStatus}

-- | The names of the fields that are currently being indexed.
--
-- /Note:/ Consider using 'fieldNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idrrsFieldNames :: Lens.Lens' IndexDocumentsResponse (Core.Maybe [Types.FieldName])
idrrsFieldNames = Lens.field @"fieldNames"
{-# INLINEABLE idrrsFieldNames #-}
{-# DEPRECATED fieldNames "Use generic-lens or generic-optics with 'fieldNames' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idrrsResponseStatus :: Lens.Lens' IndexDocumentsResponse Core.Int
idrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE idrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
