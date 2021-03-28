{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.DeleteIndexField
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes an @'IndexField' @ from the search domain. For more information, see <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/configuring-index-fields.html Configuring Index Fields> in the /Amazon CloudSearch Developer Guide/ .
module Network.AWS.CloudSearch.DeleteIndexField
    (
    -- * Creating a request
      DeleteIndexField (..)
    , mkDeleteIndexField
    -- ** Request lenses
    , diffDomainName
    , diffIndexFieldName

    -- * Destructuring the response
    , DeleteIndexFieldResponse (..)
    , mkDeleteIndexFieldResponse
    -- ** Response lenses
    , difrrsIndexField
    , difrrsResponseStatus
    ) where

import qualified Network.AWS.CloudSearch.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Container for the parameters to the @'DeleteIndexField' @ operation. Specifies the name of the domain you want to update and the name of the index field you want to delete.
--
-- /See:/ 'mkDeleteIndexField' smart constructor.
data DeleteIndexField = DeleteIndexField'
  { domainName :: Types.DomainName
  , indexFieldName :: Types.DynamicFieldName
    -- ^ The name of the index field your want to remove from the domain's indexing options.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteIndexField' value with any optional fields omitted.
mkDeleteIndexField
    :: Types.DomainName -- ^ 'domainName'
    -> Types.DynamicFieldName -- ^ 'indexFieldName'
    -> DeleteIndexField
mkDeleteIndexField domainName indexFieldName
  = DeleteIndexField'{domainName, indexFieldName}

-- | Undocumented field.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diffDomainName :: Lens.Lens' DeleteIndexField Types.DomainName
diffDomainName = Lens.field @"domainName"
{-# INLINEABLE diffDomainName #-}
{-# DEPRECATED domainName "Use generic-lens or generic-optics with 'domainName' instead"  #-}

-- | The name of the index field your want to remove from the domain's indexing options.
--
-- /Note:/ Consider using 'indexFieldName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diffIndexFieldName :: Lens.Lens' DeleteIndexField Types.DynamicFieldName
diffIndexFieldName = Lens.field @"indexFieldName"
{-# INLINEABLE diffIndexFieldName #-}
{-# DEPRECATED indexFieldName "Use generic-lens or generic-optics with 'indexFieldName' instead"  #-}

instance Core.ToQuery DeleteIndexField where
        toQuery DeleteIndexField{..}
          = Core.toQueryPair "Action" ("DeleteIndexField" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2013-01-01" :: Core.Text)
              Core.<> Core.toQueryPair "DomainName" domainName
              Core.<> Core.toQueryPair "IndexFieldName" indexFieldName

instance Core.ToHeaders DeleteIndexField where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteIndexField where
        type Rs DeleteIndexField = DeleteIndexFieldResponse
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
          = Response.receiveXMLWrapper "DeleteIndexFieldResult"
              (\ s h x ->
                 DeleteIndexFieldResponse' Core.<$>
                   (x Core..@ "IndexField") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | The result of a @'DeleteIndexField' @ request.
--
-- /See:/ 'mkDeleteIndexFieldResponse' smart constructor.
data DeleteIndexFieldResponse = DeleteIndexFieldResponse'
  { indexField :: Types.IndexFieldStatus
    -- ^ The status of the index field being deleted.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DeleteIndexFieldResponse' value with any optional fields omitted.
mkDeleteIndexFieldResponse
    :: Types.IndexFieldStatus -- ^ 'indexField'
    -> Core.Int -- ^ 'responseStatus'
    -> DeleteIndexFieldResponse
mkDeleteIndexFieldResponse indexField responseStatus
  = DeleteIndexFieldResponse'{indexField, responseStatus}

-- | The status of the index field being deleted.
--
-- /Note:/ Consider using 'indexField' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
difrrsIndexField :: Lens.Lens' DeleteIndexFieldResponse Types.IndexFieldStatus
difrrsIndexField = Lens.field @"indexField"
{-# INLINEABLE difrrsIndexField #-}
{-# DEPRECATED indexField "Use generic-lens or generic-optics with 'indexField' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
difrrsResponseStatus :: Lens.Lens' DeleteIndexFieldResponse Core.Int
difrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE difrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
