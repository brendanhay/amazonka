{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.DefineIndexField
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Configures an @'IndexField' @ for the search domain. Used to create new fields and modify existing ones. You must specify the name of the domain you are configuring and an index field configuration. The index field configuration specifies a unique name, the index field type, and the options you want to configure for the field. The options you can specify depend on the @'IndexFieldType' @ . If the field exists, the new configuration replaces the old one. For more information, see <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/configuring-index-fields.html Configuring Index Fields> in the /Amazon CloudSearch Developer Guide/ . 
module Network.AWS.CloudSearch.DefineIndexField
    (
    -- * Creating a request
      DefineIndexField (..)
    , mkDefineIndexField
    -- ** Request lenses
    , difgDomainName
    , difgIndexField

    -- * Destructuring the response
    , DefineIndexFieldResponse (..)
    , mkDefineIndexFieldResponse
    -- ** Response lenses
    , difrfrsIndexField
    , difrfrsResponseStatus
    ) where

import qualified Network.AWS.CloudSearch.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Container for the parameters to the @'DefineIndexField' @ operation. Specifies the name of the domain you want to update and the index field configuration.
--
-- /See:/ 'mkDefineIndexField' smart constructor.
data DefineIndexField = DefineIndexField'
  { domainName :: Types.DomainName
  , indexField :: Types.IndexField
    -- ^ The index field and field options you want to configure. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DefineIndexField' value with any optional fields omitted.
mkDefineIndexField
    :: Types.DomainName -- ^ 'domainName'
    -> Types.IndexField -- ^ 'indexField'
    -> DefineIndexField
mkDefineIndexField domainName indexField
  = DefineIndexField'{domainName, indexField}

-- | Undocumented field.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
difgDomainName :: Lens.Lens' DefineIndexField Types.DomainName
difgDomainName = Lens.field @"domainName"
{-# INLINEABLE difgDomainName #-}
{-# DEPRECATED domainName "Use generic-lens or generic-optics with 'domainName' instead"  #-}

-- | The index field and field options you want to configure. 
--
-- /Note:/ Consider using 'indexField' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
difgIndexField :: Lens.Lens' DefineIndexField Types.IndexField
difgIndexField = Lens.field @"indexField"
{-# INLINEABLE difgIndexField #-}
{-# DEPRECATED indexField "Use generic-lens or generic-optics with 'indexField' instead"  #-}

instance Core.ToQuery DefineIndexField where
        toQuery DefineIndexField{..}
          = Core.toQueryPair "Action" ("DefineIndexField" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2013-01-01" :: Core.Text)
              Core.<> Core.toQueryPair "DomainName" domainName
              Core.<> Core.toQueryPair "IndexField" indexField

instance Core.ToHeaders DefineIndexField where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DefineIndexField where
        type Rs DefineIndexField = DefineIndexFieldResponse
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
          = Response.receiveXMLWrapper "DefineIndexFieldResult"
              (\ s h x ->
                 DefineIndexFieldResponse' Core.<$>
                   (x Core..@ "IndexField") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | The result of a @'DefineIndexField' @ request. Contains the status of the newly-configured index field.
--
-- /See:/ 'mkDefineIndexFieldResponse' smart constructor.
data DefineIndexFieldResponse = DefineIndexFieldResponse'
  { indexField :: Types.IndexFieldStatus
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DefineIndexFieldResponse' value with any optional fields omitted.
mkDefineIndexFieldResponse
    :: Types.IndexFieldStatus -- ^ 'indexField'
    -> Core.Int -- ^ 'responseStatus'
    -> DefineIndexFieldResponse
mkDefineIndexFieldResponse indexField responseStatus
  = DefineIndexFieldResponse'{indexField, responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'indexField' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
difrfrsIndexField :: Lens.Lens' DefineIndexFieldResponse Types.IndexFieldStatus
difrfrsIndexField = Lens.field @"indexField"
{-# INLINEABLE difrfrsIndexField #-}
{-# DEPRECATED indexField "Use generic-lens or generic-optics with 'indexField' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
difrfrsResponseStatus :: Lens.Lens' DefineIndexFieldResponse Core.Int
difrfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE difrfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
