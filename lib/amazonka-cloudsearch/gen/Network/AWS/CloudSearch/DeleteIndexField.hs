{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    DeleteIndexField (..),
    mkDeleteIndexField,

    -- ** Request lenses
    diffDomainName,
    diffIndexFieldName,

    -- * Destructuring the response
    DeleteIndexFieldResponse (..),
    mkDeleteIndexFieldResponse,

    -- ** Response lenses
    difrrsIndexField,
    difrrsResponseStatus,
  )
where

import qualified Network.AWS.CloudSearch.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Container for the parameters to the @'DeleteIndexField' @ operation. Specifies the name of the domain you want to update and the name of the index field you want to delete.
--
-- /See:/ 'mkDeleteIndexField' smart constructor.
data DeleteIndexField = DeleteIndexField'
  { domainName :: Types.DomainName,
    -- | The name of the index field your want to remove from the domain's indexing options.
    indexFieldName :: Types.DynamicFieldName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteIndexField' value with any optional fields omitted.
mkDeleteIndexField ::
  -- | 'domainName'
  Types.DomainName ->
  -- | 'indexFieldName'
  Types.DynamicFieldName ->
  DeleteIndexField
mkDeleteIndexField domainName indexFieldName =
  DeleteIndexField' {domainName, indexFieldName}

-- | Undocumented field.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diffDomainName :: Lens.Lens' DeleteIndexField Types.DomainName
diffDomainName = Lens.field @"domainName"
{-# DEPRECATED diffDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

-- | The name of the index field your want to remove from the domain's indexing options.
--
-- /Note:/ Consider using 'indexFieldName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diffIndexFieldName :: Lens.Lens' DeleteIndexField Types.DynamicFieldName
diffIndexFieldName = Lens.field @"indexFieldName"
{-# DEPRECATED diffIndexFieldName "Use generic-lens or generic-optics with 'indexFieldName' instead." #-}

instance Core.AWSRequest DeleteIndexField where
  type Rs DeleteIndexField = DeleteIndexFieldResponse
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
            ( Core.pure ("Action", "DeleteIndexField")
                Core.<> (Core.pure ("Version", "2013-01-01"))
                Core.<> (Core.toQueryValue "DomainName" domainName)
                Core.<> (Core.toQueryValue "IndexFieldName" indexFieldName)
            )
      }
  response =
    Response.receiveXMLWrapper
      "DeleteIndexFieldResult"
      ( \s h x ->
          DeleteIndexFieldResponse'
            Core.<$> (x Core..@ "IndexField") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | The result of a @'DeleteIndexField' @ request.
--
-- /See:/ 'mkDeleteIndexFieldResponse' smart constructor.
data DeleteIndexFieldResponse = DeleteIndexFieldResponse'
  { -- | The status of the index field being deleted.
    indexField :: Types.IndexFieldStatus,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DeleteIndexFieldResponse' value with any optional fields omitted.
mkDeleteIndexFieldResponse ::
  -- | 'indexField'
  Types.IndexFieldStatus ->
  -- | 'responseStatus'
  Core.Int ->
  DeleteIndexFieldResponse
mkDeleteIndexFieldResponse indexField responseStatus =
  DeleteIndexFieldResponse' {indexField, responseStatus}

-- | The status of the index field being deleted.
--
-- /Note:/ Consider using 'indexField' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
difrrsIndexField :: Lens.Lens' DeleteIndexFieldResponse Types.IndexFieldStatus
difrrsIndexField = Lens.field @"indexField"
{-# DEPRECATED difrrsIndexField "Use generic-lens or generic-optics with 'indexField' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
difrrsResponseStatus :: Lens.Lens' DeleteIndexFieldResponse Core.Int
difrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED difrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
