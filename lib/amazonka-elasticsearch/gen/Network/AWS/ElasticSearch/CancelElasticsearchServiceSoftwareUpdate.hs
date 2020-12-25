{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.CancelElasticsearchServiceSoftwareUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels a scheduled service software update for an Amazon ES domain. You can only perform this operation before the @AutomatedUpdateDate@ and when the @UpdateStatus@ is in the @PENDING_UPDATE@ state.
module Network.AWS.ElasticSearch.CancelElasticsearchServiceSoftwareUpdate
  ( -- * Creating a request
    CancelElasticsearchServiceSoftwareUpdate (..),
    mkCancelElasticsearchServiceSoftwareUpdate,

    -- ** Request lenses
    cessuDomainName,

    -- * Destructuring the response
    CancelElasticsearchServiceSoftwareUpdateResponse (..),
    mkCancelElasticsearchServiceSoftwareUpdateResponse,

    -- ** Response lenses
    cessurrsServiceSoftwareOptions,
    cessurrsResponseStatus,
  )
where

import qualified Network.AWS.ElasticSearch.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Container for the parameters to the @'CancelElasticsearchServiceSoftwareUpdate' @ operation. Specifies the name of the Elasticsearch domain that you wish to cancel a service software update on.
--
-- /See:/ 'mkCancelElasticsearchServiceSoftwareUpdate' smart constructor.
newtype CancelElasticsearchServiceSoftwareUpdate = CancelElasticsearchServiceSoftwareUpdate'
  { -- | The name of the domain that you want to stop the latest service software update on.
    domainName :: Types.DomainName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CancelElasticsearchServiceSoftwareUpdate' value with any optional fields omitted.
mkCancelElasticsearchServiceSoftwareUpdate ::
  -- | 'domainName'
  Types.DomainName ->
  CancelElasticsearchServiceSoftwareUpdate
mkCancelElasticsearchServiceSoftwareUpdate domainName =
  CancelElasticsearchServiceSoftwareUpdate' {domainName}

-- | The name of the domain that you want to stop the latest service software update on.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cessuDomainName :: Lens.Lens' CancelElasticsearchServiceSoftwareUpdate Types.DomainName
cessuDomainName = Lens.field @"domainName"
{-# DEPRECATED cessuDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

instance Core.FromJSON CancelElasticsearchServiceSoftwareUpdate where
  toJSON CancelElasticsearchServiceSoftwareUpdate {..} =
    Core.object
      (Core.catMaybes [Core.Just ("DomainName" Core..= domainName)])

instance Core.AWSRequest CancelElasticsearchServiceSoftwareUpdate where
  type
    Rs CancelElasticsearchServiceSoftwareUpdate =
      CancelElasticsearchServiceSoftwareUpdateResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath =
          Core.rawPath "/2015-01-01/es/serviceSoftwareUpdate/cancel",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CancelElasticsearchServiceSoftwareUpdateResponse'
            Core.<$> (x Core..:? "ServiceSoftwareOptions")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | The result of a @CancelElasticsearchServiceSoftwareUpdate@ operation. Contains the status of the update.
--
-- /See:/ 'mkCancelElasticsearchServiceSoftwareUpdateResponse' smart constructor.
data CancelElasticsearchServiceSoftwareUpdateResponse = CancelElasticsearchServiceSoftwareUpdateResponse'
  { -- | The current status of the Elasticsearch service software update.
    serviceSoftwareOptions :: Core.Maybe Types.ServiceSoftwareOptions,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'CancelElasticsearchServiceSoftwareUpdateResponse' value with any optional fields omitted.
mkCancelElasticsearchServiceSoftwareUpdateResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CancelElasticsearchServiceSoftwareUpdateResponse
mkCancelElasticsearchServiceSoftwareUpdateResponse responseStatus =
  CancelElasticsearchServiceSoftwareUpdateResponse'
    { serviceSoftwareOptions =
        Core.Nothing,
      responseStatus
    }

-- | The current status of the Elasticsearch service software update.
--
-- /Note:/ Consider using 'serviceSoftwareOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cessurrsServiceSoftwareOptions :: Lens.Lens' CancelElasticsearchServiceSoftwareUpdateResponse (Core.Maybe Types.ServiceSoftwareOptions)
cessurrsServiceSoftwareOptions = Lens.field @"serviceSoftwareOptions"
{-# DEPRECATED cessurrsServiceSoftwareOptions "Use generic-lens or generic-optics with 'serviceSoftwareOptions' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cessurrsResponseStatus :: Lens.Lens' CancelElasticsearchServiceSoftwareUpdateResponse Core.Int
cessurrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cessurrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
