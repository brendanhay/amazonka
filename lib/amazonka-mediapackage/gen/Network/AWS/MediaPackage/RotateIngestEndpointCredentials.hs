{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaPackage.RotateIngestEndpointCredentials
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Rotate the IngestEndpoint's username and password, as specified by the IngestEndpoint's id.
module Network.AWS.MediaPackage.RotateIngestEndpointCredentials
    (
    -- * Creating a request
      RotateIngestEndpointCredentials (..)
    , mkRotateIngestEndpointCredentials
    -- ** Request lenses
    , riecIngestEndpointId
    , riecId

    -- * Destructuring the response
    , RotateIngestEndpointCredentialsResponse (..)
    , mkRotateIngestEndpointCredentialsResponse
    -- ** Response lenses
    , riecrrsArn
    , riecrrsDescription
    , riecrrsEgressAccessLogs
    , riecrrsHlsIngest
    , riecrrsId
    , riecrrsIngressAccessLogs
    , riecrrsTags
    , riecrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaPackage.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkRotateIngestEndpointCredentials' smart constructor.
data RotateIngestEndpointCredentials = RotateIngestEndpointCredentials'
  { ingestEndpointId :: Core.Text
    -- ^ The id of the IngestEndpoint whose credentials should be rotated
  , id :: Core.Text
    -- ^ The ID of the channel the IngestEndpoint is on.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RotateIngestEndpointCredentials' value with any optional fields omitted.
mkRotateIngestEndpointCredentials
    :: Core.Text -- ^ 'ingestEndpointId'
    -> Core.Text -- ^ 'id'
    -> RotateIngestEndpointCredentials
mkRotateIngestEndpointCredentials ingestEndpointId id
  = RotateIngestEndpointCredentials'{ingestEndpointId, id}

-- | The id of the IngestEndpoint whose credentials should be rotated
--
-- /Note:/ Consider using 'ingestEndpointId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riecIngestEndpointId :: Lens.Lens' RotateIngestEndpointCredentials Core.Text
riecIngestEndpointId = Lens.field @"ingestEndpointId"
{-# INLINEABLE riecIngestEndpointId #-}
{-# DEPRECATED ingestEndpointId "Use generic-lens or generic-optics with 'ingestEndpointId' instead"  #-}

-- | The ID of the channel the IngestEndpoint is on.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riecId :: Lens.Lens' RotateIngestEndpointCredentials Core.Text
riecId = Lens.field @"id"
{-# INLINEABLE riecId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

instance Core.ToQuery RotateIngestEndpointCredentials where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders RotateIngestEndpointCredentials where
        toHeaders RotateIngestEndpointCredentials{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON RotateIngestEndpointCredentials where
        toJSON _ = Core.Object Core.mempty

instance Core.AWSRequest RotateIngestEndpointCredentials where
        type Rs RotateIngestEndpointCredentials =
             RotateIngestEndpointCredentialsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PUT,
                         Core._rqPath =
                           "/channels/" Core.<> Core.toText id Core.<> "/ingest_endpoints/"
                             Core.<> Core.toText ingestEndpointId
                             Core.<> "/credentials",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 RotateIngestEndpointCredentialsResponse' Core.<$>
                   (x Core..:? "arn") Core.<*> x Core..:? "description" Core.<*>
                     x Core..:? "egressAccessLogs"
                     Core.<*> x Core..:? "hlsIngest"
                     Core.<*> x Core..:? "id"
                     Core.<*> x Core..:? "ingressAccessLogs"
                     Core.<*> x Core..:? "tags"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkRotateIngestEndpointCredentialsResponse' smart constructor.
data RotateIngestEndpointCredentialsResponse = RotateIngestEndpointCredentialsResponse'
  { arn :: Core.Maybe Core.Text
    -- ^ The Amazon Resource Name (ARN) assigned to the Channel.
  , description :: Core.Maybe Core.Text
    -- ^ A short text description of the Channel.
  , egressAccessLogs :: Core.Maybe Types.EgressAccessLogs
  , hlsIngest :: Core.Maybe Types.HlsIngest
  , id :: Core.Maybe Core.Text
    -- ^ The ID of the Channel.
  , ingressAccessLogs :: Core.Maybe Types.IngressAccessLogs
  , tags :: Core.Maybe (Core.HashMap Core.Text Core.Text)
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RotateIngestEndpointCredentialsResponse' value with any optional fields omitted.
mkRotateIngestEndpointCredentialsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> RotateIngestEndpointCredentialsResponse
mkRotateIngestEndpointCredentialsResponse responseStatus
  = RotateIngestEndpointCredentialsResponse'{arn = Core.Nothing,
                                             description = Core.Nothing,
                                             egressAccessLogs = Core.Nothing,
                                             hlsIngest = Core.Nothing, id = Core.Nothing,
                                             ingressAccessLogs = Core.Nothing, tags = Core.Nothing,
                                             responseStatus}

-- | The Amazon Resource Name (ARN) assigned to the Channel.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riecrrsArn :: Lens.Lens' RotateIngestEndpointCredentialsResponse (Core.Maybe Core.Text)
riecrrsArn = Lens.field @"arn"
{-# INLINEABLE riecrrsArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | A short text description of the Channel.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riecrrsDescription :: Lens.Lens' RotateIngestEndpointCredentialsResponse (Core.Maybe Core.Text)
riecrrsDescription = Lens.field @"description"
{-# INLINEABLE riecrrsDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'egressAccessLogs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riecrrsEgressAccessLogs :: Lens.Lens' RotateIngestEndpointCredentialsResponse (Core.Maybe Types.EgressAccessLogs)
riecrrsEgressAccessLogs = Lens.field @"egressAccessLogs"
{-# INLINEABLE riecrrsEgressAccessLogs #-}
{-# DEPRECATED egressAccessLogs "Use generic-lens or generic-optics with 'egressAccessLogs' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'hlsIngest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riecrrsHlsIngest :: Lens.Lens' RotateIngestEndpointCredentialsResponse (Core.Maybe Types.HlsIngest)
riecrrsHlsIngest = Lens.field @"hlsIngest"
{-# INLINEABLE riecrrsHlsIngest #-}
{-# DEPRECATED hlsIngest "Use generic-lens or generic-optics with 'hlsIngest' instead"  #-}

-- | The ID of the Channel.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riecrrsId :: Lens.Lens' RotateIngestEndpointCredentialsResponse (Core.Maybe Core.Text)
riecrrsId = Lens.field @"id"
{-# INLINEABLE riecrrsId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'ingressAccessLogs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riecrrsIngressAccessLogs :: Lens.Lens' RotateIngestEndpointCredentialsResponse (Core.Maybe Types.IngressAccessLogs)
riecrrsIngressAccessLogs = Lens.field @"ingressAccessLogs"
{-# INLINEABLE riecrrsIngressAccessLogs #-}
{-# DEPRECATED ingressAccessLogs "Use generic-lens or generic-optics with 'ingressAccessLogs' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riecrrsTags :: Lens.Lens' RotateIngestEndpointCredentialsResponse (Core.Maybe (Core.HashMap Core.Text Core.Text))
riecrrsTags = Lens.field @"tags"
{-# INLINEABLE riecrrsTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riecrrsResponseStatus :: Lens.Lens' RotateIngestEndpointCredentialsResponse Core.Int
riecrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE riecrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
