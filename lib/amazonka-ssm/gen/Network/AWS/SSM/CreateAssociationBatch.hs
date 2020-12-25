{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.CreateAssociationBatch
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates the specified Systems Manager document with the specified instances or targets.
--
-- When you associate a document with one or more instances using instance IDs or tags, SSM Agent running on the instance processes the document and configures the instance as specified.
-- If you associate a document with an instance that already has an associated document, the system returns the AssociationAlreadyExists exception.
module Network.AWS.SSM.CreateAssociationBatch
  ( -- * Creating a request
    CreateAssociationBatch (..),
    mkCreateAssociationBatch,

    -- ** Request lenses
    cabEntries,

    -- * Destructuring the response
    CreateAssociationBatchResponse (..),
    mkCreateAssociationBatchResponse,

    -- ** Response lenses
    cabrrsFailed,
    cabrrsSuccessful,
    cabrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SSM.Types as Types

-- | /See:/ 'mkCreateAssociationBatch' smart constructor.
newtype CreateAssociationBatch = CreateAssociationBatch'
  { -- | One or more associations.
    entries :: Core.NonEmpty Types.CreateAssociationBatchRequestEntry
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CreateAssociationBatch' value with any optional fields omitted.
mkCreateAssociationBatch ::
  -- | 'entries'
  Core.NonEmpty Types.CreateAssociationBatchRequestEntry ->
  CreateAssociationBatch
mkCreateAssociationBatch entries = CreateAssociationBatch' {entries}

-- | One or more associations.
--
-- /Note:/ Consider using 'entries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cabEntries :: Lens.Lens' CreateAssociationBatch (Core.NonEmpty Types.CreateAssociationBatchRequestEntry)
cabEntries = Lens.field @"entries"
{-# DEPRECATED cabEntries "Use generic-lens or generic-optics with 'entries' instead." #-}

instance Core.FromJSON CreateAssociationBatch where
  toJSON CreateAssociationBatch {..} =
    Core.object
      (Core.catMaybes [Core.Just ("Entries" Core..= entries)])

instance Core.AWSRequest CreateAssociationBatch where
  type Rs CreateAssociationBatch = CreateAssociationBatchResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AmazonSSM.CreateAssociationBatch")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateAssociationBatchResponse'
            Core.<$> (x Core..:? "Failed")
            Core.<*> (x Core..:? "Successful")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateAssociationBatchResponse' smart constructor.
data CreateAssociationBatchResponse = CreateAssociationBatchResponse'
  { -- | Information about the associations that failed.
    failed :: Core.Maybe [Types.FailedCreateAssociation],
    -- | Information about the associations that succeeded.
    successful :: Core.Maybe [Types.AssociationDescription],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'CreateAssociationBatchResponse' value with any optional fields omitted.
mkCreateAssociationBatchResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateAssociationBatchResponse
mkCreateAssociationBatchResponse responseStatus =
  CreateAssociationBatchResponse'
    { failed = Core.Nothing,
      successful = Core.Nothing,
      responseStatus
    }

-- | Information about the associations that failed.
--
-- /Note:/ Consider using 'failed' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cabrrsFailed :: Lens.Lens' CreateAssociationBatchResponse (Core.Maybe [Types.FailedCreateAssociation])
cabrrsFailed = Lens.field @"failed"
{-# DEPRECATED cabrrsFailed "Use generic-lens or generic-optics with 'failed' instead." #-}

-- | Information about the associations that succeeded.
--
-- /Note:/ Consider using 'successful' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cabrrsSuccessful :: Lens.Lens' CreateAssociationBatchResponse (Core.Maybe [Types.AssociationDescription])
cabrrsSuccessful = Lens.field @"successful"
{-# DEPRECATED cabrrsSuccessful "Use generic-lens or generic-optics with 'successful' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cabrrsResponseStatus :: Lens.Lens' CreateAssociationBatchResponse Core.Int
cabrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cabrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
