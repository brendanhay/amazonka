{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SQS.ChangeMessageVisibilityBatch
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes the visibility timeout of multiple messages. This is a batch version of @'ChangeMessageVisibility' .@ The result of the action on each message is reported individually in the response. You can send up to 10 @'ChangeMessageVisibility' @ requests with each @ChangeMessageVisibilityBatch@ action.
--
-- /Important:/ Because the batch request can result in a combination of successful and unsuccessful actions, you should check for batch errors even when the call returns an HTTP status code of @200@ .
-- Some actions take lists of parameters. These lists are specified using the @param.n@ notation. Values of @n@ are integers starting from 1. For example, a parameter list with two elements looks like this:
-- @&AttributeName.1=first@
-- @&AttributeName.2=second@
module Network.AWS.SQS.ChangeMessageVisibilityBatch
  ( -- * Creating a request
    ChangeMessageVisibilityBatch (..),
    mkChangeMessageVisibilityBatch,

    -- ** Request lenses
    cmvbQueueUrl,
    cmvbEntries,

    -- * Destructuring the response
    ChangeMessageVisibilityBatchResponse (..),
    mkChangeMessageVisibilityBatchResponse,

    -- ** Response lenses
    cmvbrrsSuccessful,
    cmvbrrsFailed,
    cmvbrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SQS.Types as Types

-- |
--
-- /See:/ 'mkChangeMessageVisibilityBatch' smart constructor.
data ChangeMessageVisibilityBatch = ChangeMessageVisibilityBatch'
  { -- | The URL of the Amazon SQS queue whose messages' visibility is changed.
    --
    -- Queue URLs and names are case-sensitive.
    queueUrl :: Types.String,
    -- | A list of receipt handles of the messages for which the visibility timeout must be changed.
    entries :: [Types.ChangeMessageVisibilityBatchRequestEntry]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ChangeMessageVisibilityBatch' value with any optional fields omitted.
mkChangeMessageVisibilityBatch ::
  -- | 'queueUrl'
  Types.String ->
  ChangeMessageVisibilityBatch
mkChangeMessageVisibilityBatch queueUrl =
  ChangeMessageVisibilityBatch' {queueUrl, entries = Core.mempty}

-- | The URL of the Amazon SQS queue whose messages' visibility is changed.
--
-- Queue URLs and names are case-sensitive.
--
-- /Note:/ Consider using 'queueUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmvbQueueUrl :: Lens.Lens' ChangeMessageVisibilityBatch Types.String
cmvbQueueUrl = Lens.field @"queueUrl"
{-# DEPRECATED cmvbQueueUrl "Use generic-lens or generic-optics with 'queueUrl' instead." #-}

-- | A list of receipt handles of the messages for which the visibility timeout must be changed.
--
-- /Note:/ Consider using 'entries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmvbEntries :: Lens.Lens' ChangeMessageVisibilityBatch [Types.ChangeMessageVisibilityBatchRequestEntry]
cmvbEntries = Lens.field @"entries"
{-# DEPRECATED cmvbEntries "Use generic-lens or generic-optics with 'entries' instead." #-}

instance Core.AWSRequest ChangeMessageVisibilityBatch where
  type
    Rs ChangeMessageVisibilityBatch =
      ChangeMessageVisibilityBatchResponse
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
            ( Core.pure ("Action", "ChangeMessageVisibilityBatch")
                Core.<> (Core.pure ("Version", "2012-11-05"))
                Core.<> (Core.toQueryValue "QueueUrl" queueUrl)
                Core.<> ( Core.toQueryList
                            "ChangeMessageVisibilityBatchRequestEntry"
                            entries
                        )
            )
      }
  response =
    Response.receiveXMLWrapper
      "ChangeMessageVisibilityBatchResult"
      ( \s h x ->
          ChangeMessageVisibilityBatchResponse'
            Core.<$> ( x Core..@? "ChangeMessageVisibilityBatchResultEntry"
                         Core..@! Core.mempty
                     )
            Core.<*> (x Core..@? "BatchResultErrorEntry" Core..@! Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | For each message in the batch, the response contains a @'ChangeMessageVisibilityBatchResultEntry' @ tag if the message succeeds or a @'BatchResultErrorEntry' @ tag if the message fails.
--
-- /See:/ 'mkChangeMessageVisibilityBatchResponse' smart constructor.
data ChangeMessageVisibilityBatchResponse = ChangeMessageVisibilityBatchResponse'
  { -- | A list of @'ChangeMessageVisibilityBatchResultEntry' @ items.
    successful :: [Types.ChangeMessageVisibilityBatchResultEntry],
    -- | A list of @'BatchResultErrorEntry' @ items.
    failed :: [Types.BatchResultErrorEntry],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ChangeMessageVisibilityBatchResponse' value with any optional fields omitted.
mkChangeMessageVisibilityBatchResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ChangeMessageVisibilityBatchResponse
mkChangeMessageVisibilityBatchResponse responseStatus =
  ChangeMessageVisibilityBatchResponse'
    { successful = Core.mempty,
      failed = Core.mempty,
      responseStatus
    }

-- | A list of @'ChangeMessageVisibilityBatchResultEntry' @ items.
--
-- /Note:/ Consider using 'successful' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmvbrrsSuccessful :: Lens.Lens' ChangeMessageVisibilityBatchResponse [Types.ChangeMessageVisibilityBatchResultEntry]
cmvbrrsSuccessful = Lens.field @"successful"
{-# DEPRECATED cmvbrrsSuccessful "Use generic-lens or generic-optics with 'successful' instead." #-}

-- | A list of @'BatchResultErrorEntry' @ items.
--
-- /Note:/ Consider using 'failed' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmvbrrsFailed :: Lens.Lens' ChangeMessageVisibilityBatchResponse [Types.BatchResultErrorEntry]
cmvbrrsFailed = Lens.field @"failed"
{-# DEPRECATED cmvbrrsFailed "Use generic-lens or generic-optics with 'failed' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmvbrrsResponseStatus :: Lens.Lens' ChangeMessageVisibilityBatchResponse Core.Int
cmvbrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cmvbrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
