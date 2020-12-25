{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.DeleteApplication
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified application. Amazon Kinesis Analytics halts application execution and deletes the application, including any application artifacts (such as in-application streams, reference table, and application code).
--
-- This operation requires permissions to perform the @kinesisanalytics:DeleteApplication@ action.
module Network.AWS.KinesisAnalytics.DeleteApplication
  ( -- * Creating a request
    DeleteApplication (..),
    mkDeleteApplication,

    -- ** Request lenses
    dApplicationName,
    dCreateTimestamp,

    -- * Destructuring the response
    DeleteApplicationResponse (..),
    mkDeleteApplicationResponse,

    -- ** Response lenses
    drsResponseStatus,
  )
where

import qualified Network.AWS.KinesisAnalytics.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'mkDeleteApplication' smart constructor.
data DeleteApplication = DeleteApplication'
  { -- | Name of the Amazon Kinesis Analytics application to delete.
    applicationName :: Types.ApplicationName,
    -- | You can use the @DescribeApplication@ operation to get this value.
    createTimestamp :: Core.NominalDiffTime
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DeleteApplication' value with any optional fields omitted.
mkDeleteApplication ::
  -- | 'applicationName'
  Types.ApplicationName ->
  -- | 'createTimestamp'
  Core.NominalDiffTime ->
  DeleteApplication
mkDeleteApplication applicationName createTimestamp =
  DeleteApplication' {applicationName, createTimestamp}

-- | Name of the Amazon Kinesis Analytics application to delete.
--
-- /Note:/ Consider using 'applicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dApplicationName :: Lens.Lens' DeleteApplication Types.ApplicationName
dApplicationName = Lens.field @"applicationName"
{-# DEPRECATED dApplicationName "Use generic-lens or generic-optics with 'applicationName' instead." #-}

-- | You can use the @DescribeApplication@ operation to get this value.
--
-- /Note:/ Consider using 'createTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dCreateTimestamp :: Lens.Lens' DeleteApplication Core.NominalDiffTime
dCreateTimestamp = Lens.field @"createTimestamp"
{-# DEPRECATED dCreateTimestamp "Use generic-lens or generic-optics with 'createTimestamp' instead." #-}

instance Core.FromJSON DeleteApplication where
  toJSON DeleteApplication {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ApplicationName" Core..= applicationName),
            Core.Just ("CreateTimestamp" Core..= createTimestamp)
          ]
      )

instance Core.AWSRequest DeleteApplication where
  type Rs DeleteApplication = DeleteApplicationResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "KinesisAnalytics_20150814.DeleteApplication")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteApplicationResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- |
--
-- /See:/ 'mkDeleteApplicationResponse' smart constructor.
newtype DeleteApplicationResponse = DeleteApplicationResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteApplicationResponse' value with any optional fields omitted.
mkDeleteApplicationResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteApplicationResponse
mkDeleteApplicationResponse responseStatus =
  DeleteApplicationResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DeleteApplicationResponse Core.Int
drsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED drsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
