{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.DeleteApplicationOutput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes output destination configuration from your application configuration. Amazon Kinesis Analytics will no longer write data from the corresponding in-application stream to the external output destination.
--
-- This operation requires permissions to perform the @kinesisanalytics:DeleteApplicationOutput@ action.
module Network.AWS.KinesisAnalytics.DeleteApplicationOutput
  ( -- * Creating a request
    DeleteApplicationOutput (..),
    mkDeleteApplicationOutput,

    -- ** Request lenses
    daoApplicationName,
    daoCurrentApplicationVersionId,
    daoOutputId,

    -- * Destructuring the response
    DeleteApplicationOutputResponse (..),
    mkDeleteApplicationOutputResponse,

    -- ** Response lenses
    daorrsResponseStatus,
  )
where

import qualified Network.AWS.KinesisAnalytics.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'mkDeleteApplicationOutput' smart constructor.
data DeleteApplicationOutput = DeleteApplicationOutput'
  { -- | Amazon Kinesis Analytics application name.
    applicationName :: Types.ApplicationName,
    -- | Amazon Kinesis Analytics application version. You can use the <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_DescribeApplication.html DescribeApplication> operation to get the current application version. If the version specified is not the current version, the @ConcurrentModificationException@ is returned.
    currentApplicationVersionId :: Core.Natural,
    -- | The ID of the configuration to delete. Each output configuration that is added to the application, either when the application is created or later using the <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_AddApplicationOutput.html AddApplicationOutput> operation, has a unique ID. You need to provide the ID to uniquely identify the output configuration that you want to delete from the application configuration. You can use the <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_DescribeApplication.html DescribeApplication> operation to get the specific @OutputId@ .
    outputId :: Types.OutputId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteApplicationOutput' value with any optional fields omitted.
mkDeleteApplicationOutput ::
  -- | 'applicationName'
  Types.ApplicationName ->
  -- | 'currentApplicationVersionId'
  Core.Natural ->
  -- | 'outputId'
  Types.OutputId ->
  DeleteApplicationOutput
mkDeleteApplicationOutput
  applicationName
  currentApplicationVersionId
  outputId =
    DeleteApplicationOutput'
      { applicationName,
        currentApplicationVersionId,
        outputId
      }

-- | Amazon Kinesis Analytics application name.
--
-- /Note:/ Consider using 'applicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daoApplicationName :: Lens.Lens' DeleteApplicationOutput Types.ApplicationName
daoApplicationName = Lens.field @"applicationName"
{-# DEPRECATED daoApplicationName "Use generic-lens or generic-optics with 'applicationName' instead." #-}

-- | Amazon Kinesis Analytics application version. You can use the <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_DescribeApplication.html DescribeApplication> operation to get the current application version. If the version specified is not the current version, the @ConcurrentModificationException@ is returned.
--
-- /Note:/ Consider using 'currentApplicationVersionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daoCurrentApplicationVersionId :: Lens.Lens' DeleteApplicationOutput Core.Natural
daoCurrentApplicationVersionId = Lens.field @"currentApplicationVersionId"
{-# DEPRECATED daoCurrentApplicationVersionId "Use generic-lens or generic-optics with 'currentApplicationVersionId' instead." #-}

-- | The ID of the configuration to delete. Each output configuration that is added to the application, either when the application is created or later using the <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_AddApplicationOutput.html AddApplicationOutput> operation, has a unique ID. You need to provide the ID to uniquely identify the output configuration that you want to delete from the application configuration. You can use the <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_DescribeApplication.html DescribeApplication> operation to get the specific @OutputId@ .
--
-- /Note:/ Consider using 'outputId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daoOutputId :: Lens.Lens' DeleteApplicationOutput Types.OutputId
daoOutputId = Lens.field @"outputId"
{-# DEPRECATED daoOutputId "Use generic-lens or generic-optics with 'outputId' instead." #-}

instance Core.FromJSON DeleteApplicationOutput where
  toJSON DeleteApplicationOutput {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ApplicationName" Core..= applicationName),
            Core.Just
              ( "CurrentApplicationVersionId"
                  Core..= currentApplicationVersionId
              ),
            Core.Just ("OutputId" Core..= outputId)
          ]
      )

instance Core.AWSRequest DeleteApplicationOutput where
  type Rs DeleteApplicationOutput = DeleteApplicationOutputResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "KinesisAnalytics_20150814.DeleteApplicationOutput"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteApplicationOutputResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- |
--
-- /See:/ 'mkDeleteApplicationOutputResponse' smart constructor.
newtype DeleteApplicationOutputResponse = DeleteApplicationOutputResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteApplicationOutputResponse' value with any optional fields omitted.
mkDeleteApplicationOutputResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteApplicationOutputResponse
mkDeleteApplicationOutputResponse responseStatus =
  DeleteApplicationOutputResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daorrsResponseStatus :: Lens.Lens' DeleteApplicationOutputResponse Core.Int
daorrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED daorrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
