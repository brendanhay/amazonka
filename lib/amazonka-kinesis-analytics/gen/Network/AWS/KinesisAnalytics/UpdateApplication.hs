{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.UpdateApplication
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing Amazon Kinesis Analytics application. Using this API, you can update application code, input configuration, and output configuration.
--
-- Note that Amazon Kinesis Analytics updates the @CurrentApplicationVersionId@ each time you update your application.
-- This operation requires permission for the @kinesisanalytics:UpdateApplication@ action.
module Network.AWS.KinesisAnalytics.UpdateApplication
  ( -- * Creating a request
    UpdateApplication (..),
    mkUpdateApplication,

    -- ** Request lenses
    uaApplicationName,
    uaCurrentApplicationVersionId,
    uaApplicationUpdate,

    -- * Destructuring the response
    UpdateApplicationResponse (..),
    mkUpdateApplicationResponse,

    -- ** Response lenses
    uarrsResponseStatus,
  )
where

import qualified Network.AWS.KinesisAnalytics.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateApplication' smart constructor.
data UpdateApplication = UpdateApplication'
  { -- | Name of the Amazon Kinesis Analytics application to update.
    applicationName :: Types.ApplicationName,
    -- | The current application version ID. You can use the <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_DescribeApplication.html DescribeApplication> operation to get this value.
    currentApplicationVersionId :: Core.Natural,
    -- | Describes application updates.
    applicationUpdate :: Types.ApplicationUpdate
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateApplication' value with any optional fields omitted.
mkUpdateApplication ::
  -- | 'applicationName'
  Types.ApplicationName ->
  -- | 'currentApplicationVersionId'
  Core.Natural ->
  -- | 'applicationUpdate'
  Types.ApplicationUpdate ->
  UpdateApplication
mkUpdateApplication
  applicationName
  currentApplicationVersionId
  applicationUpdate =
    UpdateApplication'
      { applicationName,
        currentApplicationVersionId,
        applicationUpdate
      }

-- | Name of the Amazon Kinesis Analytics application to update.
--
-- /Note:/ Consider using 'applicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaApplicationName :: Lens.Lens' UpdateApplication Types.ApplicationName
uaApplicationName = Lens.field @"applicationName"
{-# DEPRECATED uaApplicationName "Use generic-lens or generic-optics with 'applicationName' instead." #-}

-- | The current application version ID. You can use the <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_DescribeApplication.html DescribeApplication> operation to get this value.
--
-- /Note:/ Consider using 'currentApplicationVersionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaCurrentApplicationVersionId :: Lens.Lens' UpdateApplication Core.Natural
uaCurrentApplicationVersionId = Lens.field @"currentApplicationVersionId"
{-# DEPRECATED uaCurrentApplicationVersionId "Use generic-lens or generic-optics with 'currentApplicationVersionId' instead." #-}

-- | Describes application updates.
--
-- /Note:/ Consider using 'applicationUpdate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaApplicationUpdate :: Lens.Lens' UpdateApplication Types.ApplicationUpdate
uaApplicationUpdate = Lens.field @"applicationUpdate"
{-# DEPRECATED uaApplicationUpdate "Use generic-lens or generic-optics with 'applicationUpdate' instead." #-}

instance Core.FromJSON UpdateApplication where
  toJSON UpdateApplication {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ApplicationName" Core..= applicationName),
            Core.Just
              ( "CurrentApplicationVersionId"
                  Core..= currentApplicationVersionId
              ),
            Core.Just ("ApplicationUpdate" Core..= applicationUpdate)
          ]
      )

instance Core.AWSRequest UpdateApplication where
  type Rs UpdateApplication = UpdateApplicationResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "KinesisAnalytics_20150814.UpdateApplication")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateApplicationResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateApplicationResponse' smart constructor.
newtype UpdateApplicationResponse = UpdateApplicationResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateApplicationResponse' value with any optional fields omitted.
mkUpdateApplicationResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateApplicationResponse
mkUpdateApplicationResponse responseStatus =
  UpdateApplicationResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uarrsResponseStatus :: Lens.Lens' UpdateApplicationResponse Core.Int
uarrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED uarrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
