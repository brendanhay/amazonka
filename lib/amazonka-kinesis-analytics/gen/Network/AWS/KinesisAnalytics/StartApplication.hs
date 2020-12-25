{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.StartApplication
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts the specified Amazon Kinesis Analytics application. After creating an application, you must exclusively call this operation to start your application.
--
-- After the application starts, it begins consuming the input data, processes it, and writes the output to the configured destination.
-- The application status must be @READY@ for you to start an application. You can get the application status in the console or using the <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_DescribeApplication.html DescribeApplication> operation.
-- After you start the application, you can stop the application from processing the input by calling the <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_StopApplication.html StopApplication> operation.
-- This operation requires permissions to perform the @kinesisanalytics:StartApplication@ action.
module Network.AWS.KinesisAnalytics.StartApplication
  ( -- * Creating a request
    StartApplication (..),
    mkStartApplication,

    -- ** Request lenses
    saApplicationName,
    saInputConfigurations,

    -- * Destructuring the response
    StartApplicationResponse (..),
    mkStartApplicationResponse,

    -- ** Response lenses
    sarrsResponseStatus,
  )
where

import qualified Network.AWS.KinesisAnalytics.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'mkStartApplication' smart constructor.
data StartApplication = StartApplication'
  { -- | Name of the application.
    applicationName :: Types.ApplicationName,
    -- | Identifies the specific input, by ID, that the application starts consuming. Amazon Kinesis Analytics starts reading the streaming source associated with the input. You can also specify where in the streaming source you want Amazon Kinesis Analytics to start reading.
    inputConfigurations :: [Types.InputConfiguration]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartApplication' value with any optional fields omitted.
mkStartApplication ::
  -- | 'applicationName'
  Types.ApplicationName ->
  StartApplication
mkStartApplication applicationName =
  StartApplication'
    { applicationName,
      inputConfigurations = Core.mempty
    }

-- | Name of the application.
--
-- /Note:/ Consider using 'applicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
saApplicationName :: Lens.Lens' StartApplication Types.ApplicationName
saApplicationName = Lens.field @"applicationName"
{-# DEPRECATED saApplicationName "Use generic-lens or generic-optics with 'applicationName' instead." #-}

-- | Identifies the specific input, by ID, that the application starts consuming. Amazon Kinesis Analytics starts reading the streaming source associated with the input. You can also specify where in the streaming source you want Amazon Kinesis Analytics to start reading.
--
-- /Note:/ Consider using 'inputConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
saInputConfigurations :: Lens.Lens' StartApplication [Types.InputConfiguration]
saInputConfigurations = Lens.field @"inputConfigurations"
{-# DEPRECATED saInputConfigurations "Use generic-lens or generic-optics with 'inputConfigurations' instead." #-}

instance Core.FromJSON StartApplication where
  toJSON StartApplication {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ApplicationName" Core..= applicationName),
            Core.Just ("InputConfigurations" Core..= inputConfigurations)
          ]
      )

instance Core.AWSRequest StartApplication where
  type Rs StartApplication = StartApplicationResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "KinesisAnalytics_20150814.StartApplication")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          StartApplicationResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- |
--
-- /See:/ 'mkStartApplicationResponse' smart constructor.
newtype StartApplicationResponse = StartApplicationResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'StartApplicationResponse' value with any optional fields omitted.
mkStartApplicationResponse ::
  -- | 'responseStatus'
  Core.Int ->
  StartApplicationResponse
mkStartApplicationResponse responseStatus =
  StartApplicationResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sarrsResponseStatus :: Lens.Lens' StartApplicationResponse Core.Int
sarrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED sarrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
