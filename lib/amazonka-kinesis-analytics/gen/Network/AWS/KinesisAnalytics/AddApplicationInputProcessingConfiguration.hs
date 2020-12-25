{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.AddApplicationInputProcessingConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds an <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_InputProcessingConfiguration.html InputProcessingConfiguration> to an application. An input processor preprocesses records on the input stream before the application's SQL code executes. Currently, the only input processor available is <https://docs.aws.amazon.com/lambda/ AWS Lambda> .
module Network.AWS.KinesisAnalytics.AddApplicationInputProcessingConfiguration
  ( -- * Creating a request
    AddApplicationInputProcessingConfiguration (..),
    mkAddApplicationInputProcessingConfiguration,

    -- ** Request lenses
    aaipcApplicationName,
    aaipcCurrentApplicationVersionId,
    aaipcInputId,
    aaipcInputProcessingConfiguration,

    -- * Destructuring the response
    AddApplicationInputProcessingConfigurationResponse (..),
    mkAddApplicationInputProcessingConfigurationResponse,

    -- ** Response lenses
    aaipcrrsResponseStatus,
  )
where

import qualified Network.AWS.KinesisAnalytics.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkAddApplicationInputProcessingConfiguration' smart constructor.
data AddApplicationInputProcessingConfiguration = AddApplicationInputProcessingConfiguration'
  { -- | Name of the application to which you want to add the input processing configuration.
    applicationName :: Types.ApplicationName,
    -- | Version of the application to which you want to add the input processing configuration. You can use the <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_DescribeApplication.html DescribeApplication> operation to get the current application version. If the version specified is not the current version, the @ConcurrentModificationException@ is returned.
    currentApplicationVersionId :: Core.Natural,
    -- | The ID of the input configuration to add the input processing configuration to. You can get a list of the input IDs for an application using the <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_DescribeApplication.html DescribeApplication> operation.
    inputId :: Types.InputId,
    -- | The <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_InputProcessingConfiguration.html InputProcessingConfiguration> to add to the application.
    inputProcessingConfiguration :: Types.InputProcessingConfiguration
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AddApplicationInputProcessingConfiguration' value with any optional fields omitted.
mkAddApplicationInputProcessingConfiguration ::
  -- | 'applicationName'
  Types.ApplicationName ->
  -- | 'currentApplicationVersionId'
  Core.Natural ->
  -- | 'inputId'
  Types.InputId ->
  -- | 'inputProcessingConfiguration'
  Types.InputProcessingConfiguration ->
  AddApplicationInputProcessingConfiguration
mkAddApplicationInputProcessingConfiguration
  applicationName
  currentApplicationVersionId
  inputId
  inputProcessingConfiguration =
    AddApplicationInputProcessingConfiguration'
      { applicationName,
        currentApplicationVersionId,
        inputId,
        inputProcessingConfiguration
      }

-- | Name of the application to which you want to add the input processing configuration.
--
-- /Note:/ Consider using 'applicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aaipcApplicationName :: Lens.Lens' AddApplicationInputProcessingConfiguration Types.ApplicationName
aaipcApplicationName = Lens.field @"applicationName"
{-# DEPRECATED aaipcApplicationName "Use generic-lens or generic-optics with 'applicationName' instead." #-}

-- | Version of the application to which you want to add the input processing configuration. You can use the <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_DescribeApplication.html DescribeApplication> operation to get the current application version. If the version specified is not the current version, the @ConcurrentModificationException@ is returned.
--
-- /Note:/ Consider using 'currentApplicationVersionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aaipcCurrentApplicationVersionId :: Lens.Lens' AddApplicationInputProcessingConfiguration Core.Natural
aaipcCurrentApplicationVersionId = Lens.field @"currentApplicationVersionId"
{-# DEPRECATED aaipcCurrentApplicationVersionId "Use generic-lens or generic-optics with 'currentApplicationVersionId' instead." #-}

-- | The ID of the input configuration to add the input processing configuration to. You can get a list of the input IDs for an application using the <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_DescribeApplication.html DescribeApplication> operation.
--
-- /Note:/ Consider using 'inputId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aaipcInputId :: Lens.Lens' AddApplicationInputProcessingConfiguration Types.InputId
aaipcInputId = Lens.field @"inputId"
{-# DEPRECATED aaipcInputId "Use generic-lens or generic-optics with 'inputId' instead." #-}

-- | The <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_InputProcessingConfiguration.html InputProcessingConfiguration> to add to the application.
--
-- /Note:/ Consider using 'inputProcessingConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aaipcInputProcessingConfiguration :: Lens.Lens' AddApplicationInputProcessingConfiguration Types.InputProcessingConfiguration
aaipcInputProcessingConfiguration = Lens.field @"inputProcessingConfiguration"
{-# DEPRECATED aaipcInputProcessingConfiguration "Use generic-lens or generic-optics with 'inputProcessingConfiguration' instead." #-}

instance Core.FromJSON AddApplicationInputProcessingConfiguration where
  toJSON AddApplicationInputProcessingConfiguration {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ApplicationName" Core..= applicationName),
            Core.Just
              ( "CurrentApplicationVersionId"
                  Core..= currentApplicationVersionId
              ),
            Core.Just ("InputId" Core..= inputId),
            Core.Just
              ( "InputProcessingConfiguration"
                  Core..= inputProcessingConfiguration
              )
          ]
      )

instance Core.AWSRequest AddApplicationInputProcessingConfiguration where
  type
    Rs AddApplicationInputProcessingConfiguration =
      AddApplicationInputProcessingConfigurationResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "KinesisAnalytics_20150814.AddApplicationInputProcessingConfiguration"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          AddApplicationInputProcessingConfigurationResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkAddApplicationInputProcessingConfigurationResponse' smart constructor.
newtype AddApplicationInputProcessingConfigurationResponse = AddApplicationInputProcessingConfigurationResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'AddApplicationInputProcessingConfigurationResponse' value with any optional fields omitted.
mkAddApplicationInputProcessingConfigurationResponse ::
  -- | 'responseStatus'
  Core.Int ->
  AddApplicationInputProcessingConfigurationResponse
mkAddApplicationInputProcessingConfigurationResponse responseStatus =
  AddApplicationInputProcessingConfigurationResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aaipcrrsResponseStatus :: Lens.Lens' AddApplicationInputProcessingConfigurationResponse Core.Int
aaipcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED aaipcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
