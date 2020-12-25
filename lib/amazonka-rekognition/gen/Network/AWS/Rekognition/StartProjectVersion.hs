{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.StartProjectVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts the running of the version of a model. Starting a model takes a while to complete. To check the current state of the model, use 'DescribeProjectVersions' .
--
-- Once the model is running, you can detect custom labels in new images by calling 'DetectCustomLabels' .
-- This operation requires permissions to perform the @rekognition:StartProjectVersion@ action.
module Network.AWS.Rekognition.StartProjectVersion
  ( -- * Creating a request
    StartProjectVersion (..),
    mkStartProjectVersion,

    -- ** Request lenses
    spvProjectVersionArn,
    spvMinInferenceUnits,

    -- * Destructuring the response
    StartProjectVersionResponse (..),
    mkStartProjectVersionResponse,

    -- ** Response lenses
    spvrrsStatus,
    spvrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Rekognition.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkStartProjectVersion' smart constructor.
data StartProjectVersion = StartProjectVersion'
  { -- | The Amazon Resource Name(ARN) of the model version that you want to start.
    projectVersionArn :: Types.ProjectVersionArn,
    -- | The minimum number of inference units to use. A single inference unit represents 1 hour of processing and can support up to 5 Transaction Pers Second (TPS). Use a higher number to increase the TPS throughput of your model. You are charged for the number of inference units that you use.
    minInferenceUnits :: Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartProjectVersion' value with any optional fields omitted.
mkStartProjectVersion ::
  -- | 'projectVersionArn'
  Types.ProjectVersionArn ->
  -- | 'minInferenceUnits'
  Core.Natural ->
  StartProjectVersion
mkStartProjectVersion projectVersionArn minInferenceUnits =
  StartProjectVersion' {projectVersionArn, minInferenceUnits}

-- | The Amazon Resource Name(ARN) of the model version that you want to start.
--
-- /Note:/ Consider using 'projectVersionArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spvProjectVersionArn :: Lens.Lens' StartProjectVersion Types.ProjectVersionArn
spvProjectVersionArn = Lens.field @"projectVersionArn"
{-# DEPRECATED spvProjectVersionArn "Use generic-lens or generic-optics with 'projectVersionArn' instead." #-}

-- | The minimum number of inference units to use. A single inference unit represents 1 hour of processing and can support up to 5 Transaction Pers Second (TPS). Use a higher number to increase the TPS throughput of your model. You are charged for the number of inference units that you use.
--
-- /Note:/ Consider using 'minInferenceUnits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spvMinInferenceUnits :: Lens.Lens' StartProjectVersion Core.Natural
spvMinInferenceUnits = Lens.field @"minInferenceUnits"
{-# DEPRECATED spvMinInferenceUnits "Use generic-lens or generic-optics with 'minInferenceUnits' instead." #-}

instance Core.FromJSON StartProjectVersion where
  toJSON StartProjectVersion {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ProjectVersionArn" Core..= projectVersionArn),
            Core.Just ("MinInferenceUnits" Core..= minInferenceUnits)
          ]
      )

instance Core.AWSRequest StartProjectVersion where
  type Rs StartProjectVersion = StartProjectVersionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "RekognitionService.StartProjectVersion")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          StartProjectVersionResponse'
            Core.<$> (x Core..:? "Status") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkStartProjectVersionResponse' smart constructor.
data StartProjectVersionResponse = StartProjectVersionResponse'
  { -- | The current running status of the model.
    status :: Core.Maybe Types.ProjectVersionStatus,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartProjectVersionResponse' value with any optional fields omitted.
mkStartProjectVersionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  StartProjectVersionResponse
mkStartProjectVersionResponse responseStatus =
  StartProjectVersionResponse'
    { status = Core.Nothing,
      responseStatus
    }

-- | The current running status of the model.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spvrrsStatus :: Lens.Lens' StartProjectVersionResponse (Core.Maybe Types.ProjectVersionStatus)
spvrrsStatus = Lens.field @"status"
{-# DEPRECATED spvrrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spvrrsResponseStatus :: Lens.Lens' StartProjectVersionResponse Core.Int
spvrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED spvrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
