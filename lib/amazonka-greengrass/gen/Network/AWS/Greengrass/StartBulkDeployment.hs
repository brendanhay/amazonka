{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.StartBulkDeployment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deploys multiple groups in one operation. This action starts the bulk deployment of a specified set of group versions. Each group version deployment will be triggered with an adaptive rate that has a fixed upper limit. We recommend that you include an ''X-Amzn-Client-Token'' token in every ''StartBulkDeployment'' request. These requests are idempotent with respect to the token and the request parameters.
module Network.AWS.Greengrass.StartBulkDeployment
  ( -- * Creating a request
    StartBulkDeployment (..),
    mkStartBulkDeployment,

    -- ** Request lenses
    sbdExecutionRoleArn,
    sbdInputFileUri,
    sbdAmznClientToken,
    sbdTags,

    -- * Destructuring the response
    StartBulkDeploymentResponse (..),
    mkStartBulkDeploymentResponse,

    -- ** Response lenses
    srsBulkDeploymentArn,
    srsBulkDeploymentId,
    srsResponseStatus,
  )
where

import qualified Network.AWS.Greengrass.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkStartBulkDeployment' smart constructor.
data StartBulkDeployment = StartBulkDeployment'
  { -- | The ARN of the execution role to associate with the bulk deployment operation. This IAM role must allow the ''greengrass:CreateDeployment'' action for all group versions that are listed in the input file. This IAM role must have access to the S3 bucket containing the input file.
    executionRoleArn :: Core.Text,
    -- | The URI of the input file contained in the S3 bucket. The execution role must have ''getObject'' permissions on this bucket to access the input file. The input file is a JSON-serialized, line delimited file with UTF-8 encoding that provides a list of group and version IDs and the deployment type. This file must be less than 100 MB. Currently, AWS IoT Greengrass supports only ''NewDeployment'' deployment types.
    inputFileUri :: Core.Text,
    -- | A client token used to correlate requests and responses.
    amznClientToken :: Core.Maybe Core.Text,
    -- | Tag(s) to add to the new resource.
    tags :: Core.Maybe (Core.HashMap Core.Text Core.Text)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartBulkDeployment' value with any optional fields omitted.
mkStartBulkDeployment ::
  -- | 'executionRoleArn'
  Core.Text ->
  -- | 'inputFileUri'
  Core.Text ->
  StartBulkDeployment
mkStartBulkDeployment executionRoleArn inputFileUri =
  StartBulkDeployment'
    { executionRoleArn,
      inputFileUri,
      amznClientToken = Core.Nothing,
      tags = Core.Nothing
    }

-- | The ARN of the execution role to associate with the bulk deployment operation. This IAM role must allow the ''greengrass:CreateDeployment'' action for all group versions that are listed in the input file. This IAM role must have access to the S3 bucket containing the input file.
--
-- /Note:/ Consider using 'executionRoleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbdExecutionRoleArn :: Lens.Lens' StartBulkDeployment Core.Text
sbdExecutionRoleArn = Lens.field @"executionRoleArn"
{-# DEPRECATED sbdExecutionRoleArn "Use generic-lens or generic-optics with 'executionRoleArn' instead." #-}

-- | The URI of the input file contained in the S3 bucket. The execution role must have ''getObject'' permissions on this bucket to access the input file. The input file is a JSON-serialized, line delimited file with UTF-8 encoding that provides a list of group and version IDs and the deployment type. This file must be less than 100 MB. Currently, AWS IoT Greengrass supports only ''NewDeployment'' deployment types.
--
-- /Note:/ Consider using 'inputFileUri' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbdInputFileUri :: Lens.Lens' StartBulkDeployment Core.Text
sbdInputFileUri = Lens.field @"inputFileUri"
{-# DEPRECATED sbdInputFileUri "Use generic-lens or generic-optics with 'inputFileUri' instead." #-}

-- | A client token used to correlate requests and responses.
--
-- /Note:/ Consider using 'amznClientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbdAmznClientToken :: Lens.Lens' StartBulkDeployment (Core.Maybe Core.Text)
sbdAmznClientToken = Lens.field @"amznClientToken"
{-# DEPRECATED sbdAmznClientToken "Use generic-lens or generic-optics with 'amznClientToken' instead." #-}

-- | Tag(s) to add to the new resource.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbdTags :: Lens.Lens' StartBulkDeployment (Core.Maybe (Core.HashMap Core.Text Core.Text))
sbdTags = Lens.field @"tags"
{-# DEPRECATED sbdTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromJSON StartBulkDeployment where
  toJSON StartBulkDeployment {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ExecutionRoleArn" Core..= executionRoleArn),
            Core.Just ("InputFileUri" Core..= inputFileUri),
            ("tags" Core..=) Core.<$> tags
          ]
      )

instance Core.AWSRequest StartBulkDeployment where
  type Rs StartBulkDeployment = StartBulkDeploymentResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/greengrass/bulk/deployments",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.toHeaders "X-Amzn-Client-Token" amznClientToken
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          StartBulkDeploymentResponse'
            Core.<$> (x Core..:? "BulkDeploymentArn")
            Core.<*> (x Core..:? "BulkDeploymentId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkStartBulkDeploymentResponse' smart constructor.
data StartBulkDeploymentResponse = StartBulkDeploymentResponse'
  { -- | The ARN of the bulk deployment.
    bulkDeploymentArn :: Core.Maybe Core.Text,
    -- | The ID of the bulk deployment.
    bulkDeploymentId :: Core.Maybe Core.Text,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartBulkDeploymentResponse' value with any optional fields omitted.
mkStartBulkDeploymentResponse ::
  -- | 'responseStatus'
  Core.Int ->
  StartBulkDeploymentResponse
mkStartBulkDeploymentResponse responseStatus =
  StartBulkDeploymentResponse'
    { bulkDeploymentArn = Core.Nothing,
      bulkDeploymentId = Core.Nothing,
      responseStatus
    }

-- | The ARN of the bulk deployment.
--
-- /Note:/ Consider using 'bulkDeploymentArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsBulkDeploymentArn :: Lens.Lens' StartBulkDeploymentResponse (Core.Maybe Core.Text)
srsBulkDeploymentArn = Lens.field @"bulkDeploymentArn"
{-# DEPRECATED srsBulkDeploymentArn "Use generic-lens or generic-optics with 'bulkDeploymentArn' instead." #-}

-- | The ID of the bulk deployment.
--
-- /Note:/ Consider using 'bulkDeploymentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsBulkDeploymentId :: Lens.Lens' StartBulkDeploymentResponse (Core.Maybe Core.Text)
srsBulkDeploymentId = Lens.field @"bulkDeploymentId"
{-# DEPRECATED srsBulkDeploymentId "Use generic-lens or generic-optics with 'bulkDeploymentId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsResponseStatus :: Lens.Lens' StartBulkDeploymentResponse Core.Int
srsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED srsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
