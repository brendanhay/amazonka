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
    sbdAmznClientToken,
    sbdExecutionRoleARN,
    sbdInputFileURI,
    sbdTags,

    -- * Destructuring the response
    StartBulkDeploymentResponse (..),
    mkStartBulkDeploymentResponse,

    -- ** Response lenses
    srsBulkDeploymentARN,
    srsBulkDeploymentId,
    srsResponseStatus,
  )
where

import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkStartBulkDeployment' smart constructor.
data StartBulkDeployment = StartBulkDeployment'
  { -- | A client token used to correlate requests and responses.
    amznClientToken :: Lude.Maybe Lude.Text,
    -- | The ARN of the execution role to associate with the bulk deployment operation. This IAM role must allow the ''greengrass:CreateDeployment'' action for all group versions that are listed in the input file. This IAM role must have access to the S3 bucket containing the input file.
    executionRoleARN :: Lude.Text,
    -- | The URI of the input file contained in the S3 bucket. The execution role must have ''getObject'' permissions on this bucket to access the input file. The input file is a JSON-serialized, line delimited file with UTF-8 encoding that provides a list of group and version IDs and the deployment type. This file must be less than 100 MB. Currently, AWS IoT Greengrass supports only ''NewDeployment'' deployment types.
    inputFileURI :: Lude.Text,
    -- | Tag(s) to add to the new resource.
    tags :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartBulkDeployment' with the minimum fields required to make a request.
--
-- * 'amznClientToken' - A client token used to correlate requests and responses.
-- * 'executionRoleARN' - The ARN of the execution role to associate with the bulk deployment operation. This IAM role must allow the ''greengrass:CreateDeployment'' action for all group versions that are listed in the input file. This IAM role must have access to the S3 bucket containing the input file.
-- * 'inputFileURI' - The URI of the input file contained in the S3 bucket. The execution role must have ''getObject'' permissions on this bucket to access the input file. The input file is a JSON-serialized, line delimited file with UTF-8 encoding that provides a list of group and version IDs and the deployment type. This file must be less than 100 MB. Currently, AWS IoT Greengrass supports only ''NewDeployment'' deployment types.
-- * 'tags' - Tag(s) to add to the new resource.
mkStartBulkDeployment ::
  -- | 'executionRoleARN'
  Lude.Text ->
  -- | 'inputFileURI'
  Lude.Text ->
  StartBulkDeployment
mkStartBulkDeployment pExecutionRoleARN_ pInputFileURI_ =
  StartBulkDeployment'
    { amznClientToken = Lude.Nothing,
      executionRoleARN = pExecutionRoleARN_,
      inputFileURI = pInputFileURI_,
      tags = Lude.Nothing
    }

-- | A client token used to correlate requests and responses.
--
-- /Note:/ Consider using 'amznClientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbdAmznClientToken :: Lens.Lens' StartBulkDeployment (Lude.Maybe Lude.Text)
sbdAmznClientToken = Lens.lens (amznClientToken :: StartBulkDeployment -> Lude.Maybe Lude.Text) (\s a -> s {amznClientToken = a} :: StartBulkDeployment)
{-# DEPRECATED sbdAmznClientToken "Use generic-lens or generic-optics with 'amznClientToken' instead." #-}

-- | The ARN of the execution role to associate with the bulk deployment operation. This IAM role must allow the ''greengrass:CreateDeployment'' action for all group versions that are listed in the input file. This IAM role must have access to the S3 bucket containing the input file.
--
-- /Note:/ Consider using 'executionRoleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbdExecutionRoleARN :: Lens.Lens' StartBulkDeployment Lude.Text
sbdExecutionRoleARN = Lens.lens (executionRoleARN :: StartBulkDeployment -> Lude.Text) (\s a -> s {executionRoleARN = a} :: StartBulkDeployment)
{-# DEPRECATED sbdExecutionRoleARN "Use generic-lens or generic-optics with 'executionRoleARN' instead." #-}

-- | The URI of the input file contained in the S3 bucket. The execution role must have ''getObject'' permissions on this bucket to access the input file. The input file is a JSON-serialized, line delimited file with UTF-8 encoding that provides a list of group and version IDs and the deployment type. This file must be less than 100 MB. Currently, AWS IoT Greengrass supports only ''NewDeployment'' deployment types.
--
-- /Note:/ Consider using 'inputFileURI' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbdInputFileURI :: Lens.Lens' StartBulkDeployment Lude.Text
sbdInputFileURI = Lens.lens (inputFileURI :: StartBulkDeployment -> Lude.Text) (\s a -> s {inputFileURI = a} :: StartBulkDeployment)
{-# DEPRECATED sbdInputFileURI "Use generic-lens or generic-optics with 'inputFileURI' instead." #-}

-- | Tag(s) to add to the new resource.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbdTags :: Lens.Lens' StartBulkDeployment (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
sbdTags = Lens.lens (tags :: StartBulkDeployment -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: StartBulkDeployment)
{-# DEPRECATED sbdTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest StartBulkDeployment where
  type Rs StartBulkDeployment = StartBulkDeploymentResponse
  request = Req.postJSON greengrassService
  response =
    Res.receiveJSON
      ( \s h x ->
          StartBulkDeploymentResponse'
            Lude.<$> (x Lude..?> "BulkDeploymentArn")
            Lude.<*> (x Lude..?> "BulkDeploymentId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StartBulkDeployment where
  toHeaders StartBulkDeployment' {..} =
    Lude.mconcat
      [ "X-Amzn-Client-Token" Lude.=# amznClientToken,
        "Content-Type"
          Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
      ]

instance Lude.ToJSON StartBulkDeployment where
  toJSON StartBulkDeployment' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ExecutionRoleArn" Lude..= executionRoleARN),
            Lude.Just ("InputFileUri" Lude..= inputFileURI),
            ("tags" Lude..=) Lude.<$> tags
          ]
      )

instance Lude.ToPath StartBulkDeployment where
  toPath = Lude.const "/greengrass/bulk/deployments"

instance Lude.ToQuery StartBulkDeployment where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkStartBulkDeploymentResponse' smart constructor.
data StartBulkDeploymentResponse = StartBulkDeploymentResponse'
  { -- | The ARN of the bulk deployment.
    bulkDeploymentARN :: Lude.Maybe Lude.Text,
    -- | The ID of the bulk deployment.
    bulkDeploymentId :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartBulkDeploymentResponse' with the minimum fields required to make a request.
--
-- * 'bulkDeploymentARN' - The ARN of the bulk deployment.
-- * 'bulkDeploymentId' - The ID of the bulk deployment.
-- * 'responseStatus' - The response status code.
mkStartBulkDeploymentResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StartBulkDeploymentResponse
mkStartBulkDeploymentResponse pResponseStatus_ =
  StartBulkDeploymentResponse'
    { bulkDeploymentARN = Lude.Nothing,
      bulkDeploymentId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ARN of the bulk deployment.
--
-- /Note:/ Consider using 'bulkDeploymentARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsBulkDeploymentARN :: Lens.Lens' StartBulkDeploymentResponse (Lude.Maybe Lude.Text)
srsBulkDeploymentARN = Lens.lens (bulkDeploymentARN :: StartBulkDeploymentResponse -> Lude.Maybe Lude.Text) (\s a -> s {bulkDeploymentARN = a} :: StartBulkDeploymentResponse)
{-# DEPRECATED srsBulkDeploymentARN "Use generic-lens or generic-optics with 'bulkDeploymentARN' instead." #-}

-- | The ID of the bulk deployment.
--
-- /Note:/ Consider using 'bulkDeploymentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsBulkDeploymentId :: Lens.Lens' StartBulkDeploymentResponse (Lude.Maybe Lude.Text)
srsBulkDeploymentId = Lens.lens (bulkDeploymentId :: StartBulkDeploymentResponse -> Lude.Maybe Lude.Text) (\s a -> s {bulkDeploymentId = a} :: StartBulkDeploymentResponse)
{-# DEPRECATED srsBulkDeploymentId "Use generic-lens or generic-optics with 'bulkDeploymentId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsResponseStatus :: Lens.Lens' StartBulkDeploymentResponse Lude.Int
srsResponseStatus = Lens.lens (responseStatus :: StartBulkDeploymentResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StartBulkDeploymentResponse)
{-# DEPRECATED srsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
