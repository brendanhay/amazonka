{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.StartBulkDeployment
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deploys multiple groups in one operation. This action starts the bulk
-- deployment of a specified set of group versions. Each group version
-- deployment will be triggered with an adaptive rate that has a fixed
-- upper limit. We recommend that you include an
-- \'\'X-Amzn-Client-Token\'\' token in every \'\'StartBulkDeployment\'\'
-- request. These requests are idempotent with respect to the token and the
-- request parameters.
module Network.AWS.Greengrass.StartBulkDeployment
  ( -- * Creating a Request
    StartBulkDeployment (..),
    newStartBulkDeployment,

    -- * Request Lenses
    startBulkDeployment_tags,
    startBulkDeployment_amznClientToken,
    startBulkDeployment_executionRoleArn,
    startBulkDeployment_inputFileUri,

    -- * Destructuring the Response
    StartBulkDeploymentResponse (..),
    newStartBulkDeploymentResponse,

    -- * Response Lenses
    startBulkDeploymentResponse_bulkDeploymentId,
    startBulkDeploymentResponse_bulkDeploymentArn,
    startBulkDeploymentResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newStartBulkDeployment' smart constructor.
data StartBulkDeployment = StartBulkDeployment'
  { -- | Tag(s) to add to the new resource.
    tags :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | A client token used to correlate requests and responses.
    amznClientToken :: Core.Maybe Core.Text,
    -- | The ARN of the execution role to associate with the bulk deployment
    -- operation. This IAM role must allow the
    -- \'\'greengrass:CreateDeployment\'\' action for all group versions that
    -- are listed in the input file. This IAM role must have access to the S3
    -- bucket containing the input file.
    executionRoleArn :: Core.Text,
    -- | The URI of the input file contained in the S3 bucket. The execution role
    -- must have \'\'getObject\'\' permissions on this bucket to access the
    -- input file. The input file is a JSON-serialized, line delimited file
    -- with UTF-8 encoding that provides a list of group and version IDs and
    -- the deployment type. This file must be less than 100 MB. Currently, AWS
    -- IoT Greengrass supports only \'\'NewDeployment\'\' deployment types.
    inputFileUri :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StartBulkDeployment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'startBulkDeployment_tags' - Tag(s) to add to the new resource.
--
-- 'amznClientToken', 'startBulkDeployment_amznClientToken' - A client token used to correlate requests and responses.
--
-- 'executionRoleArn', 'startBulkDeployment_executionRoleArn' - The ARN of the execution role to associate with the bulk deployment
-- operation. This IAM role must allow the
-- \'\'greengrass:CreateDeployment\'\' action for all group versions that
-- are listed in the input file. This IAM role must have access to the S3
-- bucket containing the input file.
--
-- 'inputFileUri', 'startBulkDeployment_inputFileUri' - The URI of the input file contained in the S3 bucket. The execution role
-- must have \'\'getObject\'\' permissions on this bucket to access the
-- input file. The input file is a JSON-serialized, line delimited file
-- with UTF-8 encoding that provides a list of group and version IDs and
-- the deployment type. This file must be less than 100 MB. Currently, AWS
-- IoT Greengrass supports only \'\'NewDeployment\'\' deployment types.
newStartBulkDeployment ::
  -- | 'executionRoleArn'
  Core.Text ->
  -- | 'inputFileUri'
  Core.Text ->
  StartBulkDeployment
newStartBulkDeployment
  pExecutionRoleArn_
  pInputFileUri_ =
    StartBulkDeployment'
      { tags = Core.Nothing,
        amznClientToken = Core.Nothing,
        executionRoleArn = pExecutionRoleArn_,
        inputFileUri = pInputFileUri_
      }

-- | Tag(s) to add to the new resource.
startBulkDeployment_tags :: Lens.Lens' StartBulkDeployment (Core.Maybe (Core.HashMap Core.Text Core.Text))
startBulkDeployment_tags = Lens.lens (\StartBulkDeployment' {tags} -> tags) (\s@StartBulkDeployment' {} a -> s {tags = a} :: StartBulkDeployment) Core.. Lens.mapping Lens._Coerce

-- | A client token used to correlate requests and responses.
startBulkDeployment_amznClientToken :: Lens.Lens' StartBulkDeployment (Core.Maybe Core.Text)
startBulkDeployment_amznClientToken = Lens.lens (\StartBulkDeployment' {amznClientToken} -> amznClientToken) (\s@StartBulkDeployment' {} a -> s {amznClientToken = a} :: StartBulkDeployment)

-- | The ARN of the execution role to associate with the bulk deployment
-- operation. This IAM role must allow the
-- \'\'greengrass:CreateDeployment\'\' action for all group versions that
-- are listed in the input file. This IAM role must have access to the S3
-- bucket containing the input file.
startBulkDeployment_executionRoleArn :: Lens.Lens' StartBulkDeployment Core.Text
startBulkDeployment_executionRoleArn = Lens.lens (\StartBulkDeployment' {executionRoleArn} -> executionRoleArn) (\s@StartBulkDeployment' {} a -> s {executionRoleArn = a} :: StartBulkDeployment)

-- | The URI of the input file contained in the S3 bucket. The execution role
-- must have \'\'getObject\'\' permissions on this bucket to access the
-- input file. The input file is a JSON-serialized, line delimited file
-- with UTF-8 encoding that provides a list of group and version IDs and
-- the deployment type. This file must be less than 100 MB. Currently, AWS
-- IoT Greengrass supports only \'\'NewDeployment\'\' deployment types.
startBulkDeployment_inputFileUri :: Lens.Lens' StartBulkDeployment Core.Text
startBulkDeployment_inputFileUri = Lens.lens (\StartBulkDeployment' {inputFileUri} -> inputFileUri) (\s@StartBulkDeployment' {} a -> s {inputFileUri = a} :: StartBulkDeployment)

instance Core.AWSRequest StartBulkDeployment where
  type
    AWSResponse StartBulkDeployment =
      StartBulkDeploymentResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          StartBulkDeploymentResponse'
            Core.<$> (x Core..?> "BulkDeploymentId")
            Core.<*> (x Core..?> "BulkDeploymentArn")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable StartBulkDeployment

instance Core.NFData StartBulkDeployment

instance Core.ToHeaders StartBulkDeployment where
  toHeaders StartBulkDeployment' {..} =
    Core.mconcat
      [ "X-Amzn-Client-Token" Core.=# amznClientToken,
        "Content-Type"
          Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
      ]

instance Core.ToJSON StartBulkDeployment where
  toJSON StartBulkDeployment' {..} =
    Core.object
      ( Core.catMaybes
          [ ("tags" Core..=) Core.<$> tags,
            Core.Just
              ("ExecutionRoleArn" Core..= executionRoleArn),
            Core.Just ("InputFileUri" Core..= inputFileUri)
          ]
      )

instance Core.ToPath StartBulkDeployment where
  toPath = Core.const "/greengrass/bulk/deployments"

instance Core.ToQuery StartBulkDeployment where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newStartBulkDeploymentResponse' smart constructor.
data StartBulkDeploymentResponse = StartBulkDeploymentResponse'
  { -- | The ID of the bulk deployment.
    bulkDeploymentId :: Core.Maybe Core.Text,
    -- | The ARN of the bulk deployment.
    bulkDeploymentArn :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StartBulkDeploymentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bulkDeploymentId', 'startBulkDeploymentResponse_bulkDeploymentId' - The ID of the bulk deployment.
--
-- 'bulkDeploymentArn', 'startBulkDeploymentResponse_bulkDeploymentArn' - The ARN of the bulk deployment.
--
-- 'httpStatus', 'startBulkDeploymentResponse_httpStatus' - The response's http status code.
newStartBulkDeploymentResponse ::
  -- | 'httpStatus'
  Core.Int ->
  StartBulkDeploymentResponse
newStartBulkDeploymentResponse pHttpStatus_ =
  StartBulkDeploymentResponse'
    { bulkDeploymentId =
        Core.Nothing,
      bulkDeploymentArn = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID of the bulk deployment.
startBulkDeploymentResponse_bulkDeploymentId :: Lens.Lens' StartBulkDeploymentResponse (Core.Maybe Core.Text)
startBulkDeploymentResponse_bulkDeploymentId = Lens.lens (\StartBulkDeploymentResponse' {bulkDeploymentId} -> bulkDeploymentId) (\s@StartBulkDeploymentResponse' {} a -> s {bulkDeploymentId = a} :: StartBulkDeploymentResponse)

-- | The ARN of the bulk deployment.
startBulkDeploymentResponse_bulkDeploymentArn :: Lens.Lens' StartBulkDeploymentResponse (Core.Maybe Core.Text)
startBulkDeploymentResponse_bulkDeploymentArn = Lens.lens (\StartBulkDeploymentResponse' {bulkDeploymentArn} -> bulkDeploymentArn) (\s@StartBulkDeploymentResponse' {} a -> s {bulkDeploymentArn = a} :: StartBulkDeploymentResponse)

-- | The response's http status code.
startBulkDeploymentResponse_httpStatus :: Lens.Lens' StartBulkDeploymentResponse Core.Int
startBulkDeploymentResponse_httpStatus = Lens.lens (\StartBulkDeploymentResponse' {httpStatus} -> httpStatus) (\s@StartBulkDeploymentResponse' {} a -> s {httpStatus = a} :: StartBulkDeploymentResponse)

instance Core.NFData StartBulkDeploymentResponse
