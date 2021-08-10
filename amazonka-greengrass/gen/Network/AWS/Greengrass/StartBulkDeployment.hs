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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newStartBulkDeployment' smart constructor.
data StartBulkDeployment = StartBulkDeployment'
  { -- | Tag(s) to add to the new resource.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | A client token used to correlate requests and responses.
    amznClientToken :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the execution role to associate with the bulk deployment
    -- operation. This IAM role must allow the
    -- \'\'greengrass:CreateDeployment\'\' action for all group versions that
    -- are listed in the input file. This IAM role must have access to the S3
    -- bucket containing the input file.
    executionRoleArn :: Prelude.Text,
    -- | The URI of the input file contained in the S3 bucket. The execution role
    -- must have \'\'getObject\'\' permissions on this bucket to access the
    -- input file. The input file is a JSON-serialized, line delimited file
    -- with UTF-8 encoding that provides a list of group and version IDs and
    -- the deployment type. This file must be less than 100 MB. Currently, AWS
    -- IoT Greengrass supports only \'\'NewDeployment\'\' deployment types.
    inputFileUri :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'inputFileUri'
  Prelude.Text ->
  StartBulkDeployment
newStartBulkDeployment
  pExecutionRoleArn_
  pInputFileUri_ =
    StartBulkDeployment'
      { tags = Prelude.Nothing,
        amznClientToken = Prelude.Nothing,
        executionRoleArn = pExecutionRoleArn_,
        inputFileUri = pInputFileUri_
      }

-- | Tag(s) to add to the new resource.
startBulkDeployment_tags :: Lens.Lens' StartBulkDeployment (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
startBulkDeployment_tags = Lens.lens (\StartBulkDeployment' {tags} -> tags) (\s@StartBulkDeployment' {} a -> s {tags = a} :: StartBulkDeployment) Prelude.. Lens.mapping Lens._Coerce

-- | A client token used to correlate requests and responses.
startBulkDeployment_amznClientToken :: Lens.Lens' StartBulkDeployment (Prelude.Maybe Prelude.Text)
startBulkDeployment_amznClientToken = Lens.lens (\StartBulkDeployment' {amznClientToken} -> amznClientToken) (\s@StartBulkDeployment' {} a -> s {amznClientToken = a} :: StartBulkDeployment)

-- | The ARN of the execution role to associate with the bulk deployment
-- operation. This IAM role must allow the
-- \'\'greengrass:CreateDeployment\'\' action for all group versions that
-- are listed in the input file. This IAM role must have access to the S3
-- bucket containing the input file.
startBulkDeployment_executionRoleArn :: Lens.Lens' StartBulkDeployment Prelude.Text
startBulkDeployment_executionRoleArn = Lens.lens (\StartBulkDeployment' {executionRoleArn} -> executionRoleArn) (\s@StartBulkDeployment' {} a -> s {executionRoleArn = a} :: StartBulkDeployment)

-- | The URI of the input file contained in the S3 bucket. The execution role
-- must have \'\'getObject\'\' permissions on this bucket to access the
-- input file. The input file is a JSON-serialized, line delimited file
-- with UTF-8 encoding that provides a list of group and version IDs and
-- the deployment type. This file must be less than 100 MB. Currently, AWS
-- IoT Greengrass supports only \'\'NewDeployment\'\' deployment types.
startBulkDeployment_inputFileUri :: Lens.Lens' StartBulkDeployment Prelude.Text
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
            Prelude.<$> (x Core..?> "BulkDeploymentId")
            Prelude.<*> (x Core..?> "BulkDeploymentArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartBulkDeployment

instance Prelude.NFData StartBulkDeployment

instance Core.ToHeaders StartBulkDeployment where
  toHeaders StartBulkDeployment' {..} =
    Prelude.mconcat
      [ "X-Amzn-Client-Token" Core.=# amznClientToken,
        "Content-Type"
          Core.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance Core.ToJSON StartBulkDeployment where
  toJSON StartBulkDeployment' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("tags" Core..=) Prelude.<$> tags,
            Prelude.Just
              ("ExecutionRoleArn" Core..= executionRoleArn),
            Prelude.Just ("InputFileUri" Core..= inputFileUri)
          ]
      )

instance Core.ToPath StartBulkDeployment where
  toPath = Prelude.const "/greengrass/bulk/deployments"

instance Core.ToQuery StartBulkDeployment where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartBulkDeploymentResponse' smart constructor.
data StartBulkDeploymentResponse = StartBulkDeploymentResponse'
  { -- | The ID of the bulk deployment.
    bulkDeploymentId :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the bulk deployment.
    bulkDeploymentArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  StartBulkDeploymentResponse
newStartBulkDeploymentResponse pHttpStatus_ =
  StartBulkDeploymentResponse'
    { bulkDeploymentId =
        Prelude.Nothing,
      bulkDeploymentArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID of the bulk deployment.
startBulkDeploymentResponse_bulkDeploymentId :: Lens.Lens' StartBulkDeploymentResponse (Prelude.Maybe Prelude.Text)
startBulkDeploymentResponse_bulkDeploymentId = Lens.lens (\StartBulkDeploymentResponse' {bulkDeploymentId} -> bulkDeploymentId) (\s@StartBulkDeploymentResponse' {} a -> s {bulkDeploymentId = a} :: StartBulkDeploymentResponse)

-- | The ARN of the bulk deployment.
startBulkDeploymentResponse_bulkDeploymentArn :: Lens.Lens' StartBulkDeploymentResponse (Prelude.Maybe Prelude.Text)
startBulkDeploymentResponse_bulkDeploymentArn = Lens.lens (\StartBulkDeploymentResponse' {bulkDeploymentArn} -> bulkDeploymentArn) (\s@StartBulkDeploymentResponse' {} a -> s {bulkDeploymentArn = a} :: StartBulkDeploymentResponse)

-- | The response's http status code.
startBulkDeploymentResponse_httpStatus :: Lens.Lens' StartBulkDeploymentResponse Prelude.Int
startBulkDeploymentResponse_httpStatus = Lens.lens (\StartBulkDeploymentResponse' {httpStatus} -> httpStatus) (\s@StartBulkDeploymentResponse' {} a -> s {httpStatus = a} :: StartBulkDeploymentResponse)

instance Prelude.NFData StartBulkDeploymentResponse
