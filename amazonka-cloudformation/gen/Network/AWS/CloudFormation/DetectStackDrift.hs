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
-- Module      : Network.AWS.CloudFormation.DetectStackDrift
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Detects whether a stack\'s actual configuration differs, or has
-- /drifted/, from it\'s expected configuration, as defined in the stack
-- template and any values specified as template parameters. For each
-- resource in the stack that supports drift detection, AWS CloudFormation
-- compares the actual configuration of the resource with its expected
-- template configuration. Only resource properties explicitly defined in
-- the stack template are checked for drift. A stack is considered to have
-- drifted if one or more of its resources differ from their expected
-- template configurations. For more information, see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-drift.html Detecting Unregulated Configuration Changes to Stacks and Resources>.
--
-- Use @DetectStackDrift@ to detect drift on all supported resources for a
-- given stack, or DetectStackResourceDrift to detect drift on individual
-- resources.
--
-- For a list of stack resources that currently support drift detection,
-- see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-drift-resource-list.html Resources that Support Drift Detection>.
--
-- @DetectStackDrift@ can take up to several minutes, depending on the
-- number of resources contained within the stack. Use
-- DescribeStackDriftDetectionStatus to monitor the progress of a detect
-- stack drift operation. Once the drift detection operation has completed,
-- use DescribeStackResourceDrifts to return drift information about the
-- stack and its resources.
--
-- When detecting drift on a stack, AWS CloudFormation does not detect
-- drift on any nested stacks belonging to that stack. Perform
-- @DetectStackDrift@ directly on the nested stack itself.
module Network.AWS.CloudFormation.DetectStackDrift
  ( -- * Creating a Request
    DetectStackDrift (..),
    newDetectStackDrift,

    -- * Request Lenses
    detectStackDrift_logicalResourceIds,
    detectStackDrift_stackName,

    -- * Destructuring the Response
    DetectStackDriftResponse (..),
    newDetectStackDriftResponse,

    -- * Response Lenses
    detectStackDriftResponse_httpStatus,
    detectStackDriftResponse_stackDriftDetectionId,
  )
where

import Network.AWS.CloudFormation.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDetectStackDrift' smart constructor.
data DetectStackDrift = DetectStackDrift'
  { -- | The logical names of any resources you want to use as filters.
    logicalResourceIds :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The name of the stack for which you want to detect drift.
    stackName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DetectStackDrift' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'logicalResourceIds', 'detectStackDrift_logicalResourceIds' - The logical names of any resources you want to use as filters.
--
-- 'stackName', 'detectStackDrift_stackName' - The name of the stack for which you want to detect drift.
newDetectStackDrift ::
  -- | 'stackName'
  Prelude.Text ->
  DetectStackDrift
newDetectStackDrift pStackName_ =
  DetectStackDrift'
    { logicalResourceIds =
        Prelude.Nothing,
      stackName = pStackName_
    }

-- | The logical names of any resources you want to use as filters.
detectStackDrift_logicalResourceIds :: Lens.Lens' DetectStackDrift (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
detectStackDrift_logicalResourceIds = Lens.lens (\DetectStackDrift' {logicalResourceIds} -> logicalResourceIds) (\s@DetectStackDrift' {} a -> s {logicalResourceIds = a} :: DetectStackDrift) Prelude.. Lens.mapping Lens._Coerce

-- | The name of the stack for which you want to detect drift.
detectStackDrift_stackName :: Lens.Lens' DetectStackDrift Prelude.Text
detectStackDrift_stackName = Lens.lens (\DetectStackDrift' {stackName} -> stackName) (\s@DetectStackDrift' {} a -> s {stackName = a} :: DetectStackDrift)

instance Core.AWSRequest DetectStackDrift where
  type
    AWSResponse DetectStackDrift =
      DetectStackDriftResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DetectStackDriftResult"
      ( \s h x ->
          DetectStackDriftResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..@ "StackDriftDetectionId")
      )

instance Prelude.Hashable DetectStackDrift

instance Prelude.NFData DetectStackDrift

instance Core.ToHeaders DetectStackDrift where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DetectStackDrift where
  toPath = Prelude.const "/"

instance Core.ToQuery DetectStackDrift where
  toQuery DetectStackDrift' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("DetectStackDrift" :: Prelude.ByteString),
        "Version"
          Core.=: ("2010-05-15" :: Prelude.ByteString),
        "LogicalResourceIds"
          Core.=: Core.toQuery
            ( Core.toQueryList "member"
                Prelude.<$> logicalResourceIds
            ),
        "StackName" Core.=: stackName
      ]

-- | /See:/ 'newDetectStackDriftResponse' smart constructor.
data DetectStackDriftResponse = DetectStackDriftResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The ID of the drift detection results of this operation.
    --
    -- AWS CloudFormation generates new results, with a new drift detection ID,
    -- each time this operation is run. However, the number of drift results
    -- AWS CloudFormation retains for any given stack, and for how long, may
    -- vary.
    stackDriftDetectionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DetectStackDriftResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'detectStackDriftResponse_httpStatus' - The response's http status code.
--
-- 'stackDriftDetectionId', 'detectStackDriftResponse_stackDriftDetectionId' - The ID of the drift detection results of this operation.
--
-- AWS CloudFormation generates new results, with a new drift detection ID,
-- each time this operation is run. However, the number of drift results
-- AWS CloudFormation retains for any given stack, and for how long, may
-- vary.
newDetectStackDriftResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'stackDriftDetectionId'
  Prelude.Text ->
  DetectStackDriftResponse
newDetectStackDriftResponse
  pHttpStatus_
  pStackDriftDetectionId_ =
    DetectStackDriftResponse'
      { httpStatus =
          pHttpStatus_,
        stackDriftDetectionId = pStackDriftDetectionId_
      }

-- | The response's http status code.
detectStackDriftResponse_httpStatus :: Lens.Lens' DetectStackDriftResponse Prelude.Int
detectStackDriftResponse_httpStatus = Lens.lens (\DetectStackDriftResponse' {httpStatus} -> httpStatus) (\s@DetectStackDriftResponse' {} a -> s {httpStatus = a} :: DetectStackDriftResponse)

-- | The ID of the drift detection results of this operation.
--
-- AWS CloudFormation generates new results, with a new drift detection ID,
-- each time this operation is run. However, the number of drift results
-- AWS CloudFormation retains for any given stack, and for how long, may
-- vary.
detectStackDriftResponse_stackDriftDetectionId :: Lens.Lens' DetectStackDriftResponse Prelude.Text
detectStackDriftResponse_stackDriftDetectionId = Lens.lens (\DetectStackDriftResponse' {stackDriftDetectionId} -> stackDriftDetectionId) (\s@DetectStackDriftResponse' {} a -> s {stackDriftDetectionId = a} :: DetectStackDriftResponse)

instance Prelude.NFData DetectStackDriftResponse
