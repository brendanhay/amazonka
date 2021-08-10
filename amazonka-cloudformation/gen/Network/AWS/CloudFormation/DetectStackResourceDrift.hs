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
-- Module      : Network.AWS.CloudFormation.DetectStackResourceDrift
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about whether a resource\'s actual configuration
-- differs, or has /drifted/, from it\'s expected configuration, as defined
-- in the stack template and any values specified as template parameters.
-- This information includes actual and expected property values for
-- resources in which AWS CloudFormation detects drift. Only resource
-- properties explicitly defined in the stack template are checked for
-- drift. For more information about stack and resource drift, see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-drift.html Detecting Unregulated Configuration Changes to Stacks and Resources>.
--
-- Use @DetectStackResourceDrift@ to detect drift on individual resources,
-- or DetectStackDrift to detect drift on all resources in a given stack
-- that support drift detection.
--
-- Resources that do not currently support drift detection cannot be
-- checked. For a list of resources that support drift detection, see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-drift-resource-list.html Resources that Support Drift Detection>.
module Network.AWS.CloudFormation.DetectStackResourceDrift
  ( -- * Creating a Request
    DetectStackResourceDrift (..),
    newDetectStackResourceDrift,

    -- * Request Lenses
    detectStackResourceDrift_stackName,
    detectStackResourceDrift_logicalResourceId,

    -- * Destructuring the Response
    DetectStackResourceDriftResponse (..),
    newDetectStackResourceDriftResponse,

    -- * Response Lenses
    detectStackResourceDriftResponse_httpStatus,
    detectStackResourceDriftResponse_stackResourceDrift,
  )
where

import Network.AWS.CloudFormation.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDetectStackResourceDrift' smart constructor.
data DetectStackResourceDrift = DetectStackResourceDrift'
  { -- | The name of the stack to which the resource belongs.
    stackName :: Prelude.Text,
    -- | The logical name of the resource for which to return drift information.
    logicalResourceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DetectStackResourceDrift' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stackName', 'detectStackResourceDrift_stackName' - The name of the stack to which the resource belongs.
--
-- 'logicalResourceId', 'detectStackResourceDrift_logicalResourceId' - The logical name of the resource for which to return drift information.
newDetectStackResourceDrift ::
  -- | 'stackName'
  Prelude.Text ->
  -- | 'logicalResourceId'
  Prelude.Text ->
  DetectStackResourceDrift
newDetectStackResourceDrift
  pStackName_
  pLogicalResourceId_ =
    DetectStackResourceDrift'
      { stackName = pStackName_,
        logicalResourceId = pLogicalResourceId_
      }

-- | The name of the stack to which the resource belongs.
detectStackResourceDrift_stackName :: Lens.Lens' DetectStackResourceDrift Prelude.Text
detectStackResourceDrift_stackName = Lens.lens (\DetectStackResourceDrift' {stackName} -> stackName) (\s@DetectStackResourceDrift' {} a -> s {stackName = a} :: DetectStackResourceDrift)

-- | The logical name of the resource for which to return drift information.
detectStackResourceDrift_logicalResourceId :: Lens.Lens' DetectStackResourceDrift Prelude.Text
detectStackResourceDrift_logicalResourceId = Lens.lens (\DetectStackResourceDrift' {logicalResourceId} -> logicalResourceId) (\s@DetectStackResourceDrift' {} a -> s {logicalResourceId = a} :: DetectStackResourceDrift)

instance Core.AWSRequest DetectStackResourceDrift where
  type
    AWSResponse DetectStackResourceDrift =
      DetectStackResourceDriftResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DetectStackResourceDriftResult"
      ( \s h x ->
          DetectStackResourceDriftResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..@ "StackResourceDrift")
      )

instance Prelude.Hashable DetectStackResourceDrift

instance Prelude.NFData DetectStackResourceDrift

instance Core.ToHeaders DetectStackResourceDrift where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DetectStackResourceDrift where
  toPath = Prelude.const "/"

instance Core.ToQuery DetectStackResourceDrift where
  toQuery DetectStackResourceDrift' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("DetectStackResourceDrift" :: Prelude.ByteString),
        "Version"
          Core.=: ("2010-05-15" :: Prelude.ByteString),
        "StackName" Core.=: stackName,
        "LogicalResourceId" Core.=: logicalResourceId
      ]

-- | /See:/ 'newDetectStackResourceDriftResponse' smart constructor.
data DetectStackResourceDriftResponse = DetectStackResourceDriftResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Information about whether the resource\'s actual configuration has
    -- drifted from its expected template configuration, including actual and
    -- expected property values and any differences detected.
    stackResourceDrift :: StackResourceDrift
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DetectStackResourceDriftResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'detectStackResourceDriftResponse_httpStatus' - The response's http status code.
--
-- 'stackResourceDrift', 'detectStackResourceDriftResponse_stackResourceDrift' - Information about whether the resource\'s actual configuration has
-- drifted from its expected template configuration, including actual and
-- expected property values and any differences detected.
newDetectStackResourceDriftResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'stackResourceDrift'
  StackResourceDrift ->
  DetectStackResourceDriftResponse
newDetectStackResourceDriftResponse
  pHttpStatus_
  pStackResourceDrift_ =
    DetectStackResourceDriftResponse'
      { httpStatus =
          pHttpStatus_,
        stackResourceDrift = pStackResourceDrift_
      }

-- | The response's http status code.
detectStackResourceDriftResponse_httpStatus :: Lens.Lens' DetectStackResourceDriftResponse Prelude.Int
detectStackResourceDriftResponse_httpStatus = Lens.lens (\DetectStackResourceDriftResponse' {httpStatus} -> httpStatus) (\s@DetectStackResourceDriftResponse' {} a -> s {httpStatus = a} :: DetectStackResourceDriftResponse)

-- | Information about whether the resource\'s actual configuration has
-- drifted from its expected template configuration, including actual and
-- expected property values and any differences detected.
detectStackResourceDriftResponse_stackResourceDrift :: Lens.Lens' DetectStackResourceDriftResponse StackResourceDrift
detectStackResourceDriftResponse_stackResourceDrift = Lens.lens (\DetectStackResourceDriftResponse' {stackResourceDrift} -> stackResourceDrift) (\s@DetectStackResourceDriftResponse' {} a -> s {stackResourceDrift = a} :: DetectStackResourceDriftResponse)

instance
  Prelude.NFData
    DetectStackResourceDriftResponse
