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
-- Module      : Amazonka.CloudFormation.DetectStackResourceDrift
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about whether a resource\'s actual configuration
-- differs, or has /drifted/, from it\'s expected configuration, as defined
-- in the stack template and any values specified as template parameters.
-- This information includes actual and expected property values for
-- resources in which CloudFormation detects drift. Only resource
-- properties explicitly defined in the stack template are checked for
-- drift. For more information about stack and resource drift, see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-drift.html Detecting Unregulated Configuration Changes to Stacks and Resources>.
--
-- Use @DetectStackResourceDrift@ to detect drift on individual resources,
-- or DetectStackDrift to detect drift on all resources in a given stack
-- that support drift detection.
--
-- Resources that don\'t currently support drift detection can\'t be
-- checked. For a list of resources that support drift detection, see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-drift-resource-list.html Resources that Support Drift Detection>.
module Amazonka.CloudFormation.DetectStackResourceDrift
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

import Amazonka.CloudFormation.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DetectStackResourceDriftResult"
      ( \s h x ->
          DetectStackResourceDriftResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..@ "StackResourceDrift")
      )

instance Prelude.Hashable DetectStackResourceDrift where
  hashWithSalt _salt DetectStackResourceDrift' {..} =
    _salt `Prelude.hashWithSalt` stackName
      `Prelude.hashWithSalt` logicalResourceId

instance Prelude.NFData DetectStackResourceDrift where
  rnf DetectStackResourceDrift' {..} =
    Prelude.rnf stackName
      `Prelude.seq` Prelude.rnf logicalResourceId

instance Data.ToHeaders DetectStackResourceDrift where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DetectStackResourceDrift where
  toPath = Prelude.const "/"

instance Data.ToQuery DetectStackResourceDrift where
  toQuery DetectStackResourceDrift' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DetectStackResourceDrift" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-05-15" :: Prelude.ByteString),
        "StackName" Data.=: stackName,
        "LogicalResourceId" Data.=: logicalResourceId
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
  where
  rnf DetectStackResourceDriftResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf stackResourceDrift
