{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.CloudFormation.DetectStackSetDrift
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Detect drift on a stack set. When CloudFormation performs drift
-- detection on a stack set, it performs drift detection on the stack
-- associated with each stack instance in the stack set. For more
-- information, see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-drift.html How CloudFormation Performs Drift Detection on a Stack Set>.
--
-- @DetectStackSetDrift@ returns the @OperationId@ of the stack set drift
-- detection operation. Use this operation id with
-- @ DescribeStackSetOperation @ to monitor the progress of the drift
-- detection operation. The drift detection operation may take some time,
-- depending on the number of stack instances included in the stack set, as
-- well as the number of resources included in each stack.
--
-- Once the operation has completed, use the following actions to return
-- drift information:
--
-- -   Use @ DescribeStackSet @ to return detailed information about the
--     stack set, including detailed information about the last /completed/
--     drift operation performed on the stack set. (Information about drift
--     operations that are in progress is not included.)
--
-- -   Use @ ListStackInstances @ to return a list of stack instances
--     belonging to the stack set, including the drift status and last
--     drift time checked of each instance.
--
-- -   Use @ DescribeStackInstance @ to return detailed information about a
--     specific stack instance, including its drift status and last drift
--     time checked.
--
-- For more information on performing a drift detection operation on a
-- stack set, see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-drift.html Detecting Unmanaged Changes in Stack Sets>.
--
-- You can only run a single drift detection operation on a given stack set
-- at one time.
--
-- To stop a drift detection stack set operation, use
-- @ StopStackSetOperation @.
module Network.AWS.CloudFormation.DetectStackSetDrift
  ( -- * Creating a Request
    DetectStackSetDrift (..),
    newDetectStackSetDrift,

    -- * Request Lenses
    detectStackSetDrift_operationId,
    detectStackSetDrift_callAs,
    detectStackSetDrift_operationPreferences,
    detectStackSetDrift_stackSetName,

    -- * Destructuring the Response
    DetectStackSetDriftResponse (..),
    newDetectStackSetDriftResponse,

    -- * Response Lenses
    detectStackSetDriftResponse_operationId,
    detectStackSetDriftResponse_httpStatus,
  )
where

import Network.AWS.CloudFormation.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDetectStackSetDrift' smart constructor.
data DetectStackSetDrift = DetectStackSetDrift'
  { -- | /The ID of the stack set operation./
    operationId :: Prelude.Maybe Prelude.Text,
    -- | [Service-managed permissions] Specifies whether you are acting as an
    -- account administrator in the organization\'s management account or as a
    -- delegated administrator in a member account.
    --
    -- By default, @SELF@ is specified. Use @SELF@ for stack sets with
    -- self-managed permissions.
    --
    -- -   If you are signed in to the management account, specify @SELF@.
    --
    -- -   If you are signed in to a delegated administrator account, specify
    --     @DELEGATED_ADMIN@.
    --
    --     Your AWS account must be registered as a delegated administrator in
    --     the management account. For more information, see
    --     <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-orgs-delegated-admin.html Register a delegated administrator>
    --     in the /AWS CloudFormation User Guide/.
    callAs :: Prelude.Maybe CallAs,
    operationPreferences :: Prelude.Maybe StackSetOperationPreferences,
    -- | The name of the stack set on which to perform the drift detection
    -- operation.
    stackSetName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DetectStackSetDrift' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operationId', 'detectStackSetDrift_operationId' - /The ID of the stack set operation./
--
-- 'callAs', 'detectStackSetDrift_callAs' - [Service-managed permissions] Specifies whether you are acting as an
-- account administrator in the organization\'s management account or as a
-- delegated administrator in a member account.
--
-- By default, @SELF@ is specified. Use @SELF@ for stack sets with
-- self-managed permissions.
--
-- -   If you are signed in to the management account, specify @SELF@.
--
-- -   If you are signed in to a delegated administrator account, specify
--     @DELEGATED_ADMIN@.
--
--     Your AWS account must be registered as a delegated administrator in
--     the management account. For more information, see
--     <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-orgs-delegated-admin.html Register a delegated administrator>
--     in the /AWS CloudFormation User Guide/.
--
-- 'operationPreferences', 'detectStackSetDrift_operationPreferences' - Undocumented member.
--
-- 'stackSetName', 'detectStackSetDrift_stackSetName' - The name of the stack set on which to perform the drift detection
-- operation.
newDetectStackSetDrift ::
  -- | 'stackSetName'
  Prelude.Text ->
  DetectStackSetDrift
newDetectStackSetDrift pStackSetName_ =
  DetectStackSetDrift'
    { operationId = Prelude.Nothing,
      callAs = Prelude.Nothing,
      operationPreferences = Prelude.Nothing,
      stackSetName = pStackSetName_
    }

-- | /The ID of the stack set operation./
detectStackSetDrift_operationId :: Lens.Lens' DetectStackSetDrift (Prelude.Maybe Prelude.Text)
detectStackSetDrift_operationId = Lens.lens (\DetectStackSetDrift' {operationId} -> operationId) (\s@DetectStackSetDrift' {} a -> s {operationId = a} :: DetectStackSetDrift)

-- | [Service-managed permissions] Specifies whether you are acting as an
-- account administrator in the organization\'s management account or as a
-- delegated administrator in a member account.
--
-- By default, @SELF@ is specified. Use @SELF@ for stack sets with
-- self-managed permissions.
--
-- -   If you are signed in to the management account, specify @SELF@.
--
-- -   If you are signed in to a delegated administrator account, specify
--     @DELEGATED_ADMIN@.
--
--     Your AWS account must be registered as a delegated administrator in
--     the management account. For more information, see
--     <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-orgs-delegated-admin.html Register a delegated administrator>
--     in the /AWS CloudFormation User Guide/.
detectStackSetDrift_callAs :: Lens.Lens' DetectStackSetDrift (Prelude.Maybe CallAs)
detectStackSetDrift_callAs = Lens.lens (\DetectStackSetDrift' {callAs} -> callAs) (\s@DetectStackSetDrift' {} a -> s {callAs = a} :: DetectStackSetDrift)

-- | Undocumented member.
detectStackSetDrift_operationPreferences :: Lens.Lens' DetectStackSetDrift (Prelude.Maybe StackSetOperationPreferences)
detectStackSetDrift_operationPreferences = Lens.lens (\DetectStackSetDrift' {operationPreferences} -> operationPreferences) (\s@DetectStackSetDrift' {} a -> s {operationPreferences = a} :: DetectStackSetDrift)

-- | The name of the stack set on which to perform the drift detection
-- operation.
detectStackSetDrift_stackSetName :: Lens.Lens' DetectStackSetDrift Prelude.Text
detectStackSetDrift_stackSetName = Lens.lens (\DetectStackSetDrift' {stackSetName} -> stackSetName) (\s@DetectStackSetDrift' {} a -> s {stackSetName = a} :: DetectStackSetDrift)

instance Prelude.AWSRequest DetectStackSetDrift where
  type
    Rs DetectStackSetDrift =
      DetectStackSetDriftResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DetectStackSetDriftResult"
      ( \s h x ->
          DetectStackSetDriftResponse'
            Prelude.<$> (x Prelude..@? "OperationId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DetectStackSetDrift

instance Prelude.NFData DetectStackSetDrift

instance Prelude.ToHeaders DetectStackSetDrift where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DetectStackSetDrift where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DetectStackSetDrift where
  toQuery DetectStackSetDrift' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("DetectStackSetDrift" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2010-05-15" :: Prelude.ByteString),
        "OperationId" Prelude.=: operationId,
        "CallAs" Prelude.=: callAs,
        "OperationPreferences"
          Prelude.=: operationPreferences,
        "StackSetName" Prelude.=: stackSetName
      ]

-- | /See:/ 'newDetectStackSetDriftResponse' smart constructor.
data DetectStackSetDriftResponse = DetectStackSetDriftResponse'
  { -- | The ID of the drift detection stack set operation.
    --
    -- you can use this operation id with @ DescribeStackSetOperation @ to
    -- monitor the progress of the drift detection operation.
    operationId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DetectStackSetDriftResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operationId', 'detectStackSetDriftResponse_operationId' - The ID of the drift detection stack set operation.
--
-- you can use this operation id with @ DescribeStackSetOperation @ to
-- monitor the progress of the drift detection operation.
--
-- 'httpStatus', 'detectStackSetDriftResponse_httpStatus' - The response's http status code.
newDetectStackSetDriftResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DetectStackSetDriftResponse
newDetectStackSetDriftResponse pHttpStatus_ =
  DetectStackSetDriftResponse'
    { operationId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID of the drift detection stack set operation.
--
-- you can use this operation id with @ DescribeStackSetOperation @ to
-- monitor the progress of the drift detection operation.
detectStackSetDriftResponse_operationId :: Lens.Lens' DetectStackSetDriftResponse (Prelude.Maybe Prelude.Text)
detectStackSetDriftResponse_operationId = Lens.lens (\DetectStackSetDriftResponse' {operationId} -> operationId) (\s@DetectStackSetDriftResponse' {} a -> s {operationId = a} :: DetectStackSetDriftResponse)

-- | The response's http status code.
detectStackSetDriftResponse_httpStatus :: Lens.Lens' DetectStackSetDriftResponse Prelude.Int
detectStackSetDriftResponse_httpStatus = Lens.lens (\DetectStackSetDriftResponse' {httpStatus} -> httpStatus) (\s@DetectStackSetDriftResponse' {} a -> s {httpStatus = a} :: DetectStackSetDriftResponse)

instance Prelude.NFData DetectStackSetDriftResponse
