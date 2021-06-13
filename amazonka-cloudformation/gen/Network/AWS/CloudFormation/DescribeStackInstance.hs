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
-- Module      : Network.AWS.CloudFormation.DescribeStackInstance
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the stack instance that\'s associated with the specified stack
-- set, AWS account, and Region.
--
-- For a list of stack instances that are associated with a specific stack
-- set, use ListStackInstances.
module Network.AWS.CloudFormation.DescribeStackInstance
  ( -- * Creating a Request
    DescribeStackInstance (..),
    newDescribeStackInstance,

    -- * Request Lenses
    describeStackInstance_callAs,
    describeStackInstance_stackSetName,
    describeStackInstance_stackInstanceAccount,
    describeStackInstance_stackInstanceRegion,

    -- * Destructuring the Response
    DescribeStackInstanceResponse (..),
    newDescribeStackInstanceResponse,

    -- * Response Lenses
    describeStackInstanceResponse_stackInstance,
    describeStackInstanceResponse_httpStatus,
  )
where

import Network.AWS.CloudFormation.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeStackInstance' smart constructor.
data DescribeStackInstance = DescribeStackInstance'
  { -- | [Service-managed permissions] Specifies whether you are acting as an
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
    -- | The name or the unique stack ID of the stack set that you want to get
    -- stack instance information for.
    stackSetName :: Prelude.Text,
    -- | The ID of an AWS account that\'s associated with this stack instance.
    stackInstanceAccount :: Prelude.Text,
    -- | The name of a Region that\'s associated with this stack instance.
    stackInstanceRegion :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeStackInstance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'callAs', 'describeStackInstance_callAs' - [Service-managed permissions] Specifies whether you are acting as an
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
-- 'stackSetName', 'describeStackInstance_stackSetName' - The name or the unique stack ID of the stack set that you want to get
-- stack instance information for.
--
-- 'stackInstanceAccount', 'describeStackInstance_stackInstanceAccount' - The ID of an AWS account that\'s associated with this stack instance.
--
-- 'stackInstanceRegion', 'describeStackInstance_stackInstanceRegion' - The name of a Region that\'s associated with this stack instance.
newDescribeStackInstance ::
  -- | 'stackSetName'
  Prelude.Text ->
  -- | 'stackInstanceAccount'
  Prelude.Text ->
  -- | 'stackInstanceRegion'
  Prelude.Text ->
  DescribeStackInstance
newDescribeStackInstance
  pStackSetName_
  pStackInstanceAccount_
  pStackInstanceRegion_ =
    DescribeStackInstance'
      { callAs = Prelude.Nothing,
        stackSetName = pStackSetName_,
        stackInstanceAccount = pStackInstanceAccount_,
        stackInstanceRegion = pStackInstanceRegion_
      }

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
describeStackInstance_callAs :: Lens.Lens' DescribeStackInstance (Prelude.Maybe CallAs)
describeStackInstance_callAs = Lens.lens (\DescribeStackInstance' {callAs} -> callAs) (\s@DescribeStackInstance' {} a -> s {callAs = a} :: DescribeStackInstance)

-- | The name or the unique stack ID of the stack set that you want to get
-- stack instance information for.
describeStackInstance_stackSetName :: Lens.Lens' DescribeStackInstance Prelude.Text
describeStackInstance_stackSetName = Lens.lens (\DescribeStackInstance' {stackSetName} -> stackSetName) (\s@DescribeStackInstance' {} a -> s {stackSetName = a} :: DescribeStackInstance)

-- | The ID of an AWS account that\'s associated with this stack instance.
describeStackInstance_stackInstanceAccount :: Lens.Lens' DescribeStackInstance Prelude.Text
describeStackInstance_stackInstanceAccount = Lens.lens (\DescribeStackInstance' {stackInstanceAccount} -> stackInstanceAccount) (\s@DescribeStackInstance' {} a -> s {stackInstanceAccount = a} :: DescribeStackInstance)

-- | The name of a Region that\'s associated with this stack instance.
describeStackInstance_stackInstanceRegion :: Lens.Lens' DescribeStackInstance Prelude.Text
describeStackInstance_stackInstanceRegion = Lens.lens (\DescribeStackInstance' {stackInstanceRegion} -> stackInstanceRegion) (\s@DescribeStackInstance' {} a -> s {stackInstanceRegion = a} :: DescribeStackInstance)

instance Core.AWSRequest DescribeStackInstance where
  type
    AWSResponse DescribeStackInstance =
      DescribeStackInstanceResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeStackInstanceResult"
      ( \s h x ->
          DescribeStackInstanceResponse'
            Prelude.<$> (x Core..@? "StackInstance")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeStackInstance

instance Prelude.NFData DescribeStackInstance

instance Core.ToHeaders DescribeStackInstance where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DescribeStackInstance where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeStackInstance where
  toQuery DescribeStackInstance' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("DescribeStackInstance" :: Prelude.ByteString),
        "Version"
          Core.=: ("2010-05-15" :: Prelude.ByteString),
        "CallAs" Core.=: callAs,
        "StackSetName" Core.=: stackSetName,
        "StackInstanceAccount" Core.=: stackInstanceAccount,
        "StackInstanceRegion" Core.=: stackInstanceRegion
      ]

-- | /See:/ 'newDescribeStackInstanceResponse' smart constructor.
data DescribeStackInstanceResponse = DescribeStackInstanceResponse'
  { -- | The stack instance that matches the specified request parameters.
    stackInstance :: Prelude.Maybe StackInstance,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeStackInstanceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stackInstance', 'describeStackInstanceResponse_stackInstance' - The stack instance that matches the specified request parameters.
--
-- 'httpStatus', 'describeStackInstanceResponse_httpStatus' - The response's http status code.
newDescribeStackInstanceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeStackInstanceResponse
newDescribeStackInstanceResponse pHttpStatus_ =
  DescribeStackInstanceResponse'
    { stackInstance =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The stack instance that matches the specified request parameters.
describeStackInstanceResponse_stackInstance :: Lens.Lens' DescribeStackInstanceResponse (Prelude.Maybe StackInstance)
describeStackInstanceResponse_stackInstance = Lens.lens (\DescribeStackInstanceResponse' {stackInstance} -> stackInstance) (\s@DescribeStackInstanceResponse' {} a -> s {stackInstance = a} :: DescribeStackInstanceResponse)

-- | The response's http status code.
describeStackInstanceResponse_httpStatus :: Lens.Lens' DescribeStackInstanceResponse Prelude.Int
describeStackInstanceResponse_httpStatus = Lens.lens (\DescribeStackInstanceResponse' {httpStatus} -> httpStatus) (\s@DescribeStackInstanceResponse' {} a -> s {httpStatus = a} :: DescribeStackInstanceResponse)

instance Prelude.NFData DescribeStackInstanceResponse
