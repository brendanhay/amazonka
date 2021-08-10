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
-- Module      : Network.AWS.CloudFormation.DescribeStackSet
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the description of the specified stack set.
module Network.AWS.CloudFormation.DescribeStackSet
  ( -- * Creating a Request
    DescribeStackSet (..),
    newDescribeStackSet,

    -- * Request Lenses
    describeStackSet_callAs,
    describeStackSet_stackSetName,

    -- * Destructuring the Response
    DescribeStackSetResponse (..),
    newDescribeStackSetResponse,

    -- * Response Lenses
    describeStackSetResponse_stackSet,
    describeStackSetResponse_httpStatus,
  )
where

import Network.AWS.CloudFormation.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeStackSet' smart constructor.
data DescribeStackSet = DescribeStackSet'
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
    -- | The name or unique ID of the stack set whose description you want.
    stackSetName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeStackSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'callAs', 'describeStackSet_callAs' - [Service-managed permissions] Specifies whether you are acting as an
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
-- 'stackSetName', 'describeStackSet_stackSetName' - The name or unique ID of the stack set whose description you want.
newDescribeStackSet ::
  -- | 'stackSetName'
  Prelude.Text ->
  DescribeStackSet
newDescribeStackSet pStackSetName_ =
  DescribeStackSet'
    { callAs = Prelude.Nothing,
      stackSetName = pStackSetName_
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
describeStackSet_callAs :: Lens.Lens' DescribeStackSet (Prelude.Maybe CallAs)
describeStackSet_callAs = Lens.lens (\DescribeStackSet' {callAs} -> callAs) (\s@DescribeStackSet' {} a -> s {callAs = a} :: DescribeStackSet)

-- | The name or unique ID of the stack set whose description you want.
describeStackSet_stackSetName :: Lens.Lens' DescribeStackSet Prelude.Text
describeStackSet_stackSetName = Lens.lens (\DescribeStackSet' {stackSetName} -> stackSetName) (\s@DescribeStackSet' {} a -> s {stackSetName = a} :: DescribeStackSet)

instance Core.AWSRequest DescribeStackSet where
  type
    AWSResponse DescribeStackSet =
      DescribeStackSetResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeStackSetResult"
      ( \s h x ->
          DescribeStackSetResponse'
            Prelude.<$> (x Core..@? "StackSet")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeStackSet

instance Prelude.NFData DescribeStackSet

instance Core.ToHeaders DescribeStackSet where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DescribeStackSet where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeStackSet where
  toQuery DescribeStackSet' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("DescribeStackSet" :: Prelude.ByteString),
        "Version"
          Core.=: ("2010-05-15" :: Prelude.ByteString),
        "CallAs" Core.=: callAs,
        "StackSetName" Core.=: stackSetName
      ]

-- | /See:/ 'newDescribeStackSetResponse' smart constructor.
data DescribeStackSetResponse = DescribeStackSetResponse'
  { -- | The specified stack set.
    stackSet :: Prelude.Maybe StackSet,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeStackSetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stackSet', 'describeStackSetResponse_stackSet' - The specified stack set.
--
-- 'httpStatus', 'describeStackSetResponse_httpStatus' - The response's http status code.
newDescribeStackSetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeStackSetResponse
newDescribeStackSetResponse pHttpStatus_ =
  DescribeStackSetResponse'
    { stackSet =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The specified stack set.
describeStackSetResponse_stackSet :: Lens.Lens' DescribeStackSetResponse (Prelude.Maybe StackSet)
describeStackSetResponse_stackSet = Lens.lens (\DescribeStackSetResponse' {stackSet} -> stackSet) (\s@DescribeStackSetResponse' {} a -> s {stackSet = a} :: DescribeStackSetResponse)

-- | The response's http status code.
describeStackSetResponse_httpStatus :: Lens.Lens' DescribeStackSetResponse Prelude.Int
describeStackSetResponse_httpStatus = Lens.lens (\DescribeStackSetResponse' {httpStatus} -> httpStatus) (\s@DescribeStackSetResponse' {} a -> s {httpStatus = a} :: DescribeStackSetResponse)

instance Prelude.NFData DescribeStackSetResponse
