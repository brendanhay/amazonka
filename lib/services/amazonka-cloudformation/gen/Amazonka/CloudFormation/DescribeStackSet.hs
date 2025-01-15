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
-- Module      : Amazonka.CloudFormation.DescribeStackSet
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the description of the specified stack set.
module Amazonka.CloudFormation.DescribeStackSet
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

import Amazonka.CloudFormation.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
    --     Your Amazon Web Services account must be registered as a delegated
    --     administrator in the management account. For more information, see
    --     <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-orgs-delegated-admin.html Register a delegated administrator>
    --     in the /CloudFormation User Guide/.
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
--     Your Amazon Web Services account must be registered as a delegated
--     administrator in the management account. For more information, see
--     <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-orgs-delegated-admin.html Register a delegated administrator>
--     in the /CloudFormation User Guide/.
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
--     Your Amazon Web Services account must be registered as a delegated
--     administrator in the management account. For more information, see
--     <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-orgs-delegated-admin.html Register a delegated administrator>
--     in the /CloudFormation User Guide/.
describeStackSet_callAs :: Lens.Lens' DescribeStackSet (Prelude.Maybe CallAs)
describeStackSet_callAs = Lens.lens (\DescribeStackSet' {callAs} -> callAs) (\s@DescribeStackSet' {} a -> s {callAs = a} :: DescribeStackSet)

-- | The name or unique ID of the stack set whose description you want.
describeStackSet_stackSetName :: Lens.Lens' DescribeStackSet Prelude.Text
describeStackSet_stackSetName = Lens.lens (\DescribeStackSet' {stackSetName} -> stackSetName) (\s@DescribeStackSet' {} a -> s {stackSetName = a} :: DescribeStackSet)

instance Core.AWSRequest DescribeStackSet where
  type
    AWSResponse DescribeStackSet =
      DescribeStackSetResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DescribeStackSetResult"
      ( \s h x ->
          DescribeStackSetResponse'
            Prelude.<$> (x Data..@? "StackSet")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeStackSet where
  hashWithSalt _salt DescribeStackSet' {..} =
    _salt
      `Prelude.hashWithSalt` callAs
      `Prelude.hashWithSalt` stackSetName

instance Prelude.NFData DescribeStackSet where
  rnf DescribeStackSet' {..} =
    Prelude.rnf callAs `Prelude.seq`
      Prelude.rnf stackSetName

instance Data.ToHeaders DescribeStackSet where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeStackSet where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeStackSet where
  toQuery DescribeStackSet' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DescribeStackSet" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-05-15" :: Prelude.ByteString),
        "CallAs" Data.=: callAs,
        "StackSetName" Data.=: stackSetName
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

instance Prelude.NFData DescribeStackSetResponse where
  rnf DescribeStackSetResponse' {..} =
    Prelude.rnf stackSet `Prelude.seq`
      Prelude.rnf httpStatus
