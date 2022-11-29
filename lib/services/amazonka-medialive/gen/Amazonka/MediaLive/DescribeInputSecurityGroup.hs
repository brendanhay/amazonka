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
-- Module      : Amazonka.MediaLive.DescribeInputSecurityGroup
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Produces a summary of an Input Security Group
module Amazonka.MediaLive.DescribeInputSecurityGroup
  ( -- * Creating a Request
    DescribeInputSecurityGroup (..),
    newDescribeInputSecurityGroup,

    -- * Request Lenses
    describeInputSecurityGroup_inputSecurityGroupId,

    -- * Destructuring the Response
    DescribeInputSecurityGroupResponse (..),
    newDescribeInputSecurityGroupResponse,

    -- * Response Lenses
    describeInputSecurityGroupResponse_tags,
    describeInputSecurityGroupResponse_arn,
    describeInputSecurityGroupResponse_state,
    describeInputSecurityGroupResponse_id,
    describeInputSecurityGroupResponse_whitelistRules,
    describeInputSecurityGroupResponse_inputs,
    describeInputSecurityGroupResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MediaLive.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Placeholder documentation for DescribeInputSecurityGroupRequest
--
-- /See:/ 'newDescribeInputSecurityGroup' smart constructor.
data DescribeInputSecurityGroup = DescribeInputSecurityGroup'
  { -- | The id of the Input Security Group to describe
    inputSecurityGroupId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeInputSecurityGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'inputSecurityGroupId', 'describeInputSecurityGroup_inputSecurityGroupId' - The id of the Input Security Group to describe
newDescribeInputSecurityGroup ::
  -- | 'inputSecurityGroupId'
  Prelude.Text ->
  DescribeInputSecurityGroup
newDescribeInputSecurityGroup pInputSecurityGroupId_ =
  DescribeInputSecurityGroup'
    { inputSecurityGroupId =
        pInputSecurityGroupId_
    }

-- | The id of the Input Security Group to describe
describeInputSecurityGroup_inputSecurityGroupId :: Lens.Lens' DescribeInputSecurityGroup Prelude.Text
describeInputSecurityGroup_inputSecurityGroupId = Lens.lens (\DescribeInputSecurityGroup' {inputSecurityGroupId} -> inputSecurityGroupId) (\s@DescribeInputSecurityGroup' {} a -> s {inputSecurityGroupId = a} :: DescribeInputSecurityGroup)

instance Core.AWSRequest DescribeInputSecurityGroup where
  type
    AWSResponse DescribeInputSecurityGroup =
      DescribeInputSecurityGroupResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeInputSecurityGroupResponse'
            Prelude.<$> (x Core..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "arn")
            Prelude.<*> (x Core..?> "state")
            Prelude.<*> (x Core..?> "id")
            Prelude.<*> (x Core..?> "whitelistRules" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "inputs" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeInputSecurityGroup where
  hashWithSalt _salt DescribeInputSecurityGroup' {..} =
    _salt `Prelude.hashWithSalt` inputSecurityGroupId

instance Prelude.NFData DescribeInputSecurityGroup where
  rnf DescribeInputSecurityGroup' {..} =
    Prelude.rnf inputSecurityGroupId

instance Core.ToHeaders DescribeInputSecurityGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DescribeInputSecurityGroup where
  toPath DescribeInputSecurityGroup' {..} =
    Prelude.mconcat
      [ "/prod/inputSecurityGroups/",
        Core.toBS inputSecurityGroupId
      ]

instance Core.ToQuery DescribeInputSecurityGroup where
  toQuery = Prelude.const Prelude.mempty

-- | Placeholder documentation for DescribeInputSecurityGroupResponse
--
-- /See:/ 'newDescribeInputSecurityGroupResponse' smart constructor.
data DescribeInputSecurityGroupResponse = DescribeInputSecurityGroupResponse'
  { -- | A collection of key-value pairs.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Unique ARN of Input Security Group
    arn :: Prelude.Maybe Prelude.Text,
    -- | The current state of the Input Security Group.
    state :: Prelude.Maybe InputSecurityGroupState,
    -- | The Id of the Input Security Group
    id :: Prelude.Maybe Prelude.Text,
    -- | Whitelist rules and their sync status
    whitelistRules :: Prelude.Maybe [InputWhitelistRule],
    -- | The list of inputs currently using this Input Security Group.
    inputs :: Prelude.Maybe [Prelude.Text],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeInputSecurityGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'describeInputSecurityGroupResponse_tags' - A collection of key-value pairs.
--
-- 'arn', 'describeInputSecurityGroupResponse_arn' - Unique ARN of Input Security Group
--
-- 'state', 'describeInputSecurityGroupResponse_state' - The current state of the Input Security Group.
--
-- 'id', 'describeInputSecurityGroupResponse_id' - The Id of the Input Security Group
--
-- 'whitelistRules', 'describeInputSecurityGroupResponse_whitelistRules' - Whitelist rules and their sync status
--
-- 'inputs', 'describeInputSecurityGroupResponse_inputs' - The list of inputs currently using this Input Security Group.
--
-- 'httpStatus', 'describeInputSecurityGroupResponse_httpStatus' - The response's http status code.
newDescribeInputSecurityGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeInputSecurityGroupResponse
newDescribeInputSecurityGroupResponse pHttpStatus_ =
  DescribeInputSecurityGroupResponse'
    { tags =
        Prelude.Nothing,
      arn = Prelude.Nothing,
      state = Prelude.Nothing,
      id = Prelude.Nothing,
      whitelistRules = Prelude.Nothing,
      inputs = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A collection of key-value pairs.
describeInputSecurityGroupResponse_tags :: Lens.Lens' DescribeInputSecurityGroupResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
describeInputSecurityGroupResponse_tags = Lens.lens (\DescribeInputSecurityGroupResponse' {tags} -> tags) (\s@DescribeInputSecurityGroupResponse' {} a -> s {tags = a} :: DescribeInputSecurityGroupResponse) Prelude.. Lens.mapping Lens.coerced

-- | Unique ARN of Input Security Group
describeInputSecurityGroupResponse_arn :: Lens.Lens' DescribeInputSecurityGroupResponse (Prelude.Maybe Prelude.Text)
describeInputSecurityGroupResponse_arn = Lens.lens (\DescribeInputSecurityGroupResponse' {arn} -> arn) (\s@DescribeInputSecurityGroupResponse' {} a -> s {arn = a} :: DescribeInputSecurityGroupResponse)

-- | The current state of the Input Security Group.
describeInputSecurityGroupResponse_state :: Lens.Lens' DescribeInputSecurityGroupResponse (Prelude.Maybe InputSecurityGroupState)
describeInputSecurityGroupResponse_state = Lens.lens (\DescribeInputSecurityGroupResponse' {state} -> state) (\s@DescribeInputSecurityGroupResponse' {} a -> s {state = a} :: DescribeInputSecurityGroupResponse)

-- | The Id of the Input Security Group
describeInputSecurityGroupResponse_id :: Lens.Lens' DescribeInputSecurityGroupResponse (Prelude.Maybe Prelude.Text)
describeInputSecurityGroupResponse_id = Lens.lens (\DescribeInputSecurityGroupResponse' {id} -> id) (\s@DescribeInputSecurityGroupResponse' {} a -> s {id = a} :: DescribeInputSecurityGroupResponse)

-- | Whitelist rules and their sync status
describeInputSecurityGroupResponse_whitelistRules :: Lens.Lens' DescribeInputSecurityGroupResponse (Prelude.Maybe [InputWhitelistRule])
describeInputSecurityGroupResponse_whitelistRules = Lens.lens (\DescribeInputSecurityGroupResponse' {whitelistRules} -> whitelistRules) (\s@DescribeInputSecurityGroupResponse' {} a -> s {whitelistRules = a} :: DescribeInputSecurityGroupResponse) Prelude.. Lens.mapping Lens.coerced

-- | The list of inputs currently using this Input Security Group.
describeInputSecurityGroupResponse_inputs :: Lens.Lens' DescribeInputSecurityGroupResponse (Prelude.Maybe [Prelude.Text])
describeInputSecurityGroupResponse_inputs = Lens.lens (\DescribeInputSecurityGroupResponse' {inputs} -> inputs) (\s@DescribeInputSecurityGroupResponse' {} a -> s {inputs = a} :: DescribeInputSecurityGroupResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeInputSecurityGroupResponse_httpStatus :: Lens.Lens' DescribeInputSecurityGroupResponse Prelude.Int
describeInputSecurityGroupResponse_httpStatus = Lens.lens (\DescribeInputSecurityGroupResponse' {httpStatus} -> httpStatus) (\s@DescribeInputSecurityGroupResponse' {} a -> s {httpStatus = a} :: DescribeInputSecurityGroupResponse)

instance
  Prelude.NFData
    DescribeInputSecurityGroupResponse
  where
  rnf DescribeInputSecurityGroupResponse' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf whitelistRules
      `Prelude.seq` Prelude.rnf inputs
      `Prelude.seq` Prelude.rnf httpStatus
