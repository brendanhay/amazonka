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
-- Module      : Amazonka.VPCLattice.CreateTargetGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a target group. A target group is a collection of targets, or
-- compute resources, that run your application or service. A target group
-- can only be used by a single service.
--
-- For more information, see
-- <https://docs.aws.amazon.com/vpc-lattice/latest/ug/target-groups.html Target groups>
-- in the /Amazon VPC Lattice User Guide/.
module Amazonka.VPCLattice.CreateTargetGroup
  ( -- * Creating a Request
    CreateTargetGroup (..),
    newCreateTargetGroup,

    -- * Request Lenses
    createTargetGroup_clientToken,
    createTargetGroup_config,
    createTargetGroup_tags,
    createTargetGroup_name,
    createTargetGroup_type,

    -- * Destructuring the Response
    CreateTargetGroupResponse (..),
    newCreateTargetGroupResponse,

    -- * Response Lenses
    createTargetGroupResponse_arn,
    createTargetGroupResponse_config,
    createTargetGroupResponse_id,
    createTargetGroupResponse_name,
    createTargetGroupResponse_status,
    createTargetGroupResponse_type,
    createTargetGroupResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.VPCLattice.Types

-- | /See:/ 'newCreateTargetGroup' smart constructor.
data CreateTargetGroup = CreateTargetGroup'
  { -- | A unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request. If you retry a request that completed
    -- successfully using the same client token and parameters, the retry
    -- succeeds without performing any actions. If the parameters aren\'t
    -- identical, the retry fails.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The target group configuration. If @type@ is set to @LAMBDA@, this
    -- parameter doesn\'t apply.
    config :: Prelude.Maybe TargetGroupConfig,
    -- | The tags for the target group.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name of the target group. The name must be unique within the
    -- account. The valid characters are a-z, 0-9, and hyphens (-). You can\'t
    -- use a hyphen as the first or last character, or immediately after
    -- another hyphen.
    name :: Prelude.Text,
    -- | The type of target group.
    type' :: TargetGroupType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateTargetGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'createTargetGroup_clientToken' - A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. If you retry a request that completed
-- successfully using the same client token and parameters, the retry
-- succeeds without performing any actions. If the parameters aren\'t
-- identical, the retry fails.
--
-- 'config', 'createTargetGroup_config' - The target group configuration. If @type@ is set to @LAMBDA@, this
-- parameter doesn\'t apply.
--
-- 'tags', 'createTargetGroup_tags' - The tags for the target group.
--
-- 'name', 'createTargetGroup_name' - The name of the target group. The name must be unique within the
-- account. The valid characters are a-z, 0-9, and hyphens (-). You can\'t
-- use a hyphen as the first or last character, or immediately after
-- another hyphen.
--
-- 'type'', 'createTargetGroup_type' - The type of target group.
newCreateTargetGroup ::
  -- | 'name'
  Prelude.Text ->
  -- | 'type''
  TargetGroupType ->
  CreateTargetGroup
newCreateTargetGroup pName_ pType_ =
  CreateTargetGroup'
    { clientToken = Prelude.Nothing,
      config = Prelude.Nothing,
      tags = Prelude.Nothing,
      name = pName_,
      type' = pType_
    }

-- | A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. If you retry a request that completed
-- successfully using the same client token and parameters, the retry
-- succeeds without performing any actions. If the parameters aren\'t
-- identical, the retry fails.
createTargetGroup_clientToken :: Lens.Lens' CreateTargetGroup (Prelude.Maybe Prelude.Text)
createTargetGroup_clientToken = Lens.lens (\CreateTargetGroup' {clientToken} -> clientToken) (\s@CreateTargetGroup' {} a -> s {clientToken = a} :: CreateTargetGroup)

-- | The target group configuration. If @type@ is set to @LAMBDA@, this
-- parameter doesn\'t apply.
createTargetGroup_config :: Lens.Lens' CreateTargetGroup (Prelude.Maybe TargetGroupConfig)
createTargetGroup_config = Lens.lens (\CreateTargetGroup' {config} -> config) (\s@CreateTargetGroup' {} a -> s {config = a} :: CreateTargetGroup)

-- | The tags for the target group.
createTargetGroup_tags :: Lens.Lens' CreateTargetGroup (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createTargetGroup_tags = Lens.lens (\CreateTargetGroup' {tags} -> tags) (\s@CreateTargetGroup' {} a -> s {tags = a} :: CreateTargetGroup) Prelude.. Lens.mapping Lens.coerced

-- | The name of the target group. The name must be unique within the
-- account. The valid characters are a-z, 0-9, and hyphens (-). You can\'t
-- use a hyphen as the first or last character, or immediately after
-- another hyphen.
createTargetGroup_name :: Lens.Lens' CreateTargetGroup Prelude.Text
createTargetGroup_name = Lens.lens (\CreateTargetGroup' {name} -> name) (\s@CreateTargetGroup' {} a -> s {name = a} :: CreateTargetGroup)

-- | The type of target group.
createTargetGroup_type :: Lens.Lens' CreateTargetGroup TargetGroupType
createTargetGroup_type = Lens.lens (\CreateTargetGroup' {type'} -> type') (\s@CreateTargetGroup' {} a -> s {type' = a} :: CreateTargetGroup)

instance Core.AWSRequest CreateTargetGroup where
  type
    AWSResponse CreateTargetGroup =
      CreateTargetGroupResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateTargetGroupResponse'
            Prelude.<$> (x Data..?> "arn")
            Prelude.<*> (x Data..?> "config")
            Prelude.<*> (x Data..?> "id")
            Prelude.<*> (x Data..?> "name")
            Prelude.<*> (x Data..?> "status")
            Prelude.<*> (x Data..?> "type")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateTargetGroup where
  hashWithSalt _salt CreateTargetGroup' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` config
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` type'

instance Prelude.NFData CreateTargetGroup where
  rnf CreateTargetGroup' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf config
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf type'

instance Data.ToHeaders CreateTargetGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateTargetGroup where
  toJSON CreateTargetGroup' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("clientToken" Data..=) Prelude.<$> clientToken,
            ("config" Data..=) Prelude.<$> config,
            ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("name" Data..= name),
            Prelude.Just ("type" Data..= type')
          ]
      )

instance Data.ToPath CreateTargetGroup where
  toPath = Prelude.const "/targetgroups"

instance Data.ToQuery CreateTargetGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateTargetGroupResponse' smart constructor.
data CreateTargetGroupResponse = CreateTargetGroupResponse'
  { -- | The Amazon Resource Name (ARN) of the target group.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The target group configuration. If @type@ is set to @LAMBDA@, this
    -- parameter doesn\'t apply.
    config :: Prelude.Maybe TargetGroupConfig,
    -- | The ID of the target group.
    id :: Prelude.Maybe Prelude.Text,
    -- | The name of the target group.
    name :: Prelude.Maybe Prelude.Text,
    -- | The operation\'s status. You can retry the operation if the status is
    -- @CREATE_FAILED@. However, if you retry it while the status is
    -- @CREATE_IN_PROGRESS@, there is no change in the status.
    status :: Prelude.Maybe TargetGroupStatus,
    -- | The type of target group.
    type' :: Prelude.Maybe TargetGroupType,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateTargetGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'createTargetGroupResponse_arn' - The Amazon Resource Name (ARN) of the target group.
--
-- 'config', 'createTargetGroupResponse_config' - The target group configuration. If @type@ is set to @LAMBDA@, this
-- parameter doesn\'t apply.
--
-- 'id', 'createTargetGroupResponse_id' - The ID of the target group.
--
-- 'name', 'createTargetGroupResponse_name' - The name of the target group.
--
-- 'status', 'createTargetGroupResponse_status' - The operation\'s status. You can retry the operation if the status is
-- @CREATE_FAILED@. However, if you retry it while the status is
-- @CREATE_IN_PROGRESS@, there is no change in the status.
--
-- 'type'', 'createTargetGroupResponse_type' - The type of target group.
--
-- 'httpStatus', 'createTargetGroupResponse_httpStatus' - The response's http status code.
newCreateTargetGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateTargetGroupResponse
newCreateTargetGroupResponse pHttpStatus_ =
  CreateTargetGroupResponse'
    { arn = Prelude.Nothing,
      config = Prelude.Nothing,
      id = Prelude.Nothing,
      name = Prelude.Nothing,
      status = Prelude.Nothing,
      type' = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the target group.
createTargetGroupResponse_arn :: Lens.Lens' CreateTargetGroupResponse (Prelude.Maybe Prelude.Text)
createTargetGroupResponse_arn = Lens.lens (\CreateTargetGroupResponse' {arn} -> arn) (\s@CreateTargetGroupResponse' {} a -> s {arn = a} :: CreateTargetGroupResponse)

-- | The target group configuration. If @type@ is set to @LAMBDA@, this
-- parameter doesn\'t apply.
createTargetGroupResponse_config :: Lens.Lens' CreateTargetGroupResponse (Prelude.Maybe TargetGroupConfig)
createTargetGroupResponse_config = Lens.lens (\CreateTargetGroupResponse' {config} -> config) (\s@CreateTargetGroupResponse' {} a -> s {config = a} :: CreateTargetGroupResponse)

-- | The ID of the target group.
createTargetGroupResponse_id :: Lens.Lens' CreateTargetGroupResponse (Prelude.Maybe Prelude.Text)
createTargetGroupResponse_id = Lens.lens (\CreateTargetGroupResponse' {id} -> id) (\s@CreateTargetGroupResponse' {} a -> s {id = a} :: CreateTargetGroupResponse)

-- | The name of the target group.
createTargetGroupResponse_name :: Lens.Lens' CreateTargetGroupResponse (Prelude.Maybe Prelude.Text)
createTargetGroupResponse_name = Lens.lens (\CreateTargetGroupResponse' {name} -> name) (\s@CreateTargetGroupResponse' {} a -> s {name = a} :: CreateTargetGroupResponse)

-- | The operation\'s status. You can retry the operation if the status is
-- @CREATE_FAILED@. However, if you retry it while the status is
-- @CREATE_IN_PROGRESS@, there is no change in the status.
createTargetGroupResponse_status :: Lens.Lens' CreateTargetGroupResponse (Prelude.Maybe TargetGroupStatus)
createTargetGroupResponse_status = Lens.lens (\CreateTargetGroupResponse' {status} -> status) (\s@CreateTargetGroupResponse' {} a -> s {status = a} :: CreateTargetGroupResponse)

-- | The type of target group.
createTargetGroupResponse_type :: Lens.Lens' CreateTargetGroupResponse (Prelude.Maybe TargetGroupType)
createTargetGroupResponse_type = Lens.lens (\CreateTargetGroupResponse' {type'} -> type') (\s@CreateTargetGroupResponse' {} a -> s {type' = a} :: CreateTargetGroupResponse)

-- | The response's http status code.
createTargetGroupResponse_httpStatus :: Lens.Lens' CreateTargetGroupResponse Prelude.Int
createTargetGroupResponse_httpStatus = Lens.lens (\CreateTargetGroupResponse' {httpStatus} -> httpStatus) (\s@CreateTargetGroupResponse' {} a -> s {httpStatus = a} :: CreateTargetGroupResponse)

instance Prelude.NFData CreateTargetGroupResponse where
  rnf CreateTargetGroupResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf config
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf httpStatus
