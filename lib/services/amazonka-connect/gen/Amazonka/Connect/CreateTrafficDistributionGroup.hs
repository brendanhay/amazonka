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
-- Module      : Amazonka.Connect.CreateTrafficDistributionGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a traffic distribution group given an Amazon Connect instance
-- that has been replicated.
--
-- For more information about creating traffic distribution groups, see
-- <https://docs.aws.amazon.com/connect/latest/adminguide/setup-traffic-distribution-groups.html Set up traffic distribution groups>
-- in the /Amazon Connect Administrator Guide/.
module Amazonka.Connect.CreateTrafficDistributionGroup
  ( -- * Creating a Request
    CreateTrafficDistributionGroup (..),
    newCreateTrafficDistributionGroup,

    -- * Request Lenses
    createTrafficDistributionGroup_clientToken,
    createTrafficDistributionGroup_description,
    createTrafficDistributionGroup_tags,
    createTrafficDistributionGroup_name,
    createTrafficDistributionGroup_instanceId,

    -- * Destructuring the Response
    CreateTrafficDistributionGroupResponse (..),
    newCreateTrafficDistributionGroupResponse,

    -- * Response Lenses
    createTrafficDistributionGroupResponse_arn,
    createTrafficDistributionGroupResponse_id,
    createTrafficDistributionGroupResponse_httpStatus,
  )
where

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateTrafficDistributionGroup' smart constructor.
data CreateTrafficDistributionGroup = CreateTrafficDistributionGroup'
  { -- | A unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request. If not provided, the Amazon Web Services SDK
    -- populates this field. For more information about idempotency, see
    -- <https://aws.amazon.com/builders-library/making-retries-safe-with-idempotent-APIs/ Making retries safe with idempotent APIs>.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | A description for the traffic distribution group.
    description :: Prelude.Maybe Prelude.Text,
    -- | The tags used to organize, track, or control access for this resource.
    -- For example, { \"tags\": {\"key1\":\"value1\", \"key2\":\"value2\"} }.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name for the traffic distribution group.
    name :: Prelude.Text,
    -- | The identifier of the Amazon Connect instance that has been replicated.
    -- You can find the @instanceId@ in the ARN of the instance.
    instanceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateTrafficDistributionGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'createTrafficDistributionGroup_clientToken' - A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. If not provided, the Amazon Web Services SDK
-- populates this field. For more information about idempotency, see
-- <https://aws.amazon.com/builders-library/making-retries-safe-with-idempotent-APIs/ Making retries safe with idempotent APIs>.
--
-- 'description', 'createTrafficDistributionGroup_description' - A description for the traffic distribution group.
--
-- 'tags', 'createTrafficDistributionGroup_tags' - The tags used to organize, track, or control access for this resource.
-- For example, { \"tags\": {\"key1\":\"value1\", \"key2\":\"value2\"} }.
--
-- 'name', 'createTrafficDistributionGroup_name' - The name for the traffic distribution group.
--
-- 'instanceId', 'createTrafficDistributionGroup_instanceId' - The identifier of the Amazon Connect instance that has been replicated.
-- You can find the @instanceId@ in the ARN of the instance.
newCreateTrafficDistributionGroup ::
  -- | 'name'
  Prelude.Text ->
  -- | 'instanceId'
  Prelude.Text ->
  CreateTrafficDistributionGroup
newCreateTrafficDistributionGroup pName_ pInstanceId_ =
  CreateTrafficDistributionGroup'
    { clientToken =
        Prelude.Nothing,
      description = Prelude.Nothing,
      tags = Prelude.Nothing,
      name = pName_,
      instanceId = pInstanceId_
    }

-- | A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. If not provided, the Amazon Web Services SDK
-- populates this field. For more information about idempotency, see
-- <https://aws.amazon.com/builders-library/making-retries-safe-with-idempotent-APIs/ Making retries safe with idempotent APIs>.
createTrafficDistributionGroup_clientToken :: Lens.Lens' CreateTrafficDistributionGroup (Prelude.Maybe Prelude.Text)
createTrafficDistributionGroup_clientToken = Lens.lens (\CreateTrafficDistributionGroup' {clientToken} -> clientToken) (\s@CreateTrafficDistributionGroup' {} a -> s {clientToken = a} :: CreateTrafficDistributionGroup)

-- | A description for the traffic distribution group.
createTrafficDistributionGroup_description :: Lens.Lens' CreateTrafficDistributionGroup (Prelude.Maybe Prelude.Text)
createTrafficDistributionGroup_description = Lens.lens (\CreateTrafficDistributionGroup' {description} -> description) (\s@CreateTrafficDistributionGroup' {} a -> s {description = a} :: CreateTrafficDistributionGroup)

-- | The tags used to organize, track, or control access for this resource.
-- For example, { \"tags\": {\"key1\":\"value1\", \"key2\":\"value2\"} }.
createTrafficDistributionGroup_tags :: Lens.Lens' CreateTrafficDistributionGroup (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createTrafficDistributionGroup_tags = Lens.lens (\CreateTrafficDistributionGroup' {tags} -> tags) (\s@CreateTrafficDistributionGroup' {} a -> s {tags = a} :: CreateTrafficDistributionGroup) Prelude.. Lens.mapping Lens.coerced

-- | The name for the traffic distribution group.
createTrafficDistributionGroup_name :: Lens.Lens' CreateTrafficDistributionGroup Prelude.Text
createTrafficDistributionGroup_name = Lens.lens (\CreateTrafficDistributionGroup' {name} -> name) (\s@CreateTrafficDistributionGroup' {} a -> s {name = a} :: CreateTrafficDistributionGroup)

-- | The identifier of the Amazon Connect instance that has been replicated.
-- You can find the @instanceId@ in the ARN of the instance.
createTrafficDistributionGroup_instanceId :: Lens.Lens' CreateTrafficDistributionGroup Prelude.Text
createTrafficDistributionGroup_instanceId = Lens.lens (\CreateTrafficDistributionGroup' {instanceId} -> instanceId) (\s@CreateTrafficDistributionGroup' {} a -> s {instanceId = a} :: CreateTrafficDistributionGroup)

instance
  Core.AWSRequest
    CreateTrafficDistributionGroup
  where
  type
    AWSResponse CreateTrafficDistributionGroup =
      CreateTrafficDistributionGroupResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateTrafficDistributionGroupResponse'
            Prelude.<$> (x Data..?> "Arn")
            Prelude.<*> (x Data..?> "Id")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    CreateTrafficDistributionGroup
  where
  hashWithSalt
    _salt
    CreateTrafficDistributionGroup' {..} =
      _salt
        `Prelude.hashWithSalt` clientToken
        `Prelude.hashWithSalt` description
        `Prelude.hashWithSalt` tags
        `Prelude.hashWithSalt` name
        `Prelude.hashWithSalt` instanceId

instance
  Prelude.NFData
    CreateTrafficDistributionGroup
  where
  rnf CreateTrafficDistributionGroup' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf instanceId

instance
  Data.ToHeaders
    CreateTrafficDistributionGroup
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateTrafficDistributionGroup where
  toJSON CreateTrafficDistributionGroup' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClientToken" Data..=) Prelude.<$> clientToken,
            ("Description" Data..=) Prelude.<$> description,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("Name" Data..= name),
            Prelude.Just ("InstanceId" Data..= instanceId)
          ]
      )

instance Data.ToPath CreateTrafficDistributionGroup where
  toPath = Prelude.const "/traffic-distribution-group"

instance Data.ToQuery CreateTrafficDistributionGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateTrafficDistributionGroupResponse' smart constructor.
data CreateTrafficDistributionGroupResponse = CreateTrafficDistributionGroupResponse'
  { -- | The Amazon Resource Name (ARN) of the traffic distribution group.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the traffic distribution group. This can be the ID or
    -- the ARN if the API is being called in the Region where the traffic
    -- distribution group was created. The ARN must be provided if the call is
    -- from the replicated Region.
    id :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateTrafficDistributionGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'createTrafficDistributionGroupResponse_arn' - The Amazon Resource Name (ARN) of the traffic distribution group.
--
-- 'id', 'createTrafficDistributionGroupResponse_id' - The identifier of the traffic distribution group. This can be the ID or
-- the ARN if the API is being called in the Region where the traffic
-- distribution group was created. The ARN must be provided if the call is
-- from the replicated Region.
--
-- 'httpStatus', 'createTrafficDistributionGroupResponse_httpStatus' - The response's http status code.
newCreateTrafficDistributionGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateTrafficDistributionGroupResponse
newCreateTrafficDistributionGroupResponse
  pHttpStatus_ =
    CreateTrafficDistributionGroupResponse'
      { arn =
          Prelude.Nothing,
        id = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The Amazon Resource Name (ARN) of the traffic distribution group.
createTrafficDistributionGroupResponse_arn :: Lens.Lens' CreateTrafficDistributionGroupResponse (Prelude.Maybe Prelude.Text)
createTrafficDistributionGroupResponse_arn = Lens.lens (\CreateTrafficDistributionGroupResponse' {arn} -> arn) (\s@CreateTrafficDistributionGroupResponse' {} a -> s {arn = a} :: CreateTrafficDistributionGroupResponse)

-- | The identifier of the traffic distribution group. This can be the ID or
-- the ARN if the API is being called in the Region where the traffic
-- distribution group was created. The ARN must be provided if the call is
-- from the replicated Region.
createTrafficDistributionGroupResponse_id :: Lens.Lens' CreateTrafficDistributionGroupResponse (Prelude.Maybe Prelude.Text)
createTrafficDistributionGroupResponse_id = Lens.lens (\CreateTrafficDistributionGroupResponse' {id} -> id) (\s@CreateTrafficDistributionGroupResponse' {} a -> s {id = a} :: CreateTrafficDistributionGroupResponse)

-- | The response's http status code.
createTrafficDistributionGroupResponse_httpStatus :: Lens.Lens' CreateTrafficDistributionGroupResponse Prelude.Int
createTrafficDistributionGroupResponse_httpStatus = Lens.lens (\CreateTrafficDistributionGroupResponse' {httpStatus} -> httpStatus) (\s@CreateTrafficDistributionGroupResponse' {} a -> s {httpStatus = a} :: CreateTrafficDistributionGroupResponse)

instance
  Prelude.NFData
    CreateTrafficDistributionGroupResponse
  where
  rnf CreateTrafficDistributionGroupResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf httpStatus
