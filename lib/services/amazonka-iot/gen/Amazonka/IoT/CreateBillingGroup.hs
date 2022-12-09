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
-- Module      : Amazonka.IoT.CreateBillingGroup
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a billing group.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions CreateBillingGroup>
-- action.
module Amazonka.IoT.CreateBillingGroup
  ( -- * Creating a Request
    CreateBillingGroup (..),
    newCreateBillingGroup,

    -- * Request Lenses
    createBillingGroup_billingGroupProperties,
    createBillingGroup_tags,
    createBillingGroup_billingGroupName,

    -- * Destructuring the Response
    CreateBillingGroupResponse (..),
    newCreateBillingGroupResponse,

    -- * Response Lenses
    createBillingGroupResponse_billingGroupArn,
    createBillingGroupResponse_billingGroupId,
    createBillingGroupResponse_billingGroupName,
    createBillingGroupResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateBillingGroup' smart constructor.
data CreateBillingGroup = CreateBillingGroup'
  { -- | The properties of the billing group.
    billingGroupProperties :: Prelude.Maybe BillingGroupProperties,
    -- | Metadata which can be used to manage the billing group.
    tags :: Prelude.Maybe [Tag],
    -- | The name you wish to give to the billing group.
    billingGroupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateBillingGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'billingGroupProperties', 'createBillingGroup_billingGroupProperties' - The properties of the billing group.
--
-- 'tags', 'createBillingGroup_tags' - Metadata which can be used to manage the billing group.
--
-- 'billingGroupName', 'createBillingGroup_billingGroupName' - The name you wish to give to the billing group.
newCreateBillingGroup ::
  -- | 'billingGroupName'
  Prelude.Text ->
  CreateBillingGroup
newCreateBillingGroup pBillingGroupName_ =
  CreateBillingGroup'
    { billingGroupProperties =
        Prelude.Nothing,
      tags = Prelude.Nothing,
      billingGroupName = pBillingGroupName_
    }

-- | The properties of the billing group.
createBillingGroup_billingGroupProperties :: Lens.Lens' CreateBillingGroup (Prelude.Maybe BillingGroupProperties)
createBillingGroup_billingGroupProperties = Lens.lens (\CreateBillingGroup' {billingGroupProperties} -> billingGroupProperties) (\s@CreateBillingGroup' {} a -> s {billingGroupProperties = a} :: CreateBillingGroup)

-- | Metadata which can be used to manage the billing group.
createBillingGroup_tags :: Lens.Lens' CreateBillingGroup (Prelude.Maybe [Tag])
createBillingGroup_tags = Lens.lens (\CreateBillingGroup' {tags} -> tags) (\s@CreateBillingGroup' {} a -> s {tags = a} :: CreateBillingGroup) Prelude.. Lens.mapping Lens.coerced

-- | The name you wish to give to the billing group.
createBillingGroup_billingGroupName :: Lens.Lens' CreateBillingGroup Prelude.Text
createBillingGroup_billingGroupName = Lens.lens (\CreateBillingGroup' {billingGroupName} -> billingGroupName) (\s@CreateBillingGroup' {} a -> s {billingGroupName = a} :: CreateBillingGroup)

instance Core.AWSRequest CreateBillingGroup where
  type
    AWSResponse CreateBillingGroup =
      CreateBillingGroupResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateBillingGroupResponse'
            Prelude.<$> (x Data..?> "billingGroupArn")
            Prelude.<*> (x Data..?> "billingGroupId")
            Prelude.<*> (x Data..?> "billingGroupName")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateBillingGroup where
  hashWithSalt _salt CreateBillingGroup' {..} =
    _salt `Prelude.hashWithSalt` billingGroupProperties
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` billingGroupName

instance Prelude.NFData CreateBillingGroup where
  rnf CreateBillingGroup' {..} =
    Prelude.rnf billingGroupProperties
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf billingGroupName

instance Data.ToHeaders CreateBillingGroup where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON CreateBillingGroup where
  toJSON CreateBillingGroup' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("billingGroupProperties" Data..=)
              Prelude.<$> billingGroupProperties,
            ("tags" Data..=) Prelude.<$> tags
          ]
      )

instance Data.ToPath CreateBillingGroup where
  toPath CreateBillingGroup' {..} =
    Prelude.mconcat
      ["/billing-groups/", Data.toBS billingGroupName]

instance Data.ToQuery CreateBillingGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateBillingGroupResponse' smart constructor.
data CreateBillingGroupResponse = CreateBillingGroupResponse'
  { -- | The ARN of the billing group.
    billingGroupArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the billing group.
    billingGroupId :: Prelude.Maybe Prelude.Text,
    -- | The name you gave to the billing group.
    billingGroupName :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateBillingGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'billingGroupArn', 'createBillingGroupResponse_billingGroupArn' - The ARN of the billing group.
--
-- 'billingGroupId', 'createBillingGroupResponse_billingGroupId' - The ID of the billing group.
--
-- 'billingGroupName', 'createBillingGroupResponse_billingGroupName' - The name you gave to the billing group.
--
-- 'httpStatus', 'createBillingGroupResponse_httpStatus' - The response's http status code.
newCreateBillingGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateBillingGroupResponse
newCreateBillingGroupResponse pHttpStatus_ =
  CreateBillingGroupResponse'
    { billingGroupArn =
        Prelude.Nothing,
      billingGroupId = Prelude.Nothing,
      billingGroupName = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the billing group.
createBillingGroupResponse_billingGroupArn :: Lens.Lens' CreateBillingGroupResponse (Prelude.Maybe Prelude.Text)
createBillingGroupResponse_billingGroupArn = Lens.lens (\CreateBillingGroupResponse' {billingGroupArn} -> billingGroupArn) (\s@CreateBillingGroupResponse' {} a -> s {billingGroupArn = a} :: CreateBillingGroupResponse)

-- | The ID of the billing group.
createBillingGroupResponse_billingGroupId :: Lens.Lens' CreateBillingGroupResponse (Prelude.Maybe Prelude.Text)
createBillingGroupResponse_billingGroupId = Lens.lens (\CreateBillingGroupResponse' {billingGroupId} -> billingGroupId) (\s@CreateBillingGroupResponse' {} a -> s {billingGroupId = a} :: CreateBillingGroupResponse)

-- | The name you gave to the billing group.
createBillingGroupResponse_billingGroupName :: Lens.Lens' CreateBillingGroupResponse (Prelude.Maybe Prelude.Text)
createBillingGroupResponse_billingGroupName = Lens.lens (\CreateBillingGroupResponse' {billingGroupName} -> billingGroupName) (\s@CreateBillingGroupResponse' {} a -> s {billingGroupName = a} :: CreateBillingGroupResponse)

-- | The response's http status code.
createBillingGroupResponse_httpStatus :: Lens.Lens' CreateBillingGroupResponse Prelude.Int
createBillingGroupResponse_httpStatus = Lens.lens (\CreateBillingGroupResponse' {httpStatus} -> httpStatus) (\s@CreateBillingGroupResponse' {} a -> s {httpStatus = a} :: CreateBillingGroupResponse)

instance Prelude.NFData CreateBillingGroupResponse where
  rnf CreateBillingGroupResponse' {..} =
    Prelude.rnf billingGroupArn
      `Prelude.seq` Prelude.rnf billingGroupId
      `Prelude.seq` Prelude.rnf billingGroupName
      `Prelude.seq` Prelude.rnf httpStatus
