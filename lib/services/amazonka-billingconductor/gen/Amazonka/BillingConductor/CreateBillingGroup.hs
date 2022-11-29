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
-- Module      : Amazonka.BillingConductor.CreateBillingGroup
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a billing group that resembles a consolidated billing family
-- that Amazon Web Services charges, based off of the predefined pricing
-- plan computation.
module Amazonka.BillingConductor.CreateBillingGroup
  ( -- * Creating a Request
    CreateBillingGroup (..),
    newCreateBillingGroup,

    -- * Request Lenses
    createBillingGroup_tags,
    createBillingGroup_clientToken,
    createBillingGroup_description,
    createBillingGroup_primaryAccountId,
    createBillingGroup_name,
    createBillingGroup_accountGrouping,
    createBillingGroup_computationPreference,

    -- * Destructuring the Response
    CreateBillingGroupResponse (..),
    newCreateBillingGroupResponse,

    -- * Response Lenses
    createBillingGroupResponse_arn,
    createBillingGroupResponse_httpStatus,
  )
where

import Amazonka.BillingConductor.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateBillingGroup' smart constructor.
data CreateBillingGroup = CreateBillingGroup'
  { -- | A map that contains tag keys and tag values that are attached to a
    -- billing group. This feature isn\'t available during the beta.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The token that is needed to support idempotency. Idempotency isn\'t
    -- currently supported, but will be implemented in a future update.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The description of the billing group.
    description :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The account ID that serves as the main account in a billing group.
    primaryAccountId :: Prelude.Maybe Prelude.Text,
    -- | The billing group name. The names must be unique.
    name :: Core.Sensitive Prelude.Text,
    -- | The set of accounts that will be under the billing group. The set of
    -- accounts resemble the linked accounts in a consolidated family.
    accountGrouping :: AccountGrouping,
    -- | The preferences and settings that will be used to compute the Amazon Web
    -- Services charges for a billing group.
    computationPreference :: ComputationPreference
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateBillingGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createBillingGroup_tags' - A map that contains tag keys and tag values that are attached to a
-- billing group. This feature isn\'t available during the beta.
--
-- 'clientToken', 'createBillingGroup_clientToken' - The token that is needed to support idempotency. Idempotency isn\'t
-- currently supported, but will be implemented in a future update.
--
-- 'description', 'createBillingGroup_description' - The description of the billing group.
--
-- 'primaryAccountId', 'createBillingGroup_primaryAccountId' - The account ID that serves as the main account in a billing group.
--
-- 'name', 'createBillingGroup_name' - The billing group name. The names must be unique.
--
-- 'accountGrouping', 'createBillingGroup_accountGrouping' - The set of accounts that will be under the billing group. The set of
-- accounts resemble the linked accounts in a consolidated family.
--
-- 'computationPreference', 'createBillingGroup_computationPreference' - The preferences and settings that will be used to compute the Amazon Web
-- Services charges for a billing group.
newCreateBillingGroup ::
  -- | 'name'
  Prelude.Text ->
  -- | 'accountGrouping'
  AccountGrouping ->
  -- | 'computationPreference'
  ComputationPreference ->
  CreateBillingGroup
newCreateBillingGroup
  pName_
  pAccountGrouping_
  pComputationPreference_ =
    CreateBillingGroup'
      { tags = Prelude.Nothing,
        clientToken = Prelude.Nothing,
        description = Prelude.Nothing,
        primaryAccountId = Prelude.Nothing,
        name = Core._Sensitive Lens.# pName_,
        accountGrouping = pAccountGrouping_,
        computationPreference = pComputationPreference_
      }

-- | A map that contains tag keys and tag values that are attached to a
-- billing group. This feature isn\'t available during the beta.
createBillingGroup_tags :: Lens.Lens' CreateBillingGroup (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createBillingGroup_tags = Lens.lens (\CreateBillingGroup' {tags} -> tags) (\s@CreateBillingGroup' {} a -> s {tags = a} :: CreateBillingGroup) Prelude.. Lens.mapping Lens.coerced

-- | The token that is needed to support idempotency. Idempotency isn\'t
-- currently supported, but will be implemented in a future update.
createBillingGroup_clientToken :: Lens.Lens' CreateBillingGroup (Prelude.Maybe Prelude.Text)
createBillingGroup_clientToken = Lens.lens (\CreateBillingGroup' {clientToken} -> clientToken) (\s@CreateBillingGroup' {} a -> s {clientToken = a} :: CreateBillingGroup)

-- | The description of the billing group.
createBillingGroup_description :: Lens.Lens' CreateBillingGroup (Prelude.Maybe Prelude.Text)
createBillingGroup_description = Lens.lens (\CreateBillingGroup' {description} -> description) (\s@CreateBillingGroup' {} a -> s {description = a} :: CreateBillingGroup) Prelude.. Lens.mapping Core._Sensitive

-- | The account ID that serves as the main account in a billing group.
createBillingGroup_primaryAccountId :: Lens.Lens' CreateBillingGroup (Prelude.Maybe Prelude.Text)
createBillingGroup_primaryAccountId = Lens.lens (\CreateBillingGroup' {primaryAccountId} -> primaryAccountId) (\s@CreateBillingGroup' {} a -> s {primaryAccountId = a} :: CreateBillingGroup)

-- | The billing group name. The names must be unique.
createBillingGroup_name :: Lens.Lens' CreateBillingGroup Prelude.Text
createBillingGroup_name = Lens.lens (\CreateBillingGroup' {name} -> name) (\s@CreateBillingGroup' {} a -> s {name = a} :: CreateBillingGroup) Prelude.. Core._Sensitive

-- | The set of accounts that will be under the billing group. The set of
-- accounts resemble the linked accounts in a consolidated family.
createBillingGroup_accountGrouping :: Lens.Lens' CreateBillingGroup AccountGrouping
createBillingGroup_accountGrouping = Lens.lens (\CreateBillingGroup' {accountGrouping} -> accountGrouping) (\s@CreateBillingGroup' {} a -> s {accountGrouping = a} :: CreateBillingGroup)

-- | The preferences and settings that will be used to compute the Amazon Web
-- Services charges for a billing group.
createBillingGroup_computationPreference :: Lens.Lens' CreateBillingGroup ComputationPreference
createBillingGroup_computationPreference = Lens.lens (\CreateBillingGroup' {computationPreference} -> computationPreference) (\s@CreateBillingGroup' {} a -> s {computationPreference = a} :: CreateBillingGroup)

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
            Prelude.<$> (x Core..?> "Arn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateBillingGroup where
  hashWithSalt _salt CreateBillingGroup' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` primaryAccountId
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` accountGrouping
      `Prelude.hashWithSalt` computationPreference

instance Prelude.NFData CreateBillingGroup where
  rnf CreateBillingGroup' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf primaryAccountId
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf accountGrouping
      `Prelude.seq` Prelude.rnf computationPreference

instance Core.ToHeaders CreateBillingGroup where
  toHeaders CreateBillingGroup' {..} =
    Prelude.mconcat
      [ "X-Amzn-Client-Token" Core.=# clientToken,
        "Content-Type"
          Core.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance Core.ToJSON CreateBillingGroup where
  toJSON CreateBillingGroup' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Tags" Core..=) Prelude.<$> tags,
            ("Description" Core..=) Prelude.<$> description,
            ("PrimaryAccountId" Core..=)
              Prelude.<$> primaryAccountId,
            Prelude.Just ("Name" Core..= name),
            Prelude.Just
              ("AccountGrouping" Core..= accountGrouping),
            Prelude.Just
              ( "ComputationPreference"
                  Core..= computationPreference
              )
          ]
      )

instance Core.ToPath CreateBillingGroup where
  toPath = Prelude.const "/create-billing-group"

instance Core.ToQuery CreateBillingGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateBillingGroupResponse' smart constructor.
data CreateBillingGroupResponse = CreateBillingGroupResponse'
  { -- | The Amazon Resource Name (ARN) of the created billing group.
    arn :: Prelude.Maybe Prelude.Text,
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
-- 'arn', 'createBillingGroupResponse_arn' - The Amazon Resource Name (ARN) of the created billing group.
--
-- 'httpStatus', 'createBillingGroupResponse_httpStatus' - The response's http status code.
newCreateBillingGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateBillingGroupResponse
newCreateBillingGroupResponse pHttpStatus_ =
  CreateBillingGroupResponse'
    { arn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the created billing group.
createBillingGroupResponse_arn :: Lens.Lens' CreateBillingGroupResponse (Prelude.Maybe Prelude.Text)
createBillingGroupResponse_arn = Lens.lens (\CreateBillingGroupResponse' {arn} -> arn) (\s@CreateBillingGroupResponse' {} a -> s {arn = a} :: CreateBillingGroupResponse)

-- | The response's http status code.
createBillingGroupResponse_httpStatus :: Lens.Lens' CreateBillingGroupResponse Prelude.Int
createBillingGroupResponse_httpStatus = Lens.lens (\CreateBillingGroupResponse' {httpStatus} -> httpStatus) (\s@CreateBillingGroupResponse' {} a -> s {httpStatus = a} :: CreateBillingGroupResponse)

instance Prelude.NFData CreateBillingGroupResponse where
  rnf CreateBillingGroupResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf httpStatus
