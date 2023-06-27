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
-- Module      : Amazonka.KendraRanking.CreateRescoreExecutionPlan
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a rescore execution plan. A rescore execution plan is an Amazon
-- Kendra Intelligent Ranking resource used for provisioning the @Rescore@
-- API. You set the number of capacity units that you require for Amazon
-- Kendra Intelligent Ranking to rescore or re-rank a search service\'s
-- results.
--
-- For an example of using the @CreateRescoreExecutionPlan@ API, including
-- using the Python and Java SDKs, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/search-service-rerank.html Semantically ranking a search service\'s results>.
module Amazonka.KendraRanking.CreateRescoreExecutionPlan
  ( -- * Creating a Request
    CreateRescoreExecutionPlan (..),
    newCreateRescoreExecutionPlan,

    -- * Request Lenses
    createRescoreExecutionPlan_capacityUnits,
    createRescoreExecutionPlan_clientToken,
    createRescoreExecutionPlan_description,
    createRescoreExecutionPlan_tags,
    createRescoreExecutionPlan_name,

    -- * Destructuring the Response
    CreateRescoreExecutionPlanResponse (..),
    newCreateRescoreExecutionPlanResponse,

    -- * Response Lenses
    createRescoreExecutionPlanResponse_httpStatus,
    createRescoreExecutionPlanResponse_id,
    createRescoreExecutionPlanResponse_arn,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KendraRanking.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateRescoreExecutionPlan' smart constructor.
data CreateRescoreExecutionPlan = CreateRescoreExecutionPlan'
  { -- | You can set additional capacity units to meet the needs of your rescore
    -- execution plan. You are given a single capacity unit by default. If you
    -- want to use the default capacity, you don\'t set additional capacity
    -- units. For more information on the default capacity and additional
    -- capacity units, see
    -- <https://docs.aws.amazon.com/kendra/latest/dg/adjusting-capacity.html Adjusting capacity>.
    capacityUnits :: Prelude.Maybe CapacityUnitsConfiguration,
    -- | A token that you provide to identify the request to create a rescore
    -- execution plan. Multiple calls to the
    -- @CreateRescoreExecutionPlanRequest@ API with the same client token will
    -- create only one rescore execution plan.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | A description for the rescore execution plan.
    description :: Prelude.Maybe Prelude.Text,
    -- | A list of key-value pairs that identify or categorize your rescore
    -- execution plan. You can also use tags to help control access to the
    -- rescore execution plan. Tag keys and values can consist of Unicode
    -- letters, digits, white space, and any of the following symbols: _ . : \/
    -- = + - \@.
    tags :: Prelude.Maybe [Tag],
    -- | A name for the rescore execution plan.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateRescoreExecutionPlan' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'capacityUnits', 'createRescoreExecutionPlan_capacityUnits' - You can set additional capacity units to meet the needs of your rescore
-- execution plan. You are given a single capacity unit by default. If you
-- want to use the default capacity, you don\'t set additional capacity
-- units. For more information on the default capacity and additional
-- capacity units, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/adjusting-capacity.html Adjusting capacity>.
--
-- 'clientToken', 'createRescoreExecutionPlan_clientToken' - A token that you provide to identify the request to create a rescore
-- execution plan. Multiple calls to the
-- @CreateRescoreExecutionPlanRequest@ API with the same client token will
-- create only one rescore execution plan.
--
-- 'description', 'createRescoreExecutionPlan_description' - A description for the rescore execution plan.
--
-- 'tags', 'createRescoreExecutionPlan_tags' - A list of key-value pairs that identify or categorize your rescore
-- execution plan. You can also use tags to help control access to the
-- rescore execution plan. Tag keys and values can consist of Unicode
-- letters, digits, white space, and any of the following symbols: _ . : \/
-- = + - \@.
--
-- 'name', 'createRescoreExecutionPlan_name' - A name for the rescore execution plan.
newCreateRescoreExecutionPlan ::
  -- | 'name'
  Prelude.Text ->
  CreateRescoreExecutionPlan
newCreateRescoreExecutionPlan pName_ =
  CreateRescoreExecutionPlan'
    { capacityUnits =
        Prelude.Nothing,
      clientToken = Prelude.Nothing,
      description = Prelude.Nothing,
      tags = Prelude.Nothing,
      name = pName_
    }

-- | You can set additional capacity units to meet the needs of your rescore
-- execution plan. You are given a single capacity unit by default. If you
-- want to use the default capacity, you don\'t set additional capacity
-- units. For more information on the default capacity and additional
-- capacity units, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/adjusting-capacity.html Adjusting capacity>.
createRescoreExecutionPlan_capacityUnits :: Lens.Lens' CreateRescoreExecutionPlan (Prelude.Maybe CapacityUnitsConfiguration)
createRescoreExecutionPlan_capacityUnits = Lens.lens (\CreateRescoreExecutionPlan' {capacityUnits} -> capacityUnits) (\s@CreateRescoreExecutionPlan' {} a -> s {capacityUnits = a} :: CreateRescoreExecutionPlan)

-- | A token that you provide to identify the request to create a rescore
-- execution plan. Multiple calls to the
-- @CreateRescoreExecutionPlanRequest@ API with the same client token will
-- create only one rescore execution plan.
createRescoreExecutionPlan_clientToken :: Lens.Lens' CreateRescoreExecutionPlan (Prelude.Maybe Prelude.Text)
createRescoreExecutionPlan_clientToken = Lens.lens (\CreateRescoreExecutionPlan' {clientToken} -> clientToken) (\s@CreateRescoreExecutionPlan' {} a -> s {clientToken = a} :: CreateRescoreExecutionPlan)

-- | A description for the rescore execution plan.
createRescoreExecutionPlan_description :: Lens.Lens' CreateRescoreExecutionPlan (Prelude.Maybe Prelude.Text)
createRescoreExecutionPlan_description = Lens.lens (\CreateRescoreExecutionPlan' {description} -> description) (\s@CreateRescoreExecutionPlan' {} a -> s {description = a} :: CreateRescoreExecutionPlan)

-- | A list of key-value pairs that identify or categorize your rescore
-- execution plan. You can also use tags to help control access to the
-- rescore execution plan. Tag keys and values can consist of Unicode
-- letters, digits, white space, and any of the following symbols: _ . : \/
-- = + - \@.
createRescoreExecutionPlan_tags :: Lens.Lens' CreateRescoreExecutionPlan (Prelude.Maybe [Tag])
createRescoreExecutionPlan_tags = Lens.lens (\CreateRescoreExecutionPlan' {tags} -> tags) (\s@CreateRescoreExecutionPlan' {} a -> s {tags = a} :: CreateRescoreExecutionPlan) Prelude.. Lens.mapping Lens.coerced

-- | A name for the rescore execution plan.
createRescoreExecutionPlan_name :: Lens.Lens' CreateRescoreExecutionPlan Prelude.Text
createRescoreExecutionPlan_name = Lens.lens (\CreateRescoreExecutionPlan' {name} -> name) (\s@CreateRescoreExecutionPlan' {} a -> s {name = a} :: CreateRescoreExecutionPlan)

instance Core.AWSRequest CreateRescoreExecutionPlan where
  type
    AWSResponse CreateRescoreExecutionPlan =
      CreateRescoreExecutionPlanResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateRescoreExecutionPlanResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "Id")
            Prelude.<*> (x Data..:> "Arn")
      )

instance Prelude.Hashable CreateRescoreExecutionPlan where
  hashWithSalt _salt CreateRescoreExecutionPlan' {..} =
    _salt
      `Prelude.hashWithSalt` capacityUnits
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` name

instance Prelude.NFData CreateRescoreExecutionPlan where
  rnf CreateRescoreExecutionPlan' {..} =
    Prelude.rnf capacityUnits
      `Prelude.seq` Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf name

instance Data.ToHeaders CreateRescoreExecutionPlan where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSKendraRerankingFrontendService.CreateRescoreExecutionPlan" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateRescoreExecutionPlan where
  toJSON CreateRescoreExecutionPlan' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CapacityUnits" Data..=) Prelude.<$> capacityUnits,
            ("ClientToken" Data..=) Prelude.<$> clientToken,
            ("Description" Data..=) Prelude.<$> description,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("Name" Data..= name)
          ]
      )

instance Data.ToPath CreateRescoreExecutionPlan where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateRescoreExecutionPlan where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateRescoreExecutionPlanResponse' smart constructor.
data CreateRescoreExecutionPlanResponse = CreateRescoreExecutionPlanResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The identifier of the rescore execution plan.
    id :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the rescore execution plan.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateRescoreExecutionPlanResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createRescoreExecutionPlanResponse_httpStatus' - The response's http status code.
--
-- 'id', 'createRescoreExecutionPlanResponse_id' - The identifier of the rescore execution plan.
--
-- 'arn', 'createRescoreExecutionPlanResponse_arn' - The Amazon Resource Name (ARN) of the rescore execution plan.
newCreateRescoreExecutionPlanResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'id'
  Prelude.Text ->
  -- | 'arn'
  Prelude.Text ->
  CreateRescoreExecutionPlanResponse
newCreateRescoreExecutionPlanResponse
  pHttpStatus_
  pId_
  pArn_ =
    CreateRescoreExecutionPlanResponse'
      { httpStatus =
          pHttpStatus_,
        id = pId_,
        arn = pArn_
      }

-- | The response's http status code.
createRescoreExecutionPlanResponse_httpStatus :: Lens.Lens' CreateRescoreExecutionPlanResponse Prelude.Int
createRescoreExecutionPlanResponse_httpStatus = Lens.lens (\CreateRescoreExecutionPlanResponse' {httpStatus} -> httpStatus) (\s@CreateRescoreExecutionPlanResponse' {} a -> s {httpStatus = a} :: CreateRescoreExecutionPlanResponse)

-- | The identifier of the rescore execution plan.
createRescoreExecutionPlanResponse_id :: Lens.Lens' CreateRescoreExecutionPlanResponse Prelude.Text
createRescoreExecutionPlanResponse_id = Lens.lens (\CreateRescoreExecutionPlanResponse' {id} -> id) (\s@CreateRescoreExecutionPlanResponse' {} a -> s {id = a} :: CreateRescoreExecutionPlanResponse)

-- | The Amazon Resource Name (ARN) of the rescore execution plan.
createRescoreExecutionPlanResponse_arn :: Lens.Lens' CreateRescoreExecutionPlanResponse Prelude.Text
createRescoreExecutionPlanResponse_arn = Lens.lens (\CreateRescoreExecutionPlanResponse' {arn} -> arn) (\s@CreateRescoreExecutionPlanResponse' {} a -> s {arn = a} :: CreateRescoreExecutionPlanResponse)

instance
  Prelude.NFData
    CreateRescoreExecutionPlanResponse
  where
  rnf CreateRescoreExecutionPlanResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf arn
