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
-- Module      : Amazonka.KendraRanking.DescribeRescoreExecutionPlan
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a rescore execution plan. A rescore execution
-- plan is an Amazon Kendra Intelligent Ranking resource used for
-- provisioning the @Rescore@ API.
module Amazonka.KendraRanking.DescribeRescoreExecutionPlan
  ( -- * Creating a Request
    DescribeRescoreExecutionPlan (..),
    newDescribeRescoreExecutionPlan,

    -- * Request Lenses
    describeRescoreExecutionPlan_id,

    -- * Destructuring the Response
    DescribeRescoreExecutionPlanResponse (..),
    newDescribeRescoreExecutionPlanResponse,

    -- * Response Lenses
    describeRescoreExecutionPlanResponse_arn,
    describeRescoreExecutionPlanResponse_capacityUnits,
    describeRescoreExecutionPlanResponse_createdAt,
    describeRescoreExecutionPlanResponse_description,
    describeRescoreExecutionPlanResponse_errorMessage,
    describeRescoreExecutionPlanResponse_id,
    describeRescoreExecutionPlanResponse_name,
    describeRescoreExecutionPlanResponse_status,
    describeRescoreExecutionPlanResponse_updatedAt,
    describeRescoreExecutionPlanResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KendraRanking.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeRescoreExecutionPlan' smart constructor.
data DescribeRescoreExecutionPlan = DescribeRescoreExecutionPlan'
  { -- | The identifier of the rescore execution plan that you want to get
    -- information on.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeRescoreExecutionPlan' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'describeRescoreExecutionPlan_id' - The identifier of the rescore execution plan that you want to get
-- information on.
newDescribeRescoreExecutionPlan ::
  -- | 'id'
  Prelude.Text ->
  DescribeRescoreExecutionPlan
newDescribeRescoreExecutionPlan pId_ =
  DescribeRescoreExecutionPlan' {id = pId_}

-- | The identifier of the rescore execution plan that you want to get
-- information on.
describeRescoreExecutionPlan_id :: Lens.Lens' DescribeRescoreExecutionPlan Prelude.Text
describeRescoreExecutionPlan_id = Lens.lens (\DescribeRescoreExecutionPlan' {id} -> id) (\s@DescribeRescoreExecutionPlan' {} a -> s {id = a} :: DescribeRescoreExecutionPlan)

instance Core.AWSRequest DescribeRescoreExecutionPlan where
  type
    AWSResponse DescribeRescoreExecutionPlan =
      DescribeRescoreExecutionPlanResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeRescoreExecutionPlanResponse'
            Prelude.<$> (x Data..?> "Arn")
            Prelude.<*> (x Data..?> "CapacityUnits")
            Prelude.<*> (x Data..?> "CreatedAt")
            Prelude.<*> (x Data..?> "Description")
            Prelude.<*> (x Data..?> "ErrorMessage")
            Prelude.<*> (x Data..?> "Id")
            Prelude.<*> (x Data..?> "Name")
            Prelude.<*> (x Data..?> "Status")
            Prelude.<*> (x Data..?> "UpdatedAt")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeRescoreExecutionPlan
  where
  hashWithSalt _salt DescribeRescoreExecutionPlan' {..} =
    _salt `Prelude.hashWithSalt` id

instance Prelude.NFData DescribeRescoreExecutionPlan where
  rnf DescribeRescoreExecutionPlan' {..} =
    Prelude.rnf id

instance Data.ToHeaders DescribeRescoreExecutionPlan where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSKendraRerankingFrontendService.DescribeRescoreExecutionPlan" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeRescoreExecutionPlan where
  toJSON DescribeRescoreExecutionPlan' {..} =
    Data.object
      (Prelude.catMaybes [Prelude.Just ("Id" Data..= id)])

instance Data.ToPath DescribeRescoreExecutionPlan where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeRescoreExecutionPlan where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeRescoreExecutionPlanResponse' smart constructor.
data DescribeRescoreExecutionPlanResponse = DescribeRescoreExecutionPlanResponse'
  { -- | The Amazon Resource Name (ARN) of the rescore execution plan.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The capacity units set for the rescore execution plan. A capacity of
    -- zero indicates that the rescore execution plan is using the default
    -- capacity. For more information on the default capacity and additional
    -- capacity units, see
    -- <https://docs.aws.amazon.com/kendra/latest/dg/adjusting-capacity.html Adjusting capacity>.
    capacityUnits :: Prelude.Maybe CapacityUnitsConfiguration,
    -- | The Unix timestamp of when the rescore execution plan was created.
    createdAt :: Prelude.Maybe Data.POSIX,
    -- | The description for the rescore execution plan.
    description :: Prelude.Maybe Prelude.Text,
    -- | When the @Status@ field value is @FAILED@, the @ErrorMessage@ field
    -- contains a message that explains why.
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the rescore execution plan.
    id :: Prelude.Maybe Prelude.Text,
    -- | The name for the rescore execution plan.
    name :: Prelude.Maybe Prelude.Text,
    -- | The current status of the rescore execution plan. When the value is
    -- @ACTIVE@, the rescore execution plan is ready for use. If the @Status@
    -- field value is @FAILED@, the @ErrorMessage@ field contains a message
    -- that explains why.
    status :: Prelude.Maybe RescoreExecutionPlanStatus,
    -- | The Unix timestamp of when the rescore execution plan was last updated.
    updatedAt :: Prelude.Maybe Data.POSIX,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeRescoreExecutionPlanResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'describeRescoreExecutionPlanResponse_arn' - The Amazon Resource Name (ARN) of the rescore execution plan.
--
-- 'capacityUnits', 'describeRescoreExecutionPlanResponse_capacityUnits' - The capacity units set for the rescore execution plan. A capacity of
-- zero indicates that the rescore execution plan is using the default
-- capacity. For more information on the default capacity and additional
-- capacity units, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/adjusting-capacity.html Adjusting capacity>.
--
-- 'createdAt', 'describeRescoreExecutionPlanResponse_createdAt' - The Unix timestamp of when the rescore execution plan was created.
--
-- 'description', 'describeRescoreExecutionPlanResponse_description' - The description for the rescore execution plan.
--
-- 'errorMessage', 'describeRescoreExecutionPlanResponse_errorMessage' - When the @Status@ field value is @FAILED@, the @ErrorMessage@ field
-- contains a message that explains why.
--
-- 'id', 'describeRescoreExecutionPlanResponse_id' - The identifier of the rescore execution plan.
--
-- 'name', 'describeRescoreExecutionPlanResponse_name' - The name for the rescore execution plan.
--
-- 'status', 'describeRescoreExecutionPlanResponse_status' - The current status of the rescore execution plan. When the value is
-- @ACTIVE@, the rescore execution plan is ready for use. If the @Status@
-- field value is @FAILED@, the @ErrorMessage@ field contains a message
-- that explains why.
--
-- 'updatedAt', 'describeRescoreExecutionPlanResponse_updatedAt' - The Unix timestamp of when the rescore execution plan was last updated.
--
-- 'httpStatus', 'describeRescoreExecutionPlanResponse_httpStatus' - The response's http status code.
newDescribeRescoreExecutionPlanResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeRescoreExecutionPlanResponse
newDescribeRescoreExecutionPlanResponse pHttpStatus_ =
  DescribeRescoreExecutionPlanResponse'
    { arn =
        Prelude.Nothing,
      capacityUnits = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      description = Prelude.Nothing,
      errorMessage = Prelude.Nothing,
      id = Prelude.Nothing,
      name = Prelude.Nothing,
      status = Prelude.Nothing,
      updatedAt = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the rescore execution plan.
describeRescoreExecutionPlanResponse_arn :: Lens.Lens' DescribeRescoreExecutionPlanResponse (Prelude.Maybe Prelude.Text)
describeRescoreExecutionPlanResponse_arn = Lens.lens (\DescribeRescoreExecutionPlanResponse' {arn} -> arn) (\s@DescribeRescoreExecutionPlanResponse' {} a -> s {arn = a} :: DescribeRescoreExecutionPlanResponse)

-- | The capacity units set for the rescore execution plan. A capacity of
-- zero indicates that the rescore execution plan is using the default
-- capacity. For more information on the default capacity and additional
-- capacity units, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/adjusting-capacity.html Adjusting capacity>.
describeRescoreExecutionPlanResponse_capacityUnits :: Lens.Lens' DescribeRescoreExecutionPlanResponse (Prelude.Maybe CapacityUnitsConfiguration)
describeRescoreExecutionPlanResponse_capacityUnits = Lens.lens (\DescribeRescoreExecutionPlanResponse' {capacityUnits} -> capacityUnits) (\s@DescribeRescoreExecutionPlanResponse' {} a -> s {capacityUnits = a} :: DescribeRescoreExecutionPlanResponse)

-- | The Unix timestamp of when the rescore execution plan was created.
describeRescoreExecutionPlanResponse_createdAt :: Lens.Lens' DescribeRescoreExecutionPlanResponse (Prelude.Maybe Prelude.UTCTime)
describeRescoreExecutionPlanResponse_createdAt = Lens.lens (\DescribeRescoreExecutionPlanResponse' {createdAt} -> createdAt) (\s@DescribeRescoreExecutionPlanResponse' {} a -> s {createdAt = a} :: DescribeRescoreExecutionPlanResponse) Prelude.. Lens.mapping Data._Time

-- | The description for the rescore execution plan.
describeRescoreExecutionPlanResponse_description :: Lens.Lens' DescribeRescoreExecutionPlanResponse (Prelude.Maybe Prelude.Text)
describeRescoreExecutionPlanResponse_description = Lens.lens (\DescribeRescoreExecutionPlanResponse' {description} -> description) (\s@DescribeRescoreExecutionPlanResponse' {} a -> s {description = a} :: DescribeRescoreExecutionPlanResponse)

-- | When the @Status@ field value is @FAILED@, the @ErrorMessage@ field
-- contains a message that explains why.
describeRescoreExecutionPlanResponse_errorMessage :: Lens.Lens' DescribeRescoreExecutionPlanResponse (Prelude.Maybe Prelude.Text)
describeRescoreExecutionPlanResponse_errorMessage = Lens.lens (\DescribeRescoreExecutionPlanResponse' {errorMessage} -> errorMessage) (\s@DescribeRescoreExecutionPlanResponse' {} a -> s {errorMessage = a} :: DescribeRescoreExecutionPlanResponse)

-- | The identifier of the rescore execution plan.
describeRescoreExecutionPlanResponse_id :: Lens.Lens' DescribeRescoreExecutionPlanResponse (Prelude.Maybe Prelude.Text)
describeRescoreExecutionPlanResponse_id = Lens.lens (\DescribeRescoreExecutionPlanResponse' {id} -> id) (\s@DescribeRescoreExecutionPlanResponse' {} a -> s {id = a} :: DescribeRescoreExecutionPlanResponse)

-- | The name for the rescore execution plan.
describeRescoreExecutionPlanResponse_name :: Lens.Lens' DescribeRescoreExecutionPlanResponse (Prelude.Maybe Prelude.Text)
describeRescoreExecutionPlanResponse_name = Lens.lens (\DescribeRescoreExecutionPlanResponse' {name} -> name) (\s@DescribeRescoreExecutionPlanResponse' {} a -> s {name = a} :: DescribeRescoreExecutionPlanResponse)

-- | The current status of the rescore execution plan. When the value is
-- @ACTIVE@, the rescore execution plan is ready for use. If the @Status@
-- field value is @FAILED@, the @ErrorMessage@ field contains a message
-- that explains why.
describeRescoreExecutionPlanResponse_status :: Lens.Lens' DescribeRescoreExecutionPlanResponse (Prelude.Maybe RescoreExecutionPlanStatus)
describeRescoreExecutionPlanResponse_status = Lens.lens (\DescribeRescoreExecutionPlanResponse' {status} -> status) (\s@DescribeRescoreExecutionPlanResponse' {} a -> s {status = a} :: DescribeRescoreExecutionPlanResponse)

-- | The Unix timestamp of when the rescore execution plan was last updated.
describeRescoreExecutionPlanResponse_updatedAt :: Lens.Lens' DescribeRescoreExecutionPlanResponse (Prelude.Maybe Prelude.UTCTime)
describeRescoreExecutionPlanResponse_updatedAt = Lens.lens (\DescribeRescoreExecutionPlanResponse' {updatedAt} -> updatedAt) (\s@DescribeRescoreExecutionPlanResponse' {} a -> s {updatedAt = a} :: DescribeRescoreExecutionPlanResponse) Prelude.. Lens.mapping Data._Time

-- | The response's http status code.
describeRescoreExecutionPlanResponse_httpStatus :: Lens.Lens' DescribeRescoreExecutionPlanResponse Prelude.Int
describeRescoreExecutionPlanResponse_httpStatus = Lens.lens (\DescribeRescoreExecutionPlanResponse' {httpStatus} -> httpStatus) (\s@DescribeRescoreExecutionPlanResponse' {} a -> s {httpStatus = a} :: DescribeRescoreExecutionPlanResponse)

instance
  Prelude.NFData
    DescribeRescoreExecutionPlanResponse
  where
  rnf DescribeRescoreExecutionPlanResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf capacityUnits
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf errorMessage
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf updatedAt
      `Prelude.seq` Prelude.rnf httpStatus
