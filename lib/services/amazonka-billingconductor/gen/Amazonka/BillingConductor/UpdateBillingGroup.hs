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
-- Module      : Amazonka.BillingConductor.UpdateBillingGroup
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This updates an existing billing group.
module Amazonka.BillingConductor.UpdateBillingGroup
  ( -- * Creating a Request
    UpdateBillingGroup (..),
    newUpdateBillingGroup,

    -- * Request Lenses
    updateBillingGroup_computationPreference,
    updateBillingGroup_description,
    updateBillingGroup_name,
    updateBillingGroup_status,
    updateBillingGroup_arn,

    -- * Destructuring the Response
    UpdateBillingGroupResponse (..),
    newUpdateBillingGroupResponse,

    -- * Response Lenses
    updateBillingGroupResponse_arn,
    updateBillingGroupResponse_description,
    updateBillingGroupResponse_lastModifiedTime,
    updateBillingGroupResponse_name,
    updateBillingGroupResponse_pricingPlanArn,
    updateBillingGroupResponse_primaryAccountId,
    updateBillingGroupResponse_size,
    updateBillingGroupResponse_status,
    updateBillingGroupResponse_statusReason,
    updateBillingGroupResponse_httpStatus,
  )
where

import Amazonka.BillingConductor.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateBillingGroup' smart constructor.
data UpdateBillingGroup = UpdateBillingGroup'
  { -- | The preferences and settings that will be used to compute the Amazon Web
    -- Services charges for a billing group.
    computationPreference :: Prelude.Maybe ComputationPreference,
    -- | A description of the billing group.
    description :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The name of the billing group. The names must be unique to each billing
    -- group.
    name :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The status of the billing group. Only one of the valid values can be
    -- used.
    status :: Prelude.Maybe BillingGroupStatus,
    -- | The Amazon Resource Name (ARN) of the billing group being updated.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateBillingGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'computationPreference', 'updateBillingGroup_computationPreference' - The preferences and settings that will be used to compute the Amazon Web
-- Services charges for a billing group.
--
-- 'description', 'updateBillingGroup_description' - A description of the billing group.
--
-- 'name', 'updateBillingGroup_name' - The name of the billing group. The names must be unique to each billing
-- group.
--
-- 'status', 'updateBillingGroup_status' - The status of the billing group. Only one of the valid values can be
-- used.
--
-- 'arn', 'updateBillingGroup_arn' - The Amazon Resource Name (ARN) of the billing group being updated.
newUpdateBillingGroup ::
  -- | 'arn'
  Prelude.Text ->
  UpdateBillingGroup
newUpdateBillingGroup pArn_ =
  UpdateBillingGroup'
    { computationPreference =
        Prelude.Nothing,
      description = Prelude.Nothing,
      name = Prelude.Nothing,
      status = Prelude.Nothing,
      arn = pArn_
    }

-- | The preferences and settings that will be used to compute the Amazon Web
-- Services charges for a billing group.
updateBillingGroup_computationPreference :: Lens.Lens' UpdateBillingGroup (Prelude.Maybe ComputationPreference)
updateBillingGroup_computationPreference = Lens.lens (\UpdateBillingGroup' {computationPreference} -> computationPreference) (\s@UpdateBillingGroup' {} a -> s {computationPreference = a} :: UpdateBillingGroup)

-- | A description of the billing group.
updateBillingGroup_description :: Lens.Lens' UpdateBillingGroup (Prelude.Maybe Prelude.Text)
updateBillingGroup_description = Lens.lens (\UpdateBillingGroup' {description} -> description) (\s@UpdateBillingGroup' {} a -> s {description = a} :: UpdateBillingGroup) Prelude.. Lens.mapping Data._Sensitive

-- | The name of the billing group. The names must be unique to each billing
-- group.
updateBillingGroup_name :: Lens.Lens' UpdateBillingGroup (Prelude.Maybe Prelude.Text)
updateBillingGroup_name = Lens.lens (\UpdateBillingGroup' {name} -> name) (\s@UpdateBillingGroup' {} a -> s {name = a} :: UpdateBillingGroup) Prelude.. Lens.mapping Data._Sensitive

-- | The status of the billing group. Only one of the valid values can be
-- used.
updateBillingGroup_status :: Lens.Lens' UpdateBillingGroup (Prelude.Maybe BillingGroupStatus)
updateBillingGroup_status = Lens.lens (\UpdateBillingGroup' {status} -> status) (\s@UpdateBillingGroup' {} a -> s {status = a} :: UpdateBillingGroup)

-- | The Amazon Resource Name (ARN) of the billing group being updated.
updateBillingGroup_arn :: Lens.Lens' UpdateBillingGroup Prelude.Text
updateBillingGroup_arn = Lens.lens (\UpdateBillingGroup' {arn} -> arn) (\s@UpdateBillingGroup' {} a -> s {arn = a} :: UpdateBillingGroup)

instance Core.AWSRequest UpdateBillingGroup where
  type
    AWSResponse UpdateBillingGroup =
      UpdateBillingGroupResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateBillingGroupResponse'
            Prelude.<$> (x Data..?> "Arn")
            Prelude.<*> (x Data..?> "Description")
            Prelude.<*> (x Data..?> "LastModifiedTime")
            Prelude.<*> (x Data..?> "Name")
            Prelude.<*> (x Data..?> "PricingPlanArn")
            Prelude.<*> (x Data..?> "PrimaryAccountId")
            Prelude.<*> (x Data..?> "Size")
            Prelude.<*> (x Data..?> "Status")
            Prelude.<*> (x Data..?> "StatusReason")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateBillingGroup where
  hashWithSalt _salt UpdateBillingGroup' {..} =
    _salt `Prelude.hashWithSalt` computationPreference
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` arn

instance Prelude.NFData UpdateBillingGroup where
  rnf UpdateBillingGroup' {..} =
    Prelude.rnf computationPreference
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf arn

instance Data.ToHeaders UpdateBillingGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateBillingGroup where
  toJSON UpdateBillingGroup' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ComputationPreference" Data..=)
              Prelude.<$> computationPreference,
            ("Description" Data..=) Prelude.<$> description,
            ("Name" Data..=) Prelude.<$> name,
            ("Status" Data..=) Prelude.<$> status,
            Prelude.Just ("Arn" Data..= arn)
          ]
      )

instance Data.ToPath UpdateBillingGroup where
  toPath = Prelude.const "/update-billing-group"

instance Data.ToQuery UpdateBillingGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateBillingGroupResponse' smart constructor.
data UpdateBillingGroupResponse = UpdateBillingGroupResponse'
  { -- | The Amazon Resource Name (ARN) of the billing group that was updated.
    arn :: Prelude.Maybe Prelude.Text,
    -- | A description of the billing group.
    description :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The most recent time when the billing group was modified.
    lastModifiedTime :: Prelude.Maybe Prelude.Integer,
    -- | The name of the billing group. The names must be unique to each billing
    -- group.
    name :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The Amazon Resource Name (ARN) of the pricing plan to compute Amazon Web
    -- Services charges for the billing group.
    pricingPlanArn :: Prelude.Maybe Prelude.Text,
    -- | The account ID that serves as the main account in a billing group.
    primaryAccountId :: Prelude.Maybe Prelude.Text,
    -- | The number of accounts in the particular billing group.
    size :: Prelude.Maybe Prelude.Natural,
    -- | The status of the billing group. Only one of the valid values can be
    -- used.
    status :: Prelude.Maybe BillingGroupStatus,
    -- | The reason why the billing group is in its current status.
    statusReason :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateBillingGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'updateBillingGroupResponse_arn' - The Amazon Resource Name (ARN) of the billing group that was updated.
--
-- 'description', 'updateBillingGroupResponse_description' - A description of the billing group.
--
-- 'lastModifiedTime', 'updateBillingGroupResponse_lastModifiedTime' - The most recent time when the billing group was modified.
--
-- 'name', 'updateBillingGroupResponse_name' - The name of the billing group. The names must be unique to each billing
-- group.
--
-- 'pricingPlanArn', 'updateBillingGroupResponse_pricingPlanArn' - The Amazon Resource Name (ARN) of the pricing plan to compute Amazon Web
-- Services charges for the billing group.
--
-- 'primaryAccountId', 'updateBillingGroupResponse_primaryAccountId' - The account ID that serves as the main account in a billing group.
--
-- 'size', 'updateBillingGroupResponse_size' - The number of accounts in the particular billing group.
--
-- 'status', 'updateBillingGroupResponse_status' - The status of the billing group. Only one of the valid values can be
-- used.
--
-- 'statusReason', 'updateBillingGroupResponse_statusReason' - The reason why the billing group is in its current status.
--
-- 'httpStatus', 'updateBillingGroupResponse_httpStatus' - The response's http status code.
newUpdateBillingGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateBillingGroupResponse
newUpdateBillingGroupResponse pHttpStatus_ =
  UpdateBillingGroupResponse'
    { arn = Prelude.Nothing,
      description = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      name = Prelude.Nothing,
      pricingPlanArn = Prelude.Nothing,
      primaryAccountId = Prelude.Nothing,
      size = Prelude.Nothing,
      status = Prelude.Nothing,
      statusReason = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the billing group that was updated.
updateBillingGroupResponse_arn :: Lens.Lens' UpdateBillingGroupResponse (Prelude.Maybe Prelude.Text)
updateBillingGroupResponse_arn = Lens.lens (\UpdateBillingGroupResponse' {arn} -> arn) (\s@UpdateBillingGroupResponse' {} a -> s {arn = a} :: UpdateBillingGroupResponse)

-- | A description of the billing group.
updateBillingGroupResponse_description :: Lens.Lens' UpdateBillingGroupResponse (Prelude.Maybe Prelude.Text)
updateBillingGroupResponse_description = Lens.lens (\UpdateBillingGroupResponse' {description} -> description) (\s@UpdateBillingGroupResponse' {} a -> s {description = a} :: UpdateBillingGroupResponse) Prelude.. Lens.mapping Data._Sensitive

-- | The most recent time when the billing group was modified.
updateBillingGroupResponse_lastModifiedTime :: Lens.Lens' UpdateBillingGroupResponse (Prelude.Maybe Prelude.Integer)
updateBillingGroupResponse_lastModifiedTime = Lens.lens (\UpdateBillingGroupResponse' {lastModifiedTime} -> lastModifiedTime) (\s@UpdateBillingGroupResponse' {} a -> s {lastModifiedTime = a} :: UpdateBillingGroupResponse)

-- | The name of the billing group. The names must be unique to each billing
-- group.
updateBillingGroupResponse_name :: Lens.Lens' UpdateBillingGroupResponse (Prelude.Maybe Prelude.Text)
updateBillingGroupResponse_name = Lens.lens (\UpdateBillingGroupResponse' {name} -> name) (\s@UpdateBillingGroupResponse' {} a -> s {name = a} :: UpdateBillingGroupResponse) Prelude.. Lens.mapping Data._Sensitive

-- | The Amazon Resource Name (ARN) of the pricing plan to compute Amazon Web
-- Services charges for the billing group.
updateBillingGroupResponse_pricingPlanArn :: Lens.Lens' UpdateBillingGroupResponse (Prelude.Maybe Prelude.Text)
updateBillingGroupResponse_pricingPlanArn = Lens.lens (\UpdateBillingGroupResponse' {pricingPlanArn} -> pricingPlanArn) (\s@UpdateBillingGroupResponse' {} a -> s {pricingPlanArn = a} :: UpdateBillingGroupResponse)

-- | The account ID that serves as the main account in a billing group.
updateBillingGroupResponse_primaryAccountId :: Lens.Lens' UpdateBillingGroupResponse (Prelude.Maybe Prelude.Text)
updateBillingGroupResponse_primaryAccountId = Lens.lens (\UpdateBillingGroupResponse' {primaryAccountId} -> primaryAccountId) (\s@UpdateBillingGroupResponse' {} a -> s {primaryAccountId = a} :: UpdateBillingGroupResponse)

-- | The number of accounts in the particular billing group.
updateBillingGroupResponse_size :: Lens.Lens' UpdateBillingGroupResponse (Prelude.Maybe Prelude.Natural)
updateBillingGroupResponse_size = Lens.lens (\UpdateBillingGroupResponse' {size} -> size) (\s@UpdateBillingGroupResponse' {} a -> s {size = a} :: UpdateBillingGroupResponse)

-- | The status of the billing group. Only one of the valid values can be
-- used.
updateBillingGroupResponse_status :: Lens.Lens' UpdateBillingGroupResponse (Prelude.Maybe BillingGroupStatus)
updateBillingGroupResponse_status = Lens.lens (\UpdateBillingGroupResponse' {status} -> status) (\s@UpdateBillingGroupResponse' {} a -> s {status = a} :: UpdateBillingGroupResponse)

-- | The reason why the billing group is in its current status.
updateBillingGroupResponse_statusReason :: Lens.Lens' UpdateBillingGroupResponse (Prelude.Maybe Prelude.Text)
updateBillingGroupResponse_statusReason = Lens.lens (\UpdateBillingGroupResponse' {statusReason} -> statusReason) (\s@UpdateBillingGroupResponse' {} a -> s {statusReason = a} :: UpdateBillingGroupResponse)

-- | The response's http status code.
updateBillingGroupResponse_httpStatus :: Lens.Lens' UpdateBillingGroupResponse Prelude.Int
updateBillingGroupResponse_httpStatus = Lens.lens (\UpdateBillingGroupResponse' {httpStatus} -> httpStatus) (\s@UpdateBillingGroupResponse' {} a -> s {httpStatus = a} :: UpdateBillingGroupResponse)

instance Prelude.NFData UpdateBillingGroupResponse where
  rnf UpdateBillingGroupResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf pricingPlanArn
      `Prelude.seq` Prelude.rnf primaryAccountId
      `Prelude.seq` Prelude.rnf size
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf statusReason
      `Prelude.seq` Prelude.rnf httpStatus
