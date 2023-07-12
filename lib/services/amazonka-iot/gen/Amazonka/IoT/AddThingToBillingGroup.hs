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
-- Module      : Amazonka.IoT.AddThingToBillingGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds a thing to a billing group.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions AddThingToBillingGroup>
-- action.
module Amazonka.IoT.AddThingToBillingGroup
  ( -- * Creating a Request
    AddThingToBillingGroup (..),
    newAddThingToBillingGroup,

    -- * Request Lenses
    addThingToBillingGroup_billingGroupArn,
    addThingToBillingGroup_billingGroupName,
    addThingToBillingGroup_thingArn,
    addThingToBillingGroup_thingName,

    -- * Destructuring the Response
    AddThingToBillingGroupResponse (..),
    newAddThingToBillingGroupResponse,

    -- * Response Lenses
    addThingToBillingGroupResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAddThingToBillingGroup' smart constructor.
data AddThingToBillingGroup = AddThingToBillingGroup'
  { -- | The ARN of the billing group.
    billingGroupArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the billing group.
    --
    -- This call is asynchronous. It might take several seconds for the
    -- detachment to propagate.
    billingGroupName :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the thing to be added to the billing group.
    thingArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the thing to be added to the billing group.
    thingName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AddThingToBillingGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'billingGroupArn', 'addThingToBillingGroup_billingGroupArn' - The ARN of the billing group.
--
-- 'billingGroupName', 'addThingToBillingGroup_billingGroupName' - The name of the billing group.
--
-- This call is asynchronous. It might take several seconds for the
-- detachment to propagate.
--
-- 'thingArn', 'addThingToBillingGroup_thingArn' - The ARN of the thing to be added to the billing group.
--
-- 'thingName', 'addThingToBillingGroup_thingName' - The name of the thing to be added to the billing group.
newAddThingToBillingGroup ::
  AddThingToBillingGroup
newAddThingToBillingGroup =
  AddThingToBillingGroup'
    { billingGroupArn =
        Prelude.Nothing,
      billingGroupName = Prelude.Nothing,
      thingArn = Prelude.Nothing,
      thingName = Prelude.Nothing
    }

-- | The ARN of the billing group.
addThingToBillingGroup_billingGroupArn :: Lens.Lens' AddThingToBillingGroup (Prelude.Maybe Prelude.Text)
addThingToBillingGroup_billingGroupArn = Lens.lens (\AddThingToBillingGroup' {billingGroupArn} -> billingGroupArn) (\s@AddThingToBillingGroup' {} a -> s {billingGroupArn = a} :: AddThingToBillingGroup)

-- | The name of the billing group.
--
-- This call is asynchronous. It might take several seconds for the
-- detachment to propagate.
addThingToBillingGroup_billingGroupName :: Lens.Lens' AddThingToBillingGroup (Prelude.Maybe Prelude.Text)
addThingToBillingGroup_billingGroupName = Lens.lens (\AddThingToBillingGroup' {billingGroupName} -> billingGroupName) (\s@AddThingToBillingGroup' {} a -> s {billingGroupName = a} :: AddThingToBillingGroup)

-- | The ARN of the thing to be added to the billing group.
addThingToBillingGroup_thingArn :: Lens.Lens' AddThingToBillingGroup (Prelude.Maybe Prelude.Text)
addThingToBillingGroup_thingArn = Lens.lens (\AddThingToBillingGroup' {thingArn} -> thingArn) (\s@AddThingToBillingGroup' {} a -> s {thingArn = a} :: AddThingToBillingGroup)

-- | The name of the thing to be added to the billing group.
addThingToBillingGroup_thingName :: Lens.Lens' AddThingToBillingGroup (Prelude.Maybe Prelude.Text)
addThingToBillingGroup_thingName = Lens.lens (\AddThingToBillingGroup' {thingName} -> thingName) (\s@AddThingToBillingGroup' {} a -> s {thingName = a} :: AddThingToBillingGroup)

instance Core.AWSRequest AddThingToBillingGroup where
  type
    AWSResponse AddThingToBillingGroup =
      AddThingToBillingGroupResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          AddThingToBillingGroupResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AddThingToBillingGroup where
  hashWithSalt _salt AddThingToBillingGroup' {..} =
    _salt
      `Prelude.hashWithSalt` billingGroupArn
      `Prelude.hashWithSalt` billingGroupName
      `Prelude.hashWithSalt` thingArn
      `Prelude.hashWithSalt` thingName

instance Prelude.NFData AddThingToBillingGroup where
  rnf AddThingToBillingGroup' {..} =
    Prelude.rnf billingGroupArn
      `Prelude.seq` Prelude.rnf billingGroupName
      `Prelude.seq` Prelude.rnf thingArn
      `Prelude.seq` Prelude.rnf thingName

instance Data.ToHeaders AddThingToBillingGroup where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON AddThingToBillingGroup where
  toJSON AddThingToBillingGroup' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("billingGroupArn" Data..=)
              Prelude.<$> billingGroupArn,
            ("billingGroupName" Data..=)
              Prelude.<$> billingGroupName,
            ("thingArn" Data..=) Prelude.<$> thingArn,
            ("thingName" Data..=) Prelude.<$> thingName
          ]
      )

instance Data.ToPath AddThingToBillingGroup where
  toPath =
    Prelude.const
      "/billing-groups/addThingToBillingGroup"

instance Data.ToQuery AddThingToBillingGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAddThingToBillingGroupResponse' smart constructor.
data AddThingToBillingGroupResponse = AddThingToBillingGroupResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AddThingToBillingGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'addThingToBillingGroupResponse_httpStatus' - The response's http status code.
newAddThingToBillingGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AddThingToBillingGroupResponse
newAddThingToBillingGroupResponse pHttpStatus_ =
  AddThingToBillingGroupResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
addThingToBillingGroupResponse_httpStatus :: Lens.Lens' AddThingToBillingGroupResponse Prelude.Int
addThingToBillingGroupResponse_httpStatus = Lens.lens (\AddThingToBillingGroupResponse' {httpStatus} -> httpStatus) (\s@AddThingToBillingGroupResponse' {} a -> s {httpStatus = a} :: AddThingToBillingGroupResponse)

instance
  Prelude.NFData
    AddThingToBillingGroupResponse
  where
  rnf AddThingToBillingGroupResponse' {..} =
    Prelude.rnf httpStatus
