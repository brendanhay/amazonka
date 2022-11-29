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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
    addThingToBillingGroup_billingGroupName,
    addThingToBillingGroup_thingName,
    addThingToBillingGroup_billingGroupArn,
    addThingToBillingGroup_thingArn,

    -- * Destructuring the Response
    AddThingToBillingGroupResponse (..),
    newAddThingToBillingGroupResponse,

    -- * Response Lenses
    addThingToBillingGroupResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAddThingToBillingGroup' smart constructor.
data AddThingToBillingGroup = AddThingToBillingGroup'
  { -- | The name of the billing group.
    --
    -- This call is asynchronous. It might take several seconds for the
    -- detachment to propagate.
    billingGroupName :: Prelude.Maybe Prelude.Text,
    -- | The name of the thing to be added to the billing group.
    thingName :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the billing group.
    billingGroupArn :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the thing to be added to the billing group.
    thingArn :: Prelude.Maybe Prelude.Text
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
-- 'billingGroupName', 'addThingToBillingGroup_billingGroupName' - The name of the billing group.
--
-- This call is asynchronous. It might take several seconds for the
-- detachment to propagate.
--
-- 'thingName', 'addThingToBillingGroup_thingName' - The name of the thing to be added to the billing group.
--
-- 'billingGroupArn', 'addThingToBillingGroup_billingGroupArn' - The ARN of the billing group.
--
-- 'thingArn', 'addThingToBillingGroup_thingArn' - The ARN of the thing to be added to the billing group.
newAddThingToBillingGroup ::
  AddThingToBillingGroup
newAddThingToBillingGroup =
  AddThingToBillingGroup'
    { billingGroupName =
        Prelude.Nothing,
      thingName = Prelude.Nothing,
      billingGroupArn = Prelude.Nothing,
      thingArn = Prelude.Nothing
    }

-- | The name of the billing group.
--
-- This call is asynchronous. It might take several seconds for the
-- detachment to propagate.
addThingToBillingGroup_billingGroupName :: Lens.Lens' AddThingToBillingGroup (Prelude.Maybe Prelude.Text)
addThingToBillingGroup_billingGroupName = Lens.lens (\AddThingToBillingGroup' {billingGroupName} -> billingGroupName) (\s@AddThingToBillingGroup' {} a -> s {billingGroupName = a} :: AddThingToBillingGroup)

-- | The name of the thing to be added to the billing group.
addThingToBillingGroup_thingName :: Lens.Lens' AddThingToBillingGroup (Prelude.Maybe Prelude.Text)
addThingToBillingGroup_thingName = Lens.lens (\AddThingToBillingGroup' {thingName} -> thingName) (\s@AddThingToBillingGroup' {} a -> s {thingName = a} :: AddThingToBillingGroup)

-- | The ARN of the billing group.
addThingToBillingGroup_billingGroupArn :: Lens.Lens' AddThingToBillingGroup (Prelude.Maybe Prelude.Text)
addThingToBillingGroup_billingGroupArn = Lens.lens (\AddThingToBillingGroup' {billingGroupArn} -> billingGroupArn) (\s@AddThingToBillingGroup' {} a -> s {billingGroupArn = a} :: AddThingToBillingGroup)

-- | The ARN of the thing to be added to the billing group.
addThingToBillingGroup_thingArn :: Lens.Lens' AddThingToBillingGroup (Prelude.Maybe Prelude.Text)
addThingToBillingGroup_thingArn = Lens.lens (\AddThingToBillingGroup' {thingArn} -> thingArn) (\s@AddThingToBillingGroup' {} a -> s {thingArn = a} :: AddThingToBillingGroup)

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
    _salt `Prelude.hashWithSalt` billingGroupName
      `Prelude.hashWithSalt` thingName
      `Prelude.hashWithSalt` billingGroupArn
      `Prelude.hashWithSalt` thingArn

instance Prelude.NFData AddThingToBillingGroup where
  rnf AddThingToBillingGroup' {..} =
    Prelude.rnf billingGroupName
      `Prelude.seq` Prelude.rnf thingName
      `Prelude.seq` Prelude.rnf billingGroupArn
      `Prelude.seq` Prelude.rnf thingArn

instance Core.ToHeaders AddThingToBillingGroup where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToJSON AddThingToBillingGroup where
  toJSON AddThingToBillingGroup' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("billingGroupName" Core..=)
              Prelude.<$> billingGroupName,
            ("thingName" Core..=) Prelude.<$> thingName,
            ("billingGroupArn" Core..=)
              Prelude.<$> billingGroupArn,
            ("thingArn" Core..=) Prelude.<$> thingArn
          ]
      )

instance Core.ToPath AddThingToBillingGroup where
  toPath =
    Prelude.const
      "/billing-groups/addThingToBillingGroup"

instance Core.ToQuery AddThingToBillingGroup where
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
