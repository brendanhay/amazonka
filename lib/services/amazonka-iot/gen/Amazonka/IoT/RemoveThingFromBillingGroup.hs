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
-- Module      : Amazonka.IoT.RemoveThingFromBillingGroup
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the given thing from the billing group.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions RemoveThingFromBillingGroup>
-- action.
--
-- This call is asynchronous. It might take several seconds for the
-- detachment to propagate.
module Amazonka.IoT.RemoveThingFromBillingGroup
  ( -- * Creating a Request
    RemoveThingFromBillingGroup (..),
    newRemoveThingFromBillingGroup,

    -- * Request Lenses
    removeThingFromBillingGroup_billingGroupName,
    removeThingFromBillingGroup_thingName,
    removeThingFromBillingGroup_billingGroupArn,
    removeThingFromBillingGroup_thingArn,

    -- * Destructuring the Response
    RemoveThingFromBillingGroupResponse (..),
    newRemoveThingFromBillingGroupResponse,

    -- * Response Lenses
    removeThingFromBillingGroupResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newRemoveThingFromBillingGroup' smart constructor.
data RemoveThingFromBillingGroup = RemoveThingFromBillingGroup'
  { -- | The name of the billing group.
    billingGroupName :: Prelude.Maybe Prelude.Text,
    -- | The name of the thing to be removed from the billing group.
    thingName :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the billing group.
    billingGroupArn :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the thing to be removed from the billing group.
    thingArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RemoveThingFromBillingGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'billingGroupName', 'removeThingFromBillingGroup_billingGroupName' - The name of the billing group.
--
-- 'thingName', 'removeThingFromBillingGroup_thingName' - The name of the thing to be removed from the billing group.
--
-- 'billingGroupArn', 'removeThingFromBillingGroup_billingGroupArn' - The ARN of the billing group.
--
-- 'thingArn', 'removeThingFromBillingGroup_thingArn' - The ARN of the thing to be removed from the billing group.
newRemoveThingFromBillingGroup ::
  RemoveThingFromBillingGroup
newRemoveThingFromBillingGroup =
  RemoveThingFromBillingGroup'
    { billingGroupName =
        Prelude.Nothing,
      thingName = Prelude.Nothing,
      billingGroupArn = Prelude.Nothing,
      thingArn = Prelude.Nothing
    }

-- | The name of the billing group.
removeThingFromBillingGroup_billingGroupName :: Lens.Lens' RemoveThingFromBillingGroup (Prelude.Maybe Prelude.Text)
removeThingFromBillingGroup_billingGroupName = Lens.lens (\RemoveThingFromBillingGroup' {billingGroupName} -> billingGroupName) (\s@RemoveThingFromBillingGroup' {} a -> s {billingGroupName = a} :: RemoveThingFromBillingGroup)

-- | The name of the thing to be removed from the billing group.
removeThingFromBillingGroup_thingName :: Lens.Lens' RemoveThingFromBillingGroup (Prelude.Maybe Prelude.Text)
removeThingFromBillingGroup_thingName = Lens.lens (\RemoveThingFromBillingGroup' {thingName} -> thingName) (\s@RemoveThingFromBillingGroup' {} a -> s {thingName = a} :: RemoveThingFromBillingGroup)

-- | The ARN of the billing group.
removeThingFromBillingGroup_billingGroupArn :: Lens.Lens' RemoveThingFromBillingGroup (Prelude.Maybe Prelude.Text)
removeThingFromBillingGroup_billingGroupArn = Lens.lens (\RemoveThingFromBillingGroup' {billingGroupArn} -> billingGroupArn) (\s@RemoveThingFromBillingGroup' {} a -> s {billingGroupArn = a} :: RemoveThingFromBillingGroup)

-- | The ARN of the thing to be removed from the billing group.
removeThingFromBillingGroup_thingArn :: Lens.Lens' RemoveThingFromBillingGroup (Prelude.Maybe Prelude.Text)
removeThingFromBillingGroup_thingArn = Lens.lens (\RemoveThingFromBillingGroup' {thingArn} -> thingArn) (\s@RemoveThingFromBillingGroup' {} a -> s {thingArn = a} :: RemoveThingFromBillingGroup)

instance Core.AWSRequest RemoveThingFromBillingGroup where
  type
    AWSResponse RemoveThingFromBillingGroup =
      RemoveThingFromBillingGroupResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          RemoveThingFromBillingGroupResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RemoveThingFromBillingGroup where
  hashWithSalt _salt RemoveThingFromBillingGroup' {..} =
    _salt `Prelude.hashWithSalt` billingGroupName
      `Prelude.hashWithSalt` thingName
      `Prelude.hashWithSalt` billingGroupArn
      `Prelude.hashWithSalt` thingArn

instance Prelude.NFData RemoveThingFromBillingGroup where
  rnf RemoveThingFromBillingGroup' {..} =
    Prelude.rnf billingGroupName
      `Prelude.seq` Prelude.rnf thingName
      `Prelude.seq` Prelude.rnf billingGroupArn
      `Prelude.seq` Prelude.rnf thingArn

instance Core.ToHeaders RemoveThingFromBillingGroup where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToJSON RemoveThingFromBillingGroup where
  toJSON RemoveThingFromBillingGroup' {..} =
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

instance Core.ToPath RemoveThingFromBillingGroup where
  toPath =
    Prelude.const
      "/billing-groups/removeThingFromBillingGroup"

instance Core.ToQuery RemoveThingFromBillingGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRemoveThingFromBillingGroupResponse' smart constructor.
data RemoveThingFromBillingGroupResponse = RemoveThingFromBillingGroupResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RemoveThingFromBillingGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'removeThingFromBillingGroupResponse_httpStatus' - The response's http status code.
newRemoveThingFromBillingGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RemoveThingFromBillingGroupResponse
newRemoveThingFromBillingGroupResponse pHttpStatus_ =
  RemoveThingFromBillingGroupResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
removeThingFromBillingGroupResponse_httpStatus :: Lens.Lens' RemoveThingFromBillingGroupResponse Prelude.Int
removeThingFromBillingGroupResponse_httpStatus = Lens.lens (\RemoveThingFromBillingGroupResponse' {httpStatus} -> httpStatus) (\s@RemoveThingFromBillingGroupResponse' {} a -> s {httpStatus = a} :: RemoveThingFromBillingGroupResponse)

instance
  Prelude.NFData
    RemoveThingFromBillingGroupResponse
  where
  rnf RemoveThingFromBillingGroupResponse' {..} =
    Prelude.rnf httpStatus
