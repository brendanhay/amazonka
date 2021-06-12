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
-- Module      : Network.AWS.IoT.AddThingToBillingGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds a thing to a billing group.
module Network.AWS.IoT.AddThingToBillingGroup
  ( -- * Creating a Request
    AddThingToBillingGroup (..),
    newAddThingToBillingGroup,

    -- * Request Lenses
    addThingToBillingGroup_thingArn,
    addThingToBillingGroup_thingName,
    addThingToBillingGroup_billingGroupArn,
    addThingToBillingGroup_billingGroupName,

    -- * Destructuring the Response
    AddThingToBillingGroupResponse (..),
    newAddThingToBillingGroupResponse,

    -- * Response Lenses
    addThingToBillingGroupResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newAddThingToBillingGroup' smart constructor.
data AddThingToBillingGroup = AddThingToBillingGroup'
  { -- | The ARN of the thing to be added to the billing group.
    thingArn :: Core.Maybe Core.Text,
    -- | The name of the thing to be added to the billing group.
    thingName :: Core.Maybe Core.Text,
    -- | The ARN of the billing group.
    billingGroupArn :: Core.Maybe Core.Text,
    -- | The name of the billing group.
    billingGroupName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AddThingToBillingGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'thingArn', 'addThingToBillingGroup_thingArn' - The ARN of the thing to be added to the billing group.
--
-- 'thingName', 'addThingToBillingGroup_thingName' - The name of the thing to be added to the billing group.
--
-- 'billingGroupArn', 'addThingToBillingGroup_billingGroupArn' - The ARN of the billing group.
--
-- 'billingGroupName', 'addThingToBillingGroup_billingGroupName' - The name of the billing group.
newAddThingToBillingGroup ::
  AddThingToBillingGroup
newAddThingToBillingGroup =
  AddThingToBillingGroup'
    { thingArn = Core.Nothing,
      thingName = Core.Nothing,
      billingGroupArn = Core.Nothing,
      billingGroupName = Core.Nothing
    }

-- | The ARN of the thing to be added to the billing group.
addThingToBillingGroup_thingArn :: Lens.Lens' AddThingToBillingGroup (Core.Maybe Core.Text)
addThingToBillingGroup_thingArn = Lens.lens (\AddThingToBillingGroup' {thingArn} -> thingArn) (\s@AddThingToBillingGroup' {} a -> s {thingArn = a} :: AddThingToBillingGroup)

-- | The name of the thing to be added to the billing group.
addThingToBillingGroup_thingName :: Lens.Lens' AddThingToBillingGroup (Core.Maybe Core.Text)
addThingToBillingGroup_thingName = Lens.lens (\AddThingToBillingGroup' {thingName} -> thingName) (\s@AddThingToBillingGroup' {} a -> s {thingName = a} :: AddThingToBillingGroup)

-- | The ARN of the billing group.
addThingToBillingGroup_billingGroupArn :: Lens.Lens' AddThingToBillingGroup (Core.Maybe Core.Text)
addThingToBillingGroup_billingGroupArn = Lens.lens (\AddThingToBillingGroup' {billingGroupArn} -> billingGroupArn) (\s@AddThingToBillingGroup' {} a -> s {billingGroupArn = a} :: AddThingToBillingGroup)

-- | The name of the billing group.
addThingToBillingGroup_billingGroupName :: Lens.Lens' AddThingToBillingGroup (Core.Maybe Core.Text)
addThingToBillingGroup_billingGroupName = Lens.lens (\AddThingToBillingGroup' {billingGroupName} -> billingGroupName) (\s@AddThingToBillingGroup' {} a -> s {billingGroupName = a} :: AddThingToBillingGroup)

instance Core.AWSRequest AddThingToBillingGroup where
  type
    AWSResponse AddThingToBillingGroup =
      AddThingToBillingGroupResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          AddThingToBillingGroupResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable AddThingToBillingGroup

instance Core.NFData AddThingToBillingGroup

instance Core.ToHeaders AddThingToBillingGroup where
  toHeaders = Core.const Core.mempty

instance Core.ToJSON AddThingToBillingGroup where
  toJSON AddThingToBillingGroup' {..} =
    Core.object
      ( Core.catMaybes
          [ ("thingArn" Core..=) Core.<$> thingArn,
            ("thingName" Core..=) Core.<$> thingName,
            ("billingGroupArn" Core..=) Core.<$> billingGroupArn,
            ("billingGroupName" Core..=)
              Core.<$> billingGroupName
          ]
      )

instance Core.ToPath AddThingToBillingGroup where
  toPath =
    Core.const "/billing-groups/addThingToBillingGroup"

instance Core.ToQuery AddThingToBillingGroup where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newAddThingToBillingGroupResponse' smart constructor.
data AddThingToBillingGroupResponse = AddThingToBillingGroupResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  AddThingToBillingGroupResponse
newAddThingToBillingGroupResponse pHttpStatus_ =
  AddThingToBillingGroupResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
addThingToBillingGroupResponse_httpStatus :: Lens.Lens' AddThingToBillingGroupResponse Core.Int
addThingToBillingGroupResponse_httpStatus = Lens.lens (\AddThingToBillingGroupResponse' {httpStatus} -> httpStatus) (\s@AddThingToBillingGroupResponse' {} a -> s {httpStatus = a} :: AddThingToBillingGroupResponse)

instance Core.NFData AddThingToBillingGroupResponse
