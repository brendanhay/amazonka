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
-- Module      : Network.AWS.IoT.RemoveThingFromBillingGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the given thing from the billing group.
module Network.AWS.IoT.RemoveThingFromBillingGroup
  ( -- * Creating a Request
    RemoveThingFromBillingGroup (..),
    newRemoveThingFromBillingGroup,

    -- * Request Lenses
    removeThingFromBillingGroup_thingArn,
    removeThingFromBillingGroup_thingName,
    removeThingFromBillingGroup_billingGroupArn,
    removeThingFromBillingGroup_billingGroupName,

    -- * Destructuring the Response
    RemoveThingFromBillingGroupResponse (..),
    newRemoveThingFromBillingGroupResponse,

    -- * Response Lenses
    removeThingFromBillingGroupResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newRemoveThingFromBillingGroup' smart constructor.
data RemoveThingFromBillingGroup = RemoveThingFromBillingGroup'
  { -- | The ARN of the thing to be removed from the billing group.
    thingArn :: Core.Maybe Core.Text,
    -- | The name of the thing to be removed from the billing group.
    thingName :: Core.Maybe Core.Text,
    -- | The ARN of the billing group.
    billingGroupArn :: Core.Maybe Core.Text,
    -- | The name of the billing group.
    billingGroupName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RemoveThingFromBillingGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'thingArn', 'removeThingFromBillingGroup_thingArn' - The ARN of the thing to be removed from the billing group.
--
-- 'thingName', 'removeThingFromBillingGroup_thingName' - The name of the thing to be removed from the billing group.
--
-- 'billingGroupArn', 'removeThingFromBillingGroup_billingGroupArn' - The ARN of the billing group.
--
-- 'billingGroupName', 'removeThingFromBillingGroup_billingGroupName' - The name of the billing group.
newRemoveThingFromBillingGroup ::
  RemoveThingFromBillingGroup
newRemoveThingFromBillingGroup =
  RemoveThingFromBillingGroup'
    { thingArn =
        Core.Nothing,
      thingName = Core.Nothing,
      billingGroupArn = Core.Nothing,
      billingGroupName = Core.Nothing
    }

-- | The ARN of the thing to be removed from the billing group.
removeThingFromBillingGroup_thingArn :: Lens.Lens' RemoveThingFromBillingGroup (Core.Maybe Core.Text)
removeThingFromBillingGroup_thingArn = Lens.lens (\RemoveThingFromBillingGroup' {thingArn} -> thingArn) (\s@RemoveThingFromBillingGroup' {} a -> s {thingArn = a} :: RemoveThingFromBillingGroup)

-- | The name of the thing to be removed from the billing group.
removeThingFromBillingGroup_thingName :: Lens.Lens' RemoveThingFromBillingGroup (Core.Maybe Core.Text)
removeThingFromBillingGroup_thingName = Lens.lens (\RemoveThingFromBillingGroup' {thingName} -> thingName) (\s@RemoveThingFromBillingGroup' {} a -> s {thingName = a} :: RemoveThingFromBillingGroup)

-- | The ARN of the billing group.
removeThingFromBillingGroup_billingGroupArn :: Lens.Lens' RemoveThingFromBillingGroup (Core.Maybe Core.Text)
removeThingFromBillingGroup_billingGroupArn = Lens.lens (\RemoveThingFromBillingGroup' {billingGroupArn} -> billingGroupArn) (\s@RemoveThingFromBillingGroup' {} a -> s {billingGroupArn = a} :: RemoveThingFromBillingGroup)

-- | The name of the billing group.
removeThingFromBillingGroup_billingGroupName :: Lens.Lens' RemoveThingFromBillingGroup (Core.Maybe Core.Text)
removeThingFromBillingGroup_billingGroupName = Lens.lens (\RemoveThingFromBillingGroup' {billingGroupName} -> billingGroupName) (\s@RemoveThingFromBillingGroup' {} a -> s {billingGroupName = a} :: RemoveThingFromBillingGroup)

instance Core.AWSRequest RemoveThingFromBillingGroup where
  type
    AWSResponse RemoveThingFromBillingGroup =
      RemoveThingFromBillingGroupResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          RemoveThingFromBillingGroupResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable RemoveThingFromBillingGroup

instance Core.NFData RemoveThingFromBillingGroup

instance Core.ToHeaders RemoveThingFromBillingGroup where
  toHeaders = Core.const Core.mempty

instance Core.ToJSON RemoveThingFromBillingGroup where
  toJSON RemoveThingFromBillingGroup' {..} =
    Core.object
      ( Core.catMaybes
          [ ("thingArn" Core..=) Core.<$> thingArn,
            ("thingName" Core..=) Core.<$> thingName,
            ("billingGroupArn" Core..=) Core.<$> billingGroupArn,
            ("billingGroupName" Core..=)
              Core.<$> billingGroupName
          ]
      )

instance Core.ToPath RemoveThingFromBillingGroup where
  toPath =
    Core.const
      "/billing-groups/removeThingFromBillingGroup"

instance Core.ToQuery RemoveThingFromBillingGroup where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newRemoveThingFromBillingGroupResponse' smart constructor.
data RemoveThingFromBillingGroupResponse = RemoveThingFromBillingGroupResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  RemoveThingFromBillingGroupResponse
newRemoveThingFromBillingGroupResponse pHttpStatus_ =
  RemoveThingFromBillingGroupResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
removeThingFromBillingGroupResponse_httpStatus :: Lens.Lens' RemoveThingFromBillingGroupResponse Core.Int
removeThingFromBillingGroupResponse_httpStatus = Lens.lens (\RemoveThingFromBillingGroupResponse' {httpStatus} -> httpStatus) (\s@RemoveThingFromBillingGroupResponse' {} a -> s {httpStatus = a} :: RemoveThingFromBillingGroupResponse)

instance
  Core.NFData
    RemoveThingFromBillingGroupResponse
