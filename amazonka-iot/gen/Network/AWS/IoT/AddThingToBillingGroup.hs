{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newAddThingToBillingGroup' smart constructor.
data AddThingToBillingGroup = AddThingToBillingGroup'
  { -- | The ARN of the thing to be added to the billing group.
    thingArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the thing to be added to the billing group.
    thingName :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the billing group.
    billingGroupArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the billing group.
    billingGroupName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { thingArn = Prelude.Nothing,
      thingName = Prelude.Nothing,
      billingGroupArn = Prelude.Nothing,
      billingGroupName = Prelude.Nothing
    }

-- | The ARN of the thing to be added to the billing group.
addThingToBillingGroup_thingArn :: Lens.Lens' AddThingToBillingGroup (Prelude.Maybe Prelude.Text)
addThingToBillingGroup_thingArn = Lens.lens (\AddThingToBillingGroup' {thingArn} -> thingArn) (\s@AddThingToBillingGroup' {} a -> s {thingArn = a} :: AddThingToBillingGroup)

-- | The name of the thing to be added to the billing group.
addThingToBillingGroup_thingName :: Lens.Lens' AddThingToBillingGroup (Prelude.Maybe Prelude.Text)
addThingToBillingGroup_thingName = Lens.lens (\AddThingToBillingGroup' {thingName} -> thingName) (\s@AddThingToBillingGroup' {} a -> s {thingName = a} :: AddThingToBillingGroup)

-- | The ARN of the billing group.
addThingToBillingGroup_billingGroupArn :: Lens.Lens' AddThingToBillingGroup (Prelude.Maybe Prelude.Text)
addThingToBillingGroup_billingGroupArn = Lens.lens (\AddThingToBillingGroup' {billingGroupArn} -> billingGroupArn) (\s@AddThingToBillingGroup' {} a -> s {billingGroupArn = a} :: AddThingToBillingGroup)

-- | The name of the billing group.
addThingToBillingGroup_billingGroupName :: Lens.Lens' AddThingToBillingGroup (Prelude.Maybe Prelude.Text)
addThingToBillingGroup_billingGroupName = Lens.lens (\AddThingToBillingGroup' {billingGroupName} -> billingGroupName) (\s@AddThingToBillingGroup' {} a -> s {billingGroupName = a} :: AddThingToBillingGroup)

instance Prelude.AWSRequest AddThingToBillingGroup where
  type
    Rs AddThingToBillingGroup =
      AddThingToBillingGroupResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          AddThingToBillingGroupResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AddThingToBillingGroup

instance Prelude.NFData AddThingToBillingGroup

instance Prelude.ToHeaders AddThingToBillingGroup where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToJSON AddThingToBillingGroup where
  toJSON AddThingToBillingGroup' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("thingArn" Prelude..=) Prelude.<$> thingArn,
            ("thingName" Prelude..=) Prelude.<$> thingName,
            ("billingGroupArn" Prelude..=)
              Prelude.<$> billingGroupArn,
            ("billingGroupName" Prelude..=)
              Prelude.<$> billingGroupName
          ]
      )

instance Prelude.ToPath AddThingToBillingGroup where
  toPath =
    Prelude.const
      "/billing-groups/addThingToBillingGroup"

instance Prelude.ToQuery AddThingToBillingGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAddThingToBillingGroupResponse' smart constructor.
data AddThingToBillingGroupResponse = AddThingToBillingGroupResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
