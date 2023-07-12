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
-- Module      : Amazonka.IoT.AddThingToThingGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds a thing to a thing group.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions AddThingToThingGroup>
-- action.
module Amazonka.IoT.AddThingToThingGroup
  ( -- * Creating a Request
    AddThingToThingGroup (..),
    newAddThingToThingGroup,

    -- * Request Lenses
    addThingToThingGroup_overrideDynamicGroups,
    addThingToThingGroup_thingArn,
    addThingToThingGroup_thingGroupArn,
    addThingToThingGroup_thingGroupName,
    addThingToThingGroup_thingName,

    -- * Destructuring the Response
    AddThingToThingGroupResponse (..),
    newAddThingToThingGroupResponse,

    -- * Response Lenses
    addThingToThingGroupResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAddThingToThingGroup' smart constructor.
data AddThingToThingGroup = AddThingToThingGroup'
  { -- | Override dynamic thing groups with static thing groups when 10-group
    -- limit is reached. If a thing belongs to 10 thing groups, and one or more
    -- of those groups are dynamic thing groups, adding a thing to a static
    -- group removes the thing from the last dynamic group.
    overrideDynamicGroups :: Prelude.Maybe Prelude.Bool,
    -- | The ARN of the thing to add to a group.
    thingArn :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the group to which you are adding a thing.
    thingGroupArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the group to which you are adding a thing.
    thingGroupName :: Prelude.Maybe Prelude.Text,
    -- | The name of the thing to add to a group.
    thingName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AddThingToThingGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'overrideDynamicGroups', 'addThingToThingGroup_overrideDynamicGroups' - Override dynamic thing groups with static thing groups when 10-group
-- limit is reached. If a thing belongs to 10 thing groups, and one or more
-- of those groups are dynamic thing groups, adding a thing to a static
-- group removes the thing from the last dynamic group.
--
-- 'thingArn', 'addThingToThingGroup_thingArn' - The ARN of the thing to add to a group.
--
-- 'thingGroupArn', 'addThingToThingGroup_thingGroupArn' - The ARN of the group to which you are adding a thing.
--
-- 'thingGroupName', 'addThingToThingGroup_thingGroupName' - The name of the group to which you are adding a thing.
--
-- 'thingName', 'addThingToThingGroup_thingName' - The name of the thing to add to a group.
newAddThingToThingGroup ::
  AddThingToThingGroup
newAddThingToThingGroup =
  AddThingToThingGroup'
    { overrideDynamicGroups =
        Prelude.Nothing,
      thingArn = Prelude.Nothing,
      thingGroupArn = Prelude.Nothing,
      thingGroupName = Prelude.Nothing,
      thingName = Prelude.Nothing
    }

-- | Override dynamic thing groups with static thing groups when 10-group
-- limit is reached. If a thing belongs to 10 thing groups, and one or more
-- of those groups are dynamic thing groups, adding a thing to a static
-- group removes the thing from the last dynamic group.
addThingToThingGroup_overrideDynamicGroups :: Lens.Lens' AddThingToThingGroup (Prelude.Maybe Prelude.Bool)
addThingToThingGroup_overrideDynamicGroups = Lens.lens (\AddThingToThingGroup' {overrideDynamicGroups} -> overrideDynamicGroups) (\s@AddThingToThingGroup' {} a -> s {overrideDynamicGroups = a} :: AddThingToThingGroup)

-- | The ARN of the thing to add to a group.
addThingToThingGroup_thingArn :: Lens.Lens' AddThingToThingGroup (Prelude.Maybe Prelude.Text)
addThingToThingGroup_thingArn = Lens.lens (\AddThingToThingGroup' {thingArn} -> thingArn) (\s@AddThingToThingGroup' {} a -> s {thingArn = a} :: AddThingToThingGroup)

-- | The ARN of the group to which you are adding a thing.
addThingToThingGroup_thingGroupArn :: Lens.Lens' AddThingToThingGroup (Prelude.Maybe Prelude.Text)
addThingToThingGroup_thingGroupArn = Lens.lens (\AddThingToThingGroup' {thingGroupArn} -> thingGroupArn) (\s@AddThingToThingGroup' {} a -> s {thingGroupArn = a} :: AddThingToThingGroup)

-- | The name of the group to which you are adding a thing.
addThingToThingGroup_thingGroupName :: Lens.Lens' AddThingToThingGroup (Prelude.Maybe Prelude.Text)
addThingToThingGroup_thingGroupName = Lens.lens (\AddThingToThingGroup' {thingGroupName} -> thingGroupName) (\s@AddThingToThingGroup' {} a -> s {thingGroupName = a} :: AddThingToThingGroup)

-- | The name of the thing to add to a group.
addThingToThingGroup_thingName :: Lens.Lens' AddThingToThingGroup (Prelude.Maybe Prelude.Text)
addThingToThingGroup_thingName = Lens.lens (\AddThingToThingGroup' {thingName} -> thingName) (\s@AddThingToThingGroup' {} a -> s {thingName = a} :: AddThingToThingGroup)

instance Core.AWSRequest AddThingToThingGroup where
  type
    AWSResponse AddThingToThingGroup =
      AddThingToThingGroupResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          AddThingToThingGroupResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AddThingToThingGroup where
  hashWithSalt _salt AddThingToThingGroup' {..} =
    _salt
      `Prelude.hashWithSalt` overrideDynamicGroups
      `Prelude.hashWithSalt` thingArn
      `Prelude.hashWithSalt` thingGroupArn
      `Prelude.hashWithSalt` thingGroupName
      `Prelude.hashWithSalt` thingName

instance Prelude.NFData AddThingToThingGroup where
  rnf AddThingToThingGroup' {..} =
    Prelude.rnf overrideDynamicGroups
      `Prelude.seq` Prelude.rnf thingArn
      `Prelude.seq` Prelude.rnf thingGroupArn
      `Prelude.seq` Prelude.rnf thingGroupName
      `Prelude.seq` Prelude.rnf thingName

instance Data.ToHeaders AddThingToThingGroup where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON AddThingToThingGroup where
  toJSON AddThingToThingGroup' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("overrideDynamicGroups" Data..=)
              Prelude.<$> overrideDynamicGroups,
            ("thingArn" Data..=) Prelude.<$> thingArn,
            ("thingGroupArn" Data..=) Prelude.<$> thingGroupArn,
            ("thingGroupName" Data..=)
              Prelude.<$> thingGroupName,
            ("thingName" Data..=) Prelude.<$> thingName
          ]
      )

instance Data.ToPath AddThingToThingGroup where
  toPath =
    Prelude.const "/thing-groups/addThingToThingGroup"

instance Data.ToQuery AddThingToThingGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAddThingToThingGroupResponse' smart constructor.
data AddThingToThingGroupResponse = AddThingToThingGroupResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AddThingToThingGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'addThingToThingGroupResponse_httpStatus' - The response's http status code.
newAddThingToThingGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AddThingToThingGroupResponse
newAddThingToThingGroupResponse pHttpStatus_ =
  AddThingToThingGroupResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
addThingToThingGroupResponse_httpStatus :: Lens.Lens' AddThingToThingGroupResponse Prelude.Int
addThingToThingGroupResponse_httpStatus = Lens.lens (\AddThingToThingGroupResponse' {httpStatus} -> httpStatus) (\s@AddThingToThingGroupResponse' {} a -> s {httpStatus = a} :: AddThingToThingGroupResponse)

instance Prelude.NFData AddThingToThingGroupResponse where
  rnf AddThingToThingGroupResponse' {..} =
    Prelude.rnf httpStatus
