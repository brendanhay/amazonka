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
-- Module      : Network.AWS.IoT.AddThingToThingGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds a thing to a thing group.
module Network.AWS.IoT.AddThingToThingGroup
  ( -- * Creating a Request
    AddThingToThingGroup (..),
    newAddThingToThingGroup,

    -- * Request Lenses
    addThingToThingGroup_thingArn,
    addThingToThingGroup_thingGroupArn,
    addThingToThingGroup_thingName,
    addThingToThingGroup_thingGroupName,
    addThingToThingGroup_overrideDynamicGroups,

    -- * Destructuring the Response
    AddThingToThingGroupResponse (..),
    newAddThingToThingGroupResponse,

    -- * Response Lenses
    addThingToThingGroupResponse_httpStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newAddThingToThingGroup' smart constructor.
data AddThingToThingGroup = AddThingToThingGroup'
  { -- | The ARN of the thing to add to a group.
    thingArn :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the group to which you are adding a thing.
    thingGroupArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the thing to add to a group.
    thingName :: Prelude.Maybe Prelude.Text,
    -- | The name of the group to which you are adding a thing.
    thingGroupName :: Prelude.Maybe Prelude.Text,
    -- | Override dynamic thing groups with static thing groups when 10-group
    -- limit is reached. If a thing belongs to 10 thing groups, and one or more
    -- of those groups are dynamic thing groups, adding a thing to a static
    -- group removes the thing from the last dynamic group.
    overrideDynamicGroups :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AddThingToThingGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'thingArn', 'addThingToThingGroup_thingArn' - The ARN of the thing to add to a group.
--
-- 'thingGroupArn', 'addThingToThingGroup_thingGroupArn' - The ARN of the group to which you are adding a thing.
--
-- 'thingName', 'addThingToThingGroup_thingName' - The name of the thing to add to a group.
--
-- 'thingGroupName', 'addThingToThingGroup_thingGroupName' - The name of the group to which you are adding a thing.
--
-- 'overrideDynamicGroups', 'addThingToThingGroup_overrideDynamicGroups' - Override dynamic thing groups with static thing groups when 10-group
-- limit is reached. If a thing belongs to 10 thing groups, and one or more
-- of those groups are dynamic thing groups, adding a thing to a static
-- group removes the thing from the last dynamic group.
newAddThingToThingGroup ::
  AddThingToThingGroup
newAddThingToThingGroup =
  AddThingToThingGroup'
    { thingArn = Prelude.Nothing,
      thingGroupArn = Prelude.Nothing,
      thingName = Prelude.Nothing,
      thingGroupName = Prelude.Nothing,
      overrideDynamicGroups = Prelude.Nothing
    }

-- | The ARN of the thing to add to a group.
addThingToThingGroup_thingArn :: Lens.Lens' AddThingToThingGroup (Prelude.Maybe Prelude.Text)
addThingToThingGroup_thingArn = Lens.lens (\AddThingToThingGroup' {thingArn} -> thingArn) (\s@AddThingToThingGroup' {} a -> s {thingArn = a} :: AddThingToThingGroup)

-- | The ARN of the group to which you are adding a thing.
addThingToThingGroup_thingGroupArn :: Lens.Lens' AddThingToThingGroup (Prelude.Maybe Prelude.Text)
addThingToThingGroup_thingGroupArn = Lens.lens (\AddThingToThingGroup' {thingGroupArn} -> thingGroupArn) (\s@AddThingToThingGroup' {} a -> s {thingGroupArn = a} :: AddThingToThingGroup)

-- | The name of the thing to add to a group.
addThingToThingGroup_thingName :: Lens.Lens' AddThingToThingGroup (Prelude.Maybe Prelude.Text)
addThingToThingGroup_thingName = Lens.lens (\AddThingToThingGroup' {thingName} -> thingName) (\s@AddThingToThingGroup' {} a -> s {thingName = a} :: AddThingToThingGroup)

-- | The name of the group to which you are adding a thing.
addThingToThingGroup_thingGroupName :: Lens.Lens' AddThingToThingGroup (Prelude.Maybe Prelude.Text)
addThingToThingGroup_thingGroupName = Lens.lens (\AddThingToThingGroup' {thingGroupName} -> thingGroupName) (\s@AddThingToThingGroup' {} a -> s {thingGroupName = a} :: AddThingToThingGroup)

-- | Override dynamic thing groups with static thing groups when 10-group
-- limit is reached. If a thing belongs to 10 thing groups, and one or more
-- of those groups are dynamic thing groups, adding a thing to a static
-- group removes the thing from the last dynamic group.
addThingToThingGroup_overrideDynamicGroups :: Lens.Lens' AddThingToThingGroup (Prelude.Maybe Prelude.Bool)
addThingToThingGroup_overrideDynamicGroups = Lens.lens (\AddThingToThingGroup' {overrideDynamicGroups} -> overrideDynamicGroups) (\s@AddThingToThingGroup' {} a -> s {overrideDynamicGroups = a} :: AddThingToThingGroup)

instance Prelude.AWSRequest AddThingToThingGroup where
  type
    Rs AddThingToThingGroup =
      AddThingToThingGroupResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          AddThingToThingGroupResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AddThingToThingGroup

instance Prelude.NFData AddThingToThingGroup

instance Prelude.ToHeaders AddThingToThingGroup where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToJSON AddThingToThingGroup where
  toJSON AddThingToThingGroup' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("thingArn" Prelude..=) Prelude.<$> thingArn,
            ("thingGroupArn" Prelude..=)
              Prelude.<$> thingGroupArn,
            ("thingName" Prelude..=) Prelude.<$> thingName,
            ("thingGroupName" Prelude..=)
              Prelude.<$> thingGroupName,
            ("overrideDynamicGroups" Prelude..=)
              Prelude.<$> overrideDynamicGroups
          ]
      )

instance Prelude.ToPath AddThingToThingGroup where
  toPath =
    Prelude.const "/thing-groups/addThingToThingGroup"

instance Prelude.ToQuery AddThingToThingGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAddThingToThingGroupResponse' smart constructor.
data AddThingToThingGroupResponse = AddThingToThingGroupResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.NFData AddThingToThingGroupResponse
