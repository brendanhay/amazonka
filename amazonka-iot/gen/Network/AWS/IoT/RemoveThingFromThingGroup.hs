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
-- Module      : Network.AWS.IoT.RemoveThingFromThingGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Remove the specified thing from the specified group.
--
-- You must specify either a @thingGroupArn@ or a @thingGroupName@ to
-- identify the thing group and either a @thingArn@ or a @thingName@ to
-- identify the thing to remove from the thing group.
module Network.AWS.IoT.RemoveThingFromThingGroup
  ( -- * Creating a Request
    RemoveThingFromThingGroup (..),
    newRemoveThingFromThingGroup,

    -- * Request Lenses
    removeThingFromThingGroup_thingArn,
    removeThingFromThingGroup_thingGroupArn,
    removeThingFromThingGroup_thingName,
    removeThingFromThingGroup_thingGroupName,

    -- * Destructuring the Response
    RemoveThingFromThingGroupResponse (..),
    newRemoveThingFromThingGroupResponse,

    -- * Response Lenses
    removeThingFromThingGroupResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newRemoveThingFromThingGroup' smart constructor.
data RemoveThingFromThingGroup = RemoveThingFromThingGroup'
  { -- | The ARN of the thing to remove from the group.
    thingArn :: Core.Maybe Core.Text,
    -- | The group ARN.
    thingGroupArn :: Core.Maybe Core.Text,
    -- | The name of the thing to remove from the group.
    thingName :: Core.Maybe Core.Text,
    -- | The group name.
    thingGroupName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RemoveThingFromThingGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'thingArn', 'removeThingFromThingGroup_thingArn' - The ARN of the thing to remove from the group.
--
-- 'thingGroupArn', 'removeThingFromThingGroup_thingGroupArn' - The group ARN.
--
-- 'thingName', 'removeThingFromThingGroup_thingName' - The name of the thing to remove from the group.
--
-- 'thingGroupName', 'removeThingFromThingGroup_thingGroupName' - The group name.
newRemoveThingFromThingGroup ::
  RemoveThingFromThingGroup
newRemoveThingFromThingGroup =
  RemoveThingFromThingGroup'
    { thingArn = Core.Nothing,
      thingGroupArn = Core.Nothing,
      thingName = Core.Nothing,
      thingGroupName = Core.Nothing
    }

-- | The ARN of the thing to remove from the group.
removeThingFromThingGroup_thingArn :: Lens.Lens' RemoveThingFromThingGroup (Core.Maybe Core.Text)
removeThingFromThingGroup_thingArn = Lens.lens (\RemoveThingFromThingGroup' {thingArn} -> thingArn) (\s@RemoveThingFromThingGroup' {} a -> s {thingArn = a} :: RemoveThingFromThingGroup)

-- | The group ARN.
removeThingFromThingGroup_thingGroupArn :: Lens.Lens' RemoveThingFromThingGroup (Core.Maybe Core.Text)
removeThingFromThingGroup_thingGroupArn = Lens.lens (\RemoveThingFromThingGroup' {thingGroupArn} -> thingGroupArn) (\s@RemoveThingFromThingGroup' {} a -> s {thingGroupArn = a} :: RemoveThingFromThingGroup)

-- | The name of the thing to remove from the group.
removeThingFromThingGroup_thingName :: Lens.Lens' RemoveThingFromThingGroup (Core.Maybe Core.Text)
removeThingFromThingGroup_thingName = Lens.lens (\RemoveThingFromThingGroup' {thingName} -> thingName) (\s@RemoveThingFromThingGroup' {} a -> s {thingName = a} :: RemoveThingFromThingGroup)

-- | The group name.
removeThingFromThingGroup_thingGroupName :: Lens.Lens' RemoveThingFromThingGroup (Core.Maybe Core.Text)
removeThingFromThingGroup_thingGroupName = Lens.lens (\RemoveThingFromThingGroup' {thingGroupName} -> thingGroupName) (\s@RemoveThingFromThingGroup' {} a -> s {thingGroupName = a} :: RemoveThingFromThingGroup)

instance Core.AWSRequest RemoveThingFromThingGroup where
  type
    AWSResponse RemoveThingFromThingGroup =
      RemoveThingFromThingGroupResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          RemoveThingFromThingGroupResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable RemoveThingFromThingGroup

instance Core.NFData RemoveThingFromThingGroup

instance Core.ToHeaders RemoveThingFromThingGroup where
  toHeaders = Core.const Core.mempty

instance Core.ToJSON RemoveThingFromThingGroup where
  toJSON RemoveThingFromThingGroup' {..} =
    Core.object
      ( Core.catMaybes
          [ ("thingArn" Core..=) Core.<$> thingArn,
            ("thingGroupArn" Core..=) Core.<$> thingGroupArn,
            ("thingName" Core..=) Core.<$> thingName,
            ("thingGroupName" Core..=) Core.<$> thingGroupName
          ]
      )

instance Core.ToPath RemoveThingFromThingGroup where
  toPath =
    Core.const
      "/thing-groups/removeThingFromThingGroup"

instance Core.ToQuery RemoveThingFromThingGroup where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newRemoveThingFromThingGroupResponse' smart constructor.
data RemoveThingFromThingGroupResponse = RemoveThingFromThingGroupResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RemoveThingFromThingGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'removeThingFromThingGroupResponse_httpStatus' - The response's http status code.
newRemoveThingFromThingGroupResponse ::
  -- | 'httpStatus'
  Core.Int ->
  RemoveThingFromThingGroupResponse
newRemoveThingFromThingGroupResponse pHttpStatus_ =
  RemoveThingFromThingGroupResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
removeThingFromThingGroupResponse_httpStatus :: Lens.Lens' RemoveThingFromThingGroupResponse Core.Int
removeThingFromThingGroupResponse_httpStatus = Lens.lens (\RemoveThingFromThingGroupResponse' {httpStatus} -> httpStatus) (\s@RemoveThingFromThingGroupResponse' {} a -> s {httpStatus = a} :: RemoveThingFromThingGroupResponse)

instance
  Core.NFData
    RemoveThingFromThingGroupResponse
