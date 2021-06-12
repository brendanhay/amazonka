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
-- Module      : Network.AWS.IoT.CreateThingGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create a thing group.
--
-- This is a control plane operation. See
-- <https://docs.aws.amazon.com/iot/latest/developerguide/iot-authorization.html Authorization>
-- for information about authorizing control plane actions.
module Network.AWS.IoT.CreateThingGroup
  ( -- * Creating a Request
    CreateThingGroup (..),
    newCreateThingGroup,

    -- * Request Lenses
    createThingGroup_parentGroupName,
    createThingGroup_tags,
    createThingGroup_thingGroupProperties,
    createThingGroup_thingGroupName,

    -- * Destructuring the Response
    CreateThingGroupResponse (..),
    newCreateThingGroupResponse,

    -- * Response Lenses
    createThingGroupResponse_thingGroupArn,
    createThingGroupResponse_thingGroupName,
    createThingGroupResponse_thingGroupId,
    createThingGroupResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateThingGroup' smart constructor.
data CreateThingGroup = CreateThingGroup'
  { -- | The name of the parent thing group.
    parentGroupName :: Core.Maybe Core.Text,
    -- | Metadata which can be used to manage the thing group.
    tags :: Core.Maybe [Tag],
    -- | The thing group properties.
    thingGroupProperties :: Core.Maybe ThingGroupProperties,
    -- | The thing group name to create.
    thingGroupName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateThingGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'parentGroupName', 'createThingGroup_parentGroupName' - The name of the parent thing group.
--
-- 'tags', 'createThingGroup_tags' - Metadata which can be used to manage the thing group.
--
-- 'thingGroupProperties', 'createThingGroup_thingGroupProperties' - The thing group properties.
--
-- 'thingGroupName', 'createThingGroup_thingGroupName' - The thing group name to create.
newCreateThingGroup ::
  -- | 'thingGroupName'
  Core.Text ->
  CreateThingGroup
newCreateThingGroup pThingGroupName_ =
  CreateThingGroup'
    { parentGroupName = Core.Nothing,
      tags = Core.Nothing,
      thingGroupProperties = Core.Nothing,
      thingGroupName = pThingGroupName_
    }

-- | The name of the parent thing group.
createThingGroup_parentGroupName :: Lens.Lens' CreateThingGroup (Core.Maybe Core.Text)
createThingGroup_parentGroupName = Lens.lens (\CreateThingGroup' {parentGroupName} -> parentGroupName) (\s@CreateThingGroup' {} a -> s {parentGroupName = a} :: CreateThingGroup)

-- | Metadata which can be used to manage the thing group.
createThingGroup_tags :: Lens.Lens' CreateThingGroup (Core.Maybe [Tag])
createThingGroup_tags = Lens.lens (\CreateThingGroup' {tags} -> tags) (\s@CreateThingGroup' {} a -> s {tags = a} :: CreateThingGroup) Core.. Lens.mapping Lens._Coerce

-- | The thing group properties.
createThingGroup_thingGroupProperties :: Lens.Lens' CreateThingGroup (Core.Maybe ThingGroupProperties)
createThingGroup_thingGroupProperties = Lens.lens (\CreateThingGroup' {thingGroupProperties} -> thingGroupProperties) (\s@CreateThingGroup' {} a -> s {thingGroupProperties = a} :: CreateThingGroup)

-- | The thing group name to create.
createThingGroup_thingGroupName :: Lens.Lens' CreateThingGroup Core.Text
createThingGroup_thingGroupName = Lens.lens (\CreateThingGroup' {thingGroupName} -> thingGroupName) (\s@CreateThingGroup' {} a -> s {thingGroupName = a} :: CreateThingGroup)

instance Core.AWSRequest CreateThingGroup where
  type
    AWSResponse CreateThingGroup =
      CreateThingGroupResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateThingGroupResponse'
            Core.<$> (x Core..?> "thingGroupArn")
            Core.<*> (x Core..?> "thingGroupName")
            Core.<*> (x Core..?> "thingGroupId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateThingGroup

instance Core.NFData CreateThingGroup

instance Core.ToHeaders CreateThingGroup where
  toHeaders = Core.const Core.mempty

instance Core.ToJSON CreateThingGroup where
  toJSON CreateThingGroup' {..} =
    Core.object
      ( Core.catMaybes
          [ ("parentGroupName" Core..=)
              Core.<$> parentGroupName,
            ("tags" Core..=) Core.<$> tags,
            ("thingGroupProperties" Core..=)
              Core.<$> thingGroupProperties
          ]
      )

instance Core.ToPath CreateThingGroup where
  toPath CreateThingGroup' {..} =
    Core.mconcat
      ["/thing-groups/", Core.toBS thingGroupName]

instance Core.ToQuery CreateThingGroup where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateThingGroupResponse' smart constructor.
data CreateThingGroupResponse = CreateThingGroupResponse'
  { -- | The thing group ARN.
    thingGroupArn :: Core.Maybe Core.Text,
    -- | The thing group name.
    thingGroupName :: Core.Maybe Core.Text,
    -- | The thing group ID.
    thingGroupId :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateThingGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'thingGroupArn', 'createThingGroupResponse_thingGroupArn' - The thing group ARN.
--
-- 'thingGroupName', 'createThingGroupResponse_thingGroupName' - The thing group name.
--
-- 'thingGroupId', 'createThingGroupResponse_thingGroupId' - The thing group ID.
--
-- 'httpStatus', 'createThingGroupResponse_httpStatus' - The response's http status code.
newCreateThingGroupResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateThingGroupResponse
newCreateThingGroupResponse pHttpStatus_ =
  CreateThingGroupResponse'
    { thingGroupArn =
        Core.Nothing,
      thingGroupName = Core.Nothing,
      thingGroupId = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The thing group ARN.
createThingGroupResponse_thingGroupArn :: Lens.Lens' CreateThingGroupResponse (Core.Maybe Core.Text)
createThingGroupResponse_thingGroupArn = Lens.lens (\CreateThingGroupResponse' {thingGroupArn} -> thingGroupArn) (\s@CreateThingGroupResponse' {} a -> s {thingGroupArn = a} :: CreateThingGroupResponse)

-- | The thing group name.
createThingGroupResponse_thingGroupName :: Lens.Lens' CreateThingGroupResponse (Core.Maybe Core.Text)
createThingGroupResponse_thingGroupName = Lens.lens (\CreateThingGroupResponse' {thingGroupName} -> thingGroupName) (\s@CreateThingGroupResponse' {} a -> s {thingGroupName = a} :: CreateThingGroupResponse)

-- | The thing group ID.
createThingGroupResponse_thingGroupId :: Lens.Lens' CreateThingGroupResponse (Core.Maybe Core.Text)
createThingGroupResponse_thingGroupId = Lens.lens (\CreateThingGroupResponse' {thingGroupId} -> thingGroupId) (\s@CreateThingGroupResponse' {} a -> s {thingGroupId = a} :: CreateThingGroupResponse)

-- | The response's http status code.
createThingGroupResponse_httpStatus :: Lens.Lens' CreateThingGroupResponse Core.Int
createThingGroupResponse_httpStatus = Lens.lens (\CreateThingGroupResponse' {httpStatus} -> httpStatus) (\s@CreateThingGroupResponse' {} a -> s {httpStatus = a} :: CreateThingGroupResponse)

instance Core.NFData CreateThingGroupResponse
