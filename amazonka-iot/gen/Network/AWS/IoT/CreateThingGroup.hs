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

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateThingGroup' smart constructor.
data CreateThingGroup = CreateThingGroup'
  { -- | The name of the parent thing group.
    parentGroupName :: Prelude.Maybe Prelude.Text,
    -- | Metadata which can be used to manage the thing group.
    tags :: Prelude.Maybe [Tag],
    -- | The thing group properties.
    thingGroupProperties :: Prelude.Maybe ThingGroupProperties,
    -- | The thing group name to create.
    thingGroupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  CreateThingGroup
newCreateThingGroup pThingGroupName_ =
  CreateThingGroup'
    { parentGroupName =
        Prelude.Nothing,
      tags = Prelude.Nothing,
      thingGroupProperties = Prelude.Nothing,
      thingGroupName = pThingGroupName_
    }

-- | The name of the parent thing group.
createThingGroup_parentGroupName :: Lens.Lens' CreateThingGroup (Prelude.Maybe Prelude.Text)
createThingGroup_parentGroupName = Lens.lens (\CreateThingGroup' {parentGroupName} -> parentGroupName) (\s@CreateThingGroup' {} a -> s {parentGroupName = a} :: CreateThingGroup)

-- | Metadata which can be used to manage the thing group.
createThingGroup_tags :: Lens.Lens' CreateThingGroup (Prelude.Maybe [Tag])
createThingGroup_tags = Lens.lens (\CreateThingGroup' {tags} -> tags) (\s@CreateThingGroup' {} a -> s {tags = a} :: CreateThingGroup) Prelude.. Lens.mapping Prelude._Coerce

-- | The thing group properties.
createThingGroup_thingGroupProperties :: Lens.Lens' CreateThingGroup (Prelude.Maybe ThingGroupProperties)
createThingGroup_thingGroupProperties = Lens.lens (\CreateThingGroup' {thingGroupProperties} -> thingGroupProperties) (\s@CreateThingGroup' {} a -> s {thingGroupProperties = a} :: CreateThingGroup)

-- | The thing group name to create.
createThingGroup_thingGroupName :: Lens.Lens' CreateThingGroup Prelude.Text
createThingGroup_thingGroupName = Lens.lens (\CreateThingGroup' {thingGroupName} -> thingGroupName) (\s@CreateThingGroup' {} a -> s {thingGroupName = a} :: CreateThingGroup)

instance Prelude.AWSRequest CreateThingGroup where
  type Rs CreateThingGroup = CreateThingGroupResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateThingGroupResponse'
            Prelude.<$> (x Prelude..?> "thingGroupArn")
            Prelude.<*> (x Prelude..?> "thingGroupName")
            Prelude.<*> (x Prelude..?> "thingGroupId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateThingGroup

instance Prelude.NFData CreateThingGroup

instance Prelude.ToHeaders CreateThingGroup where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToJSON CreateThingGroup where
  toJSON CreateThingGroup' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("parentGroupName" Prelude..=)
              Prelude.<$> parentGroupName,
            ("tags" Prelude..=) Prelude.<$> tags,
            ("thingGroupProperties" Prelude..=)
              Prelude.<$> thingGroupProperties
          ]
      )

instance Prelude.ToPath CreateThingGroup where
  toPath CreateThingGroup' {..} =
    Prelude.mconcat
      ["/thing-groups/", Prelude.toBS thingGroupName]

instance Prelude.ToQuery CreateThingGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateThingGroupResponse' smart constructor.
data CreateThingGroupResponse = CreateThingGroupResponse'
  { -- | The thing group ARN.
    thingGroupArn :: Prelude.Maybe Prelude.Text,
    -- | The thing group name.
    thingGroupName :: Prelude.Maybe Prelude.Text,
    -- | The thing group ID.
    thingGroupId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  CreateThingGroupResponse
newCreateThingGroupResponse pHttpStatus_ =
  CreateThingGroupResponse'
    { thingGroupArn =
        Prelude.Nothing,
      thingGroupName = Prelude.Nothing,
      thingGroupId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The thing group ARN.
createThingGroupResponse_thingGroupArn :: Lens.Lens' CreateThingGroupResponse (Prelude.Maybe Prelude.Text)
createThingGroupResponse_thingGroupArn = Lens.lens (\CreateThingGroupResponse' {thingGroupArn} -> thingGroupArn) (\s@CreateThingGroupResponse' {} a -> s {thingGroupArn = a} :: CreateThingGroupResponse)

-- | The thing group name.
createThingGroupResponse_thingGroupName :: Lens.Lens' CreateThingGroupResponse (Prelude.Maybe Prelude.Text)
createThingGroupResponse_thingGroupName = Lens.lens (\CreateThingGroupResponse' {thingGroupName} -> thingGroupName) (\s@CreateThingGroupResponse' {} a -> s {thingGroupName = a} :: CreateThingGroupResponse)

-- | The thing group ID.
createThingGroupResponse_thingGroupId :: Lens.Lens' CreateThingGroupResponse (Prelude.Maybe Prelude.Text)
createThingGroupResponse_thingGroupId = Lens.lens (\CreateThingGroupResponse' {thingGroupId} -> thingGroupId) (\s@CreateThingGroupResponse' {} a -> s {thingGroupId = a} :: CreateThingGroupResponse)

-- | The response's http status code.
createThingGroupResponse_httpStatus :: Lens.Lens' CreateThingGroupResponse Prelude.Int
createThingGroupResponse_httpStatus = Lens.lens (\CreateThingGroupResponse' {httpStatus} -> httpStatus) (\s@CreateThingGroupResponse' {} a -> s {httpStatus = a} :: CreateThingGroupResponse)

instance Prelude.NFData CreateThingGroupResponse
