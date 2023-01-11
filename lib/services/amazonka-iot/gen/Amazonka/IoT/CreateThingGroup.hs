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
-- Module      : Amazonka.IoT.CreateThingGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
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
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions CreateThingGroup>
-- action.
module Amazonka.IoT.CreateThingGroup
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
    createThingGroupResponse_thingGroupId,
    createThingGroupResponse_thingGroupName,
    createThingGroupResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
createThingGroup_tags = Lens.lens (\CreateThingGroup' {tags} -> tags) (\s@CreateThingGroup' {} a -> s {tags = a} :: CreateThingGroup) Prelude.. Lens.mapping Lens.coerced

-- | The thing group properties.
createThingGroup_thingGroupProperties :: Lens.Lens' CreateThingGroup (Prelude.Maybe ThingGroupProperties)
createThingGroup_thingGroupProperties = Lens.lens (\CreateThingGroup' {thingGroupProperties} -> thingGroupProperties) (\s@CreateThingGroup' {} a -> s {thingGroupProperties = a} :: CreateThingGroup)

-- | The thing group name to create.
createThingGroup_thingGroupName :: Lens.Lens' CreateThingGroup Prelude.Text
createThingGroup_thingGroupName = Lens.lens (\CreateThingGroup' {thingGroupName} -> thingGroupName) (\s@CreateThingGroup' {} a -> s {thingGroupName = a} :: CreateThingGroup)

instance Core.AWSRequest CreateThingGroup where
  type
    AWSResponse CreateThingGroup =
      CreateThingGroupResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateThingGroupResponse'
            Prelude.<$> (x Data..?> "thingGroupArn")
            Prelude.<*> (x Data..?> "thingGroupId")
            Prelude.<*> (x Data..?> "thingGroupName")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateThingGroup where
  hashWithSalt _salt CreateThingGroup' {..} =
    _salt `Prelude.hashWithSalt` parentGroupName
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` thingGroupProperties
      `Prelude.hashWithSalt` thingGroupName

instance Prelude.NFData CreateThingGroup where
  rnf CreateThingGroup' {..} =
    Prelude.rnf parentGroupName
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf thingGroupProperties
      `Prelude.seq` Prelude.rnf thingGroupName

instance Data.ToHeaders CreateThingGroup where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON CreateThingGroup where
  toJSON CreateThingGroup' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("parentGroupName" Data..=)
              Prelude.<$> parentGroupName,
            ("tags" Data..=) Prelude.<$> tags,
            ("thingGroupProperties" Data..=)
              Prelude.<$> thingGroupProperties
          ]
      )

instance Data.ToPath CreateThingGroup where
  toPath CreateThingGroup' {..} =
    Prelude.mconcat
      ["/thing-groups/", Data.toBS thingGroupName]

instance Data.ToQuery CreateThingGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateThingGroupResponse' smart constructor.
data CreateThingGroupResponse = CreateThingGroupResponse'
  { -- | The thing group ARN.
    thingGroupArn :: Prelude.Maybe Prelude.Text,
    -- | The thing group ID.
    thingGroupId :: Prelude.Maybe Prelude.Text,
    -- | The thing group name.
    thingGroupName :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'thingGroupId', 'createThingGroupResponse_thingGroupId' - The thing group ID.
--
-- 'thingGroupName', 'createThingGroupResponse_thingGroupName' - The thing group name.
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
      thingGroupId = Prelude.Nothing,
      thingGroupName = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The thing group ARN.
createThingGroupResponse_thingGroupArn :: Lens.Lens' CreateThingGroupResponse (Prelude.Maybe Prelude.Text)
createThingGroupResponse_thingGroupArn = Lens.lens (\CreateThingGroupResponse' {thingGroupArn} -> thingGroupArn) (\s@CreateThingGroupResponse' {} a -> s {thingGroupArn = a} :: CreateThingGroupResponse)

-- | The thing group ID.
createThingGroupResponse_thingGroupId :: Lens.Lens' CreateThingGroupResponse (Prelude.Maybe Prelude.Text)
createThingGroupResponse_thingGroupId = Lens.lens (\CreateThingGroupResponse' {thingGroupId} -> thingGroupId) (\s@CreateThingGroupResponse' {} a -> s {thingGroupId = a} :: CreateThingGroupResponse)

-- | The thing group name.
createThingGroupResponse_thingGroupName :: Lens.Lens' CreateThingGroupResponse (Prelude.Maybe Prelude.Text)
createThingGroupResponse_thingGroupName = Lens.lens (\CreateThingGroupResponse' {thingGroupName} -> thingGroupName) (\s@CreateThingGroupResponse' {} a -> s {thingGroupName = a} :: CreateThingGroupResponse)

-- | The response's http status code.
createThingGroupResponse_httpStatus :: Lens.Lens' CreateThingGroupResponse Prelude.Int
createThingGroupResponse_httpStatus = Lens.lens (\CreateThingGroupResponse' {httpStatus} -> httpStatus) (\s@CreateThingGroupResponse' {} a -> s {httpStatus = a} :: CreateThingGroupResponse)

instance Prelude.NFData CreateThingGroupResponse where
  rnf CreateThingGroupResponse' {..} =
    Prelude.rnf thingGroupArn
      `Prelude.seq` Prelude.rnf thingGroupId
      `Prelude.seq` Prelude.rnf thingGroupName
      `Prelude.seq` Prelude.rnf httpStatus
