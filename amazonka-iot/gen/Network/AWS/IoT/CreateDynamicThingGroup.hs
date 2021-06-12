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
-- Module      : Network.AWS.IoT.CreateDynamicThingGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a dynamic thing group.
module Network.AWS.IoT.CreateDynamicThingGroup
  ( -- * Creating a Request
    CreateDynamicThingGroup (..),
    newCreateDynamicThingGroup,

    -- * Request Lenses
    createDynamicThingGroup_indexName,
    createDynamicThingGroup_queryVersion,
    createDynamicThingGroup_tags,
    createDynamicThingGroup_thingGroupProperties,
    createDynamicThingGroup_thingGroupName,
    createDynamicThingGroup_queryString,

    -- * Destructuring the Response
    CreateDynamicThingGroupResponse (..),
    newCreateDynamicThingGroupResponse,

    -- * Response Lenses
    createDynamicThingGroupResponse_queryString,
    createDynamicThingGroupResponse_indexName,
    createDynamicThingGroupResponse_thingGroupArn,
    createDynamicThingGroupResponse_queryVersion,
    createDynamicThingGroupResponse_thingGroupName,
    createDynamicThingGroupResponse_thingGroupId,
    createDynamicThingGroupResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateDynamicThingGroup' smart constructor.
data CreateDynamicThingGroup = CreateDynamicThingGroup'
  { -- | The dynamic thing group index name.
    --
    -- Currently one index is supported: \"AWS_Things\".
    indexName :: Core.Maybe Core.Text,
    -- | The dynamic thing group query version.
    --
    -- Currently one query version is supported: \"2017-09-30\". If not
    -- specified, the query version defaults to this value.
    queryVersion :: Core.Maybe Core.Text,
    -- | Metadata which can be used to manage the dynamic thing group.
    tags :: Core.Maybe [Tag],
    -- | The dynamic thing group properties.
    thingGroupProperties :: Core.Maybe ThingGroupProperties,
    -- | The dynamic thing group name to create.
    thingGroupName :: Core.Text,
    -- | The dynamic thing group search query string.
    --
    -- See
    -- <https://docs.aws.amazon.com/iot/latest/developerguide/query-syntax.html Query Syntax>
    -- for information about query string syntax.
    queryString :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateDynamicThingGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'indexName', 'createDynamicThingGroup_indexName' - The dynamic thing group index name.
--
-- Currently one index is supported: \"AWS_Things\".
--
-- 'queryVersion', 'createDynamicThingGroup_queryVersion' - The dynamic thing group query version.
--
-- Currently one query version is supported: \"2017-09-30\". If not
-- specified, the query version defaults to this value.
--
-- 'tags', 'createDynamicThingGroup_tags' - Metadata which can be used to manage the dynamic thing group.
--
-- 'thingGroupProperties', 'createDynamicThingGroup_thingGroupProperties' - The dynamic thing group properties.
--
-- 'thingGroupName', 'createDynamicThingGroup_thingGroupName' - The dynamic thing group name to create.
--
-- 'queryString', 'createDynamicThingGroup_queryString' - The dynamic thing group search query string.
--
-- See
-- <https://docs.aws.amazon.com/iot/latest/developerguide/query-syntax.html Query Syntax>
-- for information about query string syntax.
newCreateDynamicThingGroup ::
  -- | 'thingGroupName'
  Core.Text ->
  -- | 'queryString'
  Core.Text ->
  CreateDynamicThingGroup
newCreateDynamicThingGroup
  pThingGroupName_
  pQueryString_ =
    CreateDynamicThingGroup'
      { indexName = Core.Nothing,
        queryVersion = Core.Nothing,
        tags = Core.Nothing,
        thingGroupProperties = Core.Nothing,
        thingGroupName = pThingGroupName_,
        queryString = pQueryString_
      }

-- | The dynamic thing group index name.
--
-- Currently one index is supported: \"AWS_Things\".
createDynamicThingGroup_indexName :: Lens.Lens' CreateDynamicThingGroup (Core.Maybe Core.Text)
createDynamicThingGroup_indexName = Lens.lens (\CreateDynamicThingGroup' {indexName} -> indexName) (\s@CreateDynamicThingGroup' {} a -> s {indexName = a} :: CreateDynamicThingGroup)

-- | The dynamic thing group query version.
--
-- Currently one query version is supported: \"2017-09-30\". If not
-- specified, the query version defaults to this value.
createDynamicThingGroup_queryVersion :: Lens.Lens' CreateDynamicThingGroup (Core.Maybe Core.Text)
createDynamicThingGroup_queryVersion = Lens.lens (\CreateDynamicThingGroup' {queryVersion} -> queryVersion) (\s@CreateDynamicThingGroup' {} a -> s {queryVersion = a} :: CreateDynamicThingGroup)

-- | Metadata which can be used to manage the dynamic thing group.
createDynamicThingGroup_tags :: Lens.Lens' CreateDynamicThingGroup (Core.Maybe [Tag])
createDynamicThingGroup_tags = Lens.lens (\CreateDynamicThingGroup' {tags} -> tags) (\s@CreateDynamicThingGroup' {} a -> s {tags = a} :: CreateDynamicThingGroup) Core.. Lens.mapping Lens._Coerce

-- | The dynamic thing group properties.
createDynamicThingGroup_thingGroupProperties :: Lens.Lens' CreateDynamicThingGroup (Core.Maybe ThingGroupProperties)
createDynamicThingGroup_thingGroupProperties = Lens.lens (\CreateDynamicThingGroup' {thingGroupProperties} -> thingGroupProperties) (\s@CreateDynamicThingGroup' {} a -> s {thingGroupProperties = a} :: CreateDynamicThingGroup)

-- | The dynamic thing group name to create.
createDynamicThingGroup_thingGroupName :: Lens.Lens' CreateDynamicThingGroup Core.Text
createDynamicThingGroup_thingGroupName = Lens.lens (\CreateDynamicThingGroup' {thingGroupName} -> thingGroupName) (\s@CreateDynamicThingGroup' {} a -> s {thingGroupName = a} :: CreateDynamicThingGroup)

-- | The dynamic thing group search query string.
--
-- See
-- <https://docs.aws.amazon.com/iot/latest/developerguide/query-syntax.html Query Syntax>
-- for information about query string syntax.
createDynamicThingGroup_queryString :: Lens.Lens' CreateDynamicThingGroup Core.Text
createDynamicThingGroup_queryString = Lens.lens (\CreateDynamicThingGroup' {queryString} -> queryString) (\s@CreateDynamicThingGroup' {} a -> s {queryString = a} :: CreateDynamicThingGroup)

instance Core.AWSRequest CreateDynamicThingGroup where
  type
    AWSResponse CreateDynamicThingGroup =
      CreateDynamicThingGroupResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateDynamicThingGroupResponse'
            Core.<$> (x Core..?> "queryString")
            Core.<*> (x Core..?> "indexName")
            Core.<*> (x Core..?> "thingGroupArn")
            Core.<*> (x Core..?> "queryVersion")
            Core.<*> (x Core..?> "thingGroupName")
            Core.<*> (x Core..?> "thingGroupId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateDynamicThingGroup

instance Core.NFData CreateDynamicThingGroup

instance Core.ToHeaders CreateDynamicThingGroup where
  toHeaders = Core.const Core.mempty

instance Core.ToJSON CreateDynamicThingGroup where
  toJSON CreateDynamicThingGroup' {..} =
    Core.object
      ( Core.catMaybes
          [ ("indexName" Core..=) Core.<$> indexName,
            ("queryVersion" Core..=) Core.<$> queryVersion,
            ("tags" Core..=) Core.<$> tags,
            ("thingGroupProperties" Core..=)
              Core.<$> thingGroupProperties,
            Core.Just ("queryString" Core..= queryString)
          ]
      )

instance Core.ToPath CreateDynamicThingGroup where
  toPath CreateDynamicThingGroup' {..} =
    Core.mconcat
      ["/dynamic-thing-groups/", Core.toBS thingGroupName]

instance Core.ToQuery CreateDynamicThingGroup where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateDynamicThingGroupResponse' smart constructor.
data CreateDynamicThingGroupResponse = CreateDynamicThingGroupResponse'
  { -- | The dynamic thing group search query string.
    queryString :: Core.Maybe Core.Text,
    -- | The dynamic thing group index name.
    indexName :: Core.Maybe Core.Text,
    -- | The dynamic thing group ARN.
    thingGroupArn :: Core.Maybe Core.Text,
    -- | The dynamic thing group query version.
    queryVersion :: Core.Maybe Core.Text,
    -- | The dynamic thing group name.
    thingGroupName :: Core.Maybe Core.Text,
    -- | The dynamic thing group ID.
    thingGroupId :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateDynamicThingGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'queryString', 'createDynamicThingGroupResponse_queryString' - The dynamic thing group search query string.
--
-- 'indexName', 'createDynamicThingGroupResponse_indexName' - The dynamic thing group index name.
--
-- 'thingGroupArn', 'createDynamicThingGroupResponse_thingGroupArn' - The dynamic thing group ARN.
--
-- 'queryVersion', 'createDynamicThingGroupResponse_queryVersion' - The dynamic thing group query version.
--
-- 'thingGroupName', 'createDynamicThingGroupResponse_thingGroupName' - The dynamic thing group name.
--
-- 'thingGroupId', 'createDynamicThingGroupResponse_thingGroupId' - The dynamic thing group ID.
--
-- 'httpStatus', 'createDynamicThingGroupResponse_httpStatus' - The response's http status code.
newCreateDynamicThingGroupResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateDynamicThingGroupResponse
newCreateDynamicThingGroupResponse pHttpStatus_ =
  CreateDynamicThingGroupResponse'
    { queryString =
        Core.Nothing,
      indexName = Core.Nothing,
      thingGroupArn = Core.Nothing,
      queryVersion = Core.Nothing,
      thingGroupName = Core.Nothing,
      thingGroupId = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The dynamic thing group search query string.
createDynamicThingGroupResponse_queryString :: Lens.Lens' CreateDynamicThingGroupResponse (Core.Maybe Core.Text)
createDynamicThingGroupResponse_queryString = Lens.lens (\CreateDynamicThingGroupResponse' {queryString} -> queryString) (\s@CreateDynamicThingGroupResponse' {} a -> s {queryString = a} :: CreateDynamicThingGroupResponse)

-- | The dynamic thing group index name.
createDynamicThingGroupResponse_indexName :: Lens.Lens' CreateDynamicThingGroupResponse (Core.Maybe Core.Text)
createDynamicThingGroupResponse_indexName = Lens.lens (\CreateDynamicThingGroupResponse' {indexName} -> indexName) (\s@CreateDynamicThingGroupResponse' {} a -> s {indexName = a} :: CreateDynamicThingGroupResponse)

-- | The dynamic thing group ARN.
createDynamicThingGroupResponse_thingGroupArn :: Lens.Lens' CreateDynamicThingGroupResponse (Core.Maybe Core.Text)
createDynamicThingGroupResponse_thingGroupArn = Lens.lens (\CreateDynamicThingGroupResponse' {thingGroupArn} -> thingGroupArn) (\s@CreateDynamicThingGroupResponse' {} a -> s {thingGroupArn = a} :: CreateDynamicThingGroupResponse)

-- | The dynamic thing group query version.
createDynamicThingGroupResponse_queryVersion :: Lens.Lens' CreateDynamicThingGroupResponse (Core.Maybe Core.Text)
createDynamicThingGroupResponse_queryVersion = Lens.lens (\CreateDynamicThingGroupResponse' {queryVersion} -> queryVersion) (\s@CreateDynamicThingGroupResponse' {} a -> s {queryVersion = a} :: CreateDynamicThingGroupResponse)

-- | The dynamic thing group name.
createDynamicThingGroupResponse_thingGroupName :: Lens.Lens' CreateDynamicThingGroupResponse (Core.Maybe Core.Text)
createDynamicThingGroupResponse_thingGroupName = Lens.lens (\CreateDynamicThingGroupResponse' {thingGroupName} -> thingGroupName) (\s@CreateDynamicThingGroupResponse' {} a -> s {thingGroupName = a} :: CreateDynamicThingGroupResponse)

-- | The dynamic thing group ID.
createDynamicThingGroupResponse_thingGroupId :: Lens.Lens' CreateDynamicThingGroupResponse (Core.Maybe Core.Text)
createDynamicThingGroupResponse_thingGroupId = Lens.lens (\CreateDynamicThingGroupResponse' {thingGroupId} -> thingGroupId) (\s@CreateDynamicThingGroupResponse' {} a -> s {thingGroupId = a} :: CreateDynamicThingGroupResponse)

-- | The response's http status code.
createDynamicThingGroupResponse_httpStatus :: Lens.Lens' CreateDynamicThingGroupResponse Core.Int
createDynamicThingGroupResponse_httpStatus = Lens.lens (\CreateDynamicThingGroupResponse' {httpStatus} -> httpStatus) (\s@CreateDynamicThingGroupResponse' {} a -> s {httpStatus = a} :: CreateDynamicThingGroupResponse)

instance Core.NFData CreateDynamicThingGroupResponse
