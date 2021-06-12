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
-- Module      : Network.AWS.ResourceGroups.CreateGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a resource group with the specified name and description. You
-- can optionally include a resource query, or a service configuration. For
-- more information about constructing a resource query, see
-- <https://docs.aws.amazon.com/ARG/latest/userguide/gettingstarted-query.html#gettingstarted-query-cli-tag Create a tag-based group in Resource Groups>.
-- For more information about service configurations, see
-- <https://docs.aws.amazon.com/ARG/latest/APIReference/about-slg.html Service configurations for resource groups>.
--
-- __Minimum permissions__
--
-- To run this command, you must have the following permissions:
--
-- -   @resource-groups:CreateGroup@
module Network.AWS.ResourceGroups.CreateGroup
  ( -- * Creating a Request
    CreateGroup (..),
    newCreateGroup,

    -- * Request Lenses
    createGroup_configuration,
    createGroup_tags,
    createGroup_description,
    createGroup_resourceQuery,
    createGroup_name,

    -- * Destructuring the Response
    CreateGroupResponse (..),
    newCreateGroupResponse,

    -- * Response Lenses
    createGroupResponse_groupConfiguration,
    createGroupResponse_group,
    createGroupResponse_tags,
    createGroupResponse_resourceQuery,
    createGroupResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import Network.AWS.ResourceGroups.Types
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateGroup' smart constructor.
data CreateGroup = CreateGroup'
  { -- | A configuration associates the resource group with an AWS service and
    -- specifies how the service can interact with the resources in the group.
    -- A configuration is an array of GroupConfigurationItem elements. For
    -- details about the syntax of service configurations, see
    -- <https://docs.aws.amazon.com/ARG/latest/APIReference/about-slg.html Service configurations for resource groups>.
    --
    -- A resource group can contain either a @Configuration@ or a
    -- @ResourceQuery@, but not both.
    configuration :: Core.Maybe [GroupConfigurationItem],
    -- | The tags to add to the group. A tag is key-value pair string.
    tags :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The description of the resource group. Descriptions can consist of
    -- letters, numbers, hyphens, underscores, periods, and spaces.
    description :: Core.Maybe Core.Text,
    -- | The resource query that determines which AWS resources are members of
    -- this group. For more information about resource queries, see
    -- <https://docs.aws.amazon.com/ARG/latest/userguide/gettingstarted-query.html#gettingstarted-query-cli-tag Create a tag-based group in Resource Groups>.
    --
    -- A resource group can contain either a @ResourceQuery@ or a
    -- @Configuration@, but not both.
    resourceQuery :: Core.Maybe ResourceQuery,
    -- | The name of the group, which is the identifier of the group in other
    -- operations. You can\'t change the name of a resource group after you
    -- create it. A resource group name can consist of letters, numbers,
    -- hyphens, periods, and underscores. The name cannot start with @AWS@ or
    -- @aws@; these are reserved. A resource group name must be unique within
    -- each AWS Region in your AWS account.
    name :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configuration', 'createGroup_configuration' - A configuration associates the resource group with an AWS service and
-- specifies how the service can interact with the resources in the group.
-- A configuration is an array of GroupConfigurationItem elements. For
-- details about the syntax of service configurations, see
-- <https://docs.aws.amazon.com/ARG/latest/APIReference/about-slg.html Service configurations for resource groups>.
--
-- A resource group can contain either a @Configuration@ or a
-- @ResourceQuery@, but not both.
--
-- 'tags', 'createGroup_tags' - The tags to add to the group. A tag is key-value pair string.
--
-- 'description', 'createGroup_description' - The description of the resource group. Descriptions can consist of
-- letters, numbers, hyphens, underscores, periods, and spaces.
--
-- 'resourceQuery', 'createGroup_resourceQuery' - The resource query that determines which AWS resources are members of
-- this group. For more information about resource queries, see
-- <https://docs.aws.amazon.com/ARG/latest/userguide/gettingstarted-query.html#gettingstarted-query-cli-tag Create a tag-based group in Resource Groups>.
--
-- A resource group can contain either a @ResourceQuery@ or a
-- @Configuration@, but not both.
--
-- 'name', 'createGroup_name' - The name of the group, which is the identifier of the group in other
-- operations. You can\'t change the name of a resource group after you
-- create it. A resource group name can consist of letters, numbers,
-- hyphens, periods, and underscores. The name cannot start with @AWS@ or
-- @aws@; these are reserved. A resource group name must be unique within
-- each AWS Region in your AWS account.
newCreateGroup ::
  -- | 'name'
  Core.Text ->
  CreateGroup
newCreateGroup pName_ =
  CreateGroup'
    { configuration = Core.Nothing,
      tags = Core.Nothing,
      description = Core.Nothing,
      resourceQuery = Core.Nothing,
      name = pName_
    }

-- | A configuration associates the resource group with an AWS service and
-- specifies how the service can interact with the resources in the group.
-- A configuration is an array of GroupConfigurationItem elements. For
-- details about the syntax of service configurations, see
-- <https://docs.aws.amazon.com/ARG/latest/APIReference/about-slg.html Service configurations for resource groups>.
--
-- A resource group can contain either a @Configuration@ or a
-- @ResourceQuery@, but not both.
createGroup_configuration :: Lens.Lens' CreateGroup (Core.Maybe [GroupConfigurationItem])
createGroup_configuration = Lens.lens (\CreateGroup' {configuration} -> configuration) (\s@CreateGroup' {} a -> s {configuration = a} :: CreateGroup) Core.. Lens.mapping Lens._Coerce

-- | The tags to add to the group. A tag is key-value pair string.
createGroup_tags :: Lens.Lens' CreateGroup (Core.Maybe (Core.HashMap Core.Text Core.Text))
createGroup_tags = Lens.lens (\CreateGroup' {tags} -> tags) (\s@CreateGroup' {} a -> s {tags = a} :: CreateGroup) Core.. Lens.mapping Lens._Coerce

-- | The description of the resource group. Descriptions can consist of
-- letters, numbers, hyphens, underscores, periods, and spaces.
createGroup_description :: Lens.Lens' CreateGroup (Core.Maybe Core.Text)
createGroup_description = Lens.lens (\CreateGroup' {description} -> description) (\s@CreateGroup' {} a -> s {description = a} :: CreateGroup)

-- | The resource query that determines which AWS resources are members of
-- this group. For more information about resource queries, see
-- <https://docs.aws.amazon.com/ARG/latest/userguide/gettingstarted-query.html#gettingstarted-query-cli-tag Create a tag-based group in Resource Groups>.
--
-- A resource group can contain either a @ResourceQuery@ or a
-- @Configuration@, but not both.
createGroup_resourceQuery :: Lens.Lens' CreateGroup (Core.Maybe ResourceQuery)
createGroup_resourceQuery = Lens.lens (\CreateGroup' {resourceQuery} -> resourceQuery) (\s@CreateGroup' {} a -> s {resourceQuery = a} :: CreateGroup)

-- | The name of the group, which is the identifier of the group in other
-- operations. You can\'t change the name of a resource group after you
-- create it. A resource group name can consist of letters, numbers,
-- hyphens, periods, and underscores. The name cannot start with @AWS@ or
-- @aws@; these are reserved. A resource group name must be unique within
-- each AWS Region in your AWS account.
createGroup_name :: Lens.Lens' CreateGroup Core.Text
createGroup_name = Lens.lens (\CreateGroup' {name} -> name) (\s@CreateGroup' {} a -> s {name = a} :: CreateGroup)

instance Core.AWSRequest CreateGroup where
  type AWSResponse CreateGroup = CreateGroupResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateGroupResponse'
            Core.<$> (x Core..?> "GroupConfiguration")
            Core.<*> (x Core..?> "Group")
            Core.<*> (x Core..?> "Tags" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "ResourceQuery")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateGroup

instance Core.NFData CreateGroup

instance Core.ToHeaders CreateGroup where
  toHeaders = Core.const Core.mempty

instance Core.ToJSON CreateGroup where
  toJSON CreateGroup' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Configuration" Core..=) Core.<$> configuration,
            ("Tags" Core..=) Core.<$> tags,
            ("Description" Core..=) Core.<$> description,
            ("ResourceQuery" Core..=) Core.<$> resourceQuery,
            Core.Just ("Name" Core..= name)
          ]
      )

instance Core.ToPath CreateGroup where
  toPath = Core.const "/groups"

instance Core.ToQuery CreateGroup where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateGroupResponse' smart constructor.
data CreateGroupResponse = CreateGroupResponse'
  { -- | The service configuration associated with the resource group. For
    -- details about the syntax of a service configuration, see
    -- <https://docs.aws.amazon.com/ARG/latest/APIReference/about-slg.html Service configurations for resource groups>.
    groupConfiguration :: Core.Maybe GroupConfiguration,
    -- | The description of the resource group.
    group' :: Core.Maybe Group,
    -- | The tags associated with the group.
    tags :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The resource query associated with the group. For more information about
    -- resource queries, see
    -- <https://docs.aws.amazon.com/ARG/latest/userguide/gettingstarted-query.html#gettingstarted-query-cli-tag Create a tag-based group in Resource Groups>.
    resourceQuery :: Core.Maybe ResourceQuery,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groupConfiguration', 'createGroupResponse_groupConfiguration' - The service configuration associated with the resource group. For
-- details about the syntax of a service configuration, see
-- <https://docs.aws.amazon.com/ARG/latest/APIReference/about-slg.html Service configurations for resource groups>.
--
-- 'group'', 'createGroupResponse_group' - The description of the resource group.
--
-- 'tags', 'createGroupResponse_tags' - The tags associated with the group.
--
-- 'resourceQuery', 'createGroupResponse_resourceQuery' - The resource query associated with the group. For more information about
-- resource queries, see
-- <https://docs.aws.amazon.com/ARG/latest/userguide/gettingstarted-query.html#gettingstarted-query-cli-tag Create a tag-based group in Resource Groups>.
--
-- 'httpStatus', 'createGroupResponse_httpStatus' - The response's http status code.
newCreateGroupResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateGroupResponse
newCreateGroupResponse pHttpStatus_ =
  CreateGroupResponse'
    { groupConfiguration =
        Core.Nothing,
      group' = Core.Nothing,
      tags = Core.Nothing,
      resourceQuery = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The service configuration associated with the resource group. For
-- details about the syntax of a service configuration, see
-- <https://docs.aws.amazon.com/ARG/latest/APIReference/about-slg.html Service configurations for resource groups>.
createGroupResponse_groupConfiguration :: Lens.Lens' CreateGroupResponse (Core.Maybe GroupConfiguration)
createGroupResponse_groupConfiguration = Lens.lens (\CreateGroupResponse' {groupConfiguration} -> groupConfiguration) (\s@CreateGroupResponse' {} a -> s {groupConfiguration = a} :: CreateGroupResponse)

-- | The description of the resource group.
createGroupResponse_group :: Lens.Lens' CreateGroupResponse (Core.Maybe Group)
createGroupResponse_group = Lens.lens (\CreateGroupResponse' {group'} -> group') (\s@CreateGroupResponse' {} a -> s {group' = a} :: CreateGroupResponse)

-- | The tags associated with the group.
createGroupResponse_tags :: Lens.Lens' CreateGroupResponse (Core.Maybe (Core.HashMap Core.Text Core.Text))
createGroupResponse_tags = Lens.lens (\CreateGroupResponse' {tags} -> tags) (\s@CreateGroupResponse' {} a -> s {tags = a} :: CreateGroupResponse) Core.. Lens.mapping Lens._Coerce

-- | The resource query associated with the group. For more information about
-- resource queries, see
-- <https://docs.aws.amazon.com/ARG/latest/userguide/gettingstarted-query.html#gettingstarted-query-cli-tag Create a tag-based group in Resource Groups>.
createGroupResponse_resourceQuery :: Lens.Lens' CreateGroupResponse (Core.Maybe ResourceQuery)
createGroupResponse_resourceQuery = Lens.lens (\CreateGroupResponse' {resourceQuery} -> resourceQuery) (\s@CreateGroupResponse' {} a -> s {resourceQuery = a} :: CreateGroupResponse)

-- | The response's http status code.
createGroupResponse_httpStatus :: Lens.Lens' CreateGroupResponse Core.Int
createGroupResponse_httpStatus = Lens.lens (\CreateGroupResponse' {httpStatus} -> httpStatus) (\s@CreateGroupResponse' {} a -> s {httpStatus = a} :: CreateGroupResponse)

instance Core.NFData CreateGroupResponse
