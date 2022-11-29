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
-- Module      : Amazonka.ResourceGroups.CreateGroup
-- Copyright   : (c) 2013-2022 Brendan Hay
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
module Amazonka.ResourceGroups.CreateGroup
  ( -- * Creating a Request
    CreateGroup (..),
    newCreateGroup,

    -- * Request Lenses
    createGroup_tags,
    createGroup_configuration,
    createGroup_description,
    createGroup_resourceQuery,
    createGroup_name,

    -- * Destructuring the Response
    CreateGroupResponse (..),
    newCreateGroupResponse,

    -- * Response Lenses
    createGroupResponse_tags,
    createGroupResponse_groupConfiguration,
    createGroupResponse_resourceQuery,
    createGroupResponse_group,
    createGroupResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import Amazonka.ResourceGroups.Types
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateGroup' smart constructor.
data CreateGroup = CreateGroup'
  { -- | The tags to add to the group. A tag is key-value pair string.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | A configuration associates the resource group with an AWS service and
    -- specifies how the service can interact with the resources in the group.
    -- A configuration is an array of GroupConfigurationItem elements. For
    -- details about the syntax of service configurations, see
    -- <https://docs.aws.amazon.com/ARG/latest/APIReference/about-slg.html Service configurations for resource groups>.
    --
    -- A resource group can contain either a @Configuration@ or a
    -- @ResourceQuery@, but not both.
    configuration :: Prelude.Maybe [GroupConfigurationItem],
    -- | The description of the resource group. Descriptions can consist of
    -- letters, numbers, hyphens, underscores, periods, and spaces.
    description :: Prelude.Maybe Prelude.Text,
    -- | The resource query that determines which AWS resources are members of
    -- this group. For more information about resource queries, see
    -- <https://docs.aws.amazon.com/ARG/latest/userguide/gettingstarted-query.html#gettingstarted-query-cli-tag Create a tag-based group in Resource Groups>.
    --
    -- A resource group can contain either a @ResourceQuery@ or a
    -- @Configuration@, but not both.
    resourceQuery :: Prelude.Maybe ResourceQuery,
    -- | The name of the group, which is the identifier of the group in other
    -- operations. You can\'t change the name of a resource group after you
    -- create it. A resource group name can consist of letters, numbers,
    -- hyphens, periods, and underscores. The name cannot start with @AWS@ or
    -- @aws@; these are reserved. A resource group name must be unique within
    -- each AWS Region in your AWS account.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createGroup_tags' - The tags to add to the group. A tag is key-value pair string.
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
  Prelude.Text ->
  CreateGroup
newCreateGroup pName_ =
  CreateGroup'
    { tags = Prelude.Nothing,
      configuration = Prelude.Nothing,
      description = Prelude.Nothing,
      resourceQuery = Prelude.Nothing,
      name = pName_
    }

-- | The tags to add to the group. A tag is key-value pair string.
createGroup_tags :: Lens.Lens' CreateGroup (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createGroup_tags = Lens.lens (\CreateGroup' {tags} -> tags) (\s@CreateGroup' {} a -> s {tags = a} :: CreateGroup) Prelude.. Lens.mapping Lens.coerced

-- | A configuration associates the resource group with an AWS service and
-- specifies how the service can interact with the resources in the group.
-- A configuration is an array of GroupConfigurationItem elements. For
-- details about the syntax of service configurations, see
-- <https://docs.aws.amazon.com/ARG/latest/APIReference/about-slg.html Service configurations for resource groups>.
--
-- A resource group can contain either a @Configuration@ or a
-- @ResourceQuery@, but not both.
createGroup_configuration :: Lens.Lens' CreateGroup (Prelude.Maybe [GroupConfigurationItem])
createGroup_configuration = Lens.lens (\CreateGroup' {configuration} -> configuration) (\s@CreateGroup' {} a -> s {configuration = a} :: CreateGroup) Prelude.. Lens.mapping Lens.coerced

-- | The description of the resource group. Descriptions can consist of
-- letters, numbers, hyphens, underscores, periods, and spaces.
createGroup_description :: Lens.Lens' CreateGroup (Prelude.Maybe Prelude.Text)
createGroup_description = Lens.lens (\CreateGroup' {description} -> description) (\s@CreateGroup' {} a -> s {description = a} :: CreateGroup)

-- | The resource query that determines which AWS resources are members of
-- this group. For more information about resource queries, see
-- <https://docs.aws.amazon.com/ARG/latest/userguide/gettingstarted-query.html#gettingstarted-query-cli-tag Create a tag-based group in Resource Groups>.
--
-- A resource group can contain either a @ResourceQuery@ or a
-- @Configuration@, but not both.
createGroup_resourceQuery :: Lens.Lens' CreateGroup (Prelude.Maybe ResourceQuery)
createGroup_resourceQuery = Lens.lens (\CreateGroup' {resourceQuery} -> resourceQuery) (\s@CreateGroup' {} a -> s {resourceQuery = a} :: CreateGroup)

-- | The name of the group, which is the identifier of the group in other
-- operations. You can\'t change the name of a resource group after you
-- create it. A resource group name can consist of letters, numbers,
-- hyphens, periods, and underscores. The name cannot start with @AWS@ or
-- @aws@; these are reserved. A resource group name must be unique within
-- each AWS Region in your AWS account.
createGroup_name :: Lens.Lens' CreateGroup Prelude.Text
createGroup_name = Lens.lens (\CreateGroup' {name} -> name) (\s@CreateGroup' {} a -> s {name = a} :: CreateGroup)

instance Core.AWSRequest CreateGroup where
  type AWSResponse CreateGroup = CreateGroupResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateGroupResponse'
            Prelude.<$> (x Core..?> "Tags" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "GroupConfiguration")
            Prelude.<*> (x Core..?> "ResourceQuery")
            Prelude.<*> (x Core..?> "Group")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateGroup where
  hashWithSalt _salt CreateGroup' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` configuration
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` resourceQuery
      `Prelude.hashWithSalt` name

instance Prelude.NFData CreateGroup where
  rnf CreateGroup' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf configuration
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf resourceQuery
      `Prelude.seq` Prelude.rnf name

instance Core.ToHeaders CreateGroup where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToJSON CreateGroup where
  toJSON CreateGroup' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Tags" Core..=) Prelude.<$> tags,
            ("Configuration" Core..=) Prelude.<$> configuration,
            ("Description" Core..=) Prelude.<$> description,
            ("ResourceQuery" Core..=) Prelude.<$> resourceQuery,
            Prelude.Just ("Name" Core..= name)
          ]
      )

instance Core.ToPath CreateGroup where
  toPath = Prelude.const "/groups"

instance Core.ToQuery CreateGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateGroupResponse' smart constructor.
data CreateGroupResponse = CreateGroupResponse'
  { -- | The tags associated with the group.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The service configuration associated with the resource group. For
    -- details about the syntax of a service configuration, see
    -- <https://docs.aws.amazon.com/ARG/latest/APIReference/about-slg.html Service configurations for resource groups>.
    groupConfiguration :: Prelude.Maybe GroupConfiguration,
    -- | The resource query associated with the group. For more information about
    -- resource queries, see
    -- <https://docs.aws.amazon.com/ARG/latest/userguide/gettingstarted-query.html#gettingstarted-query-cli-tag Create a tag-based group in Resource Groups>.
    resourceQuery :: Prelude.Maybe ResourceQuery,
    -- | The description of the resource group.
    group' :: Prelude.Maybe Group,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createGroupResponse_tags' - The tags associated with the group.
--
-- 'groupConfiguration', 'createGroupResponse_groupConfiguration' - The service configuration associated with the resource group. For
-- details about the syntax of a service configuration, see
-- <https://docs.aws.amazon.com/ARG/latest/APIReference/about-slg.html Service configurations for resource groups>.
--
-- 'resourceQuery', 'createGroupResponse_resourceQuery' - The resource query associated with the group. For more information about
-- resource queries, see
-- <https://docs.aws.amazon.com/ARG/latest/userguide/gettingstarted-query.html#gettingstarted-query-cli-tag Create a tag-based group in Resource Groups>.
--
-- 'group'', 'createGroupResponse_group' - The description of the resource group.
--
-- 'httpStatus', 'createGroupResponse_httpStatus' - The response's http status code.
newCreateGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateGroupResponse
newCreateGroupResponse pHttpStatus_ =
  CreateGroupResponse'
    { tags = Prelude.Nothing,
      groupConfiguration = Prelude.Nothing,
      resourceQuery = Prelude.Nothing,
      group' = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The tags associated with the group.
createGroupResponse_tags :: Lens.Lens' CreateGroupResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createGroupResponse_tags = Lens.lens (\CreateGroupResponse' {tags} -> tags) (\s@CreateGroupResponse' {} a -> s {tags = a} :: CreateGroupResponse) Prelude.. Lens.mapping Lens.coerced

-- | The service configuration associated with the resource group. For
-- details about the syntax of a service configuration, see
-- <https://docs.aws.amazon.com/ARG/latest/APIReference/about-slg.html Service configurations for resource groups>.
createGroupResponse_groupConfiguration :: Lens.Lens' CreateGroupResponse (Prelude.Maybe GroupConfiguration)
createGroupResponse_groupConfiguration = Lens.lens (\CreateGroupResponse' {groupConfiguration} -> groupConfiguration) (\s@CreateGroupResponse' {} a -> s {groupConfiguration = a} :: CreateGroupResponse)

-- | The resource query associated with the group. For more information about
-- resource queries, see
-- <https://docs.aws.amazon.com/ARG/latest/userguide/gettingstarted-query.html#gettingstarted-query-cli-tag Create a tag-based group in Resource Groups>.
createGroupResponse_resourceQuery :: Lens.Lens' CreateGroupResponse (Prelude.Maybe ResourceQuery)
createGroupResponse_resourceQuery = Lens.lens (\CreateGroupResponse' {resourceQuery} -> resourceQuery) (\s@CreateGroupResponse' {} a -> s {resourceQuery = a} :: CreateGroupResponse)

-- | The description of the resource group.
createGroupResponse_group :: Lens.Lens' CreateGroupResponse (Prelude.Maybe Group)
createGroupResponse_group = Lens.lens (\CreateGroupResponse' {group'} -> group') (\s@CreateGroupResponse' {} a -> s {group' = a} :: CreateGroupResponse)

-- | The response's http status code.
createGroupResponse_httpStatus :: Lens.Lens' CreateGroupResponse Prelude.Int
createGroupResponse_httpStatus = Lens.lens (\CreateGroupResponse' {httpStatus} -> httpStatus) (\s@CreateGroupResponse' {} a -> s {httpStatus = a} :: CreateGroupResponse)

instance Prelude.NFData CreateGroupResponse where
  rnf CreateGroupResponse' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf groupConfiguration
      `Prelude.seq` Prelude.rnf resourceQuery
      `Prelude.seq` Prelude.rnf group'
      `Prelude.seq` Prelude.rnf httpStatus
