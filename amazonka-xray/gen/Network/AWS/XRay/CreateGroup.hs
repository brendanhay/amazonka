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
-- Module      : Network.AWS.XRay.CreateGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a group resource with a name and a filter expression.
module Network.AWS.XRay.CreateGroup
  ( -- * Creating a Request
    CreateGroup (..),
    newCreateGroup,

    -- * Request Lenses
    createGroup_insightsConfiguration,
    createGroup_filterExpression,
    createGroup_tags,
    createGroup_groupName,

    -- * Destructuring the Response
    CreateGroupResponse (..),
    newCreateGroupResponse,

    -- * Response Lenses
    createGroupResponse_group,
    createGroupResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.XRay.Types

-- | /See:/ 'newCreateGroup' smart constructor.
data CreateGroup = CreateGroup'
  { -- | The structure containing configurations related to insights.
    --
    -- -   The InsightsEnabled boolean can be set to true to enable insights
    --     for the new group or false to disable insights for the new group.
    --
    -- -   The NotifcationsEnabled boolean can be set to true to enable
    --     insights notifications for the new group. Notifications may only be
    --     enabled on a group with InsightsEnabled set to true.
    insightsConfiguration :: Prelude.Maybe InsightsConfiguration,
    -- | The filter expression defining criteria by which to group traces.
    filterExpression :: Prelude.Maybe Prelude.Text,
    -- | A map that contains one or more tag keys and tag values to attach to an
    -- X-Ray group. For more information about ways to use tags, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS resources>
    -- in the /AWS General Reference/.
    --
    -- The following restrictions apply to tags:
    --
    -- -   Maximum number of user-applied tags per resource: 50
    --
    -- -   Maximum tag key length: 128 Unicode characters
    --
    -- -   Maximum tag value length: 256 Unicode characters
    --
    -- -   Valid values for key and value: a-z, A-Z, 0-9, space, and the
    --     following characters: _ . : \/ = + - and \@
    --
    -- -   Tag keys and values are case sensitive.
    --
    -- -   Don\'t use @aws:@ as a prefix for keys; it\'s reserved for AWS use.
    tags :: Prelude.Maybe [Tag],
    -- | The case-sensitive name of the new group. Default is a reserved name and
    -- names must be unique.
    groupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'insightsConfiguration', 'createGroup_insightsConfiguration' - The structure containing configurations related to insights.
--
-- -   The InsightsEnabled boolean can be set to true to enable insights
--     for the new group or false to disable insights for the new group.
--
-- -   The NotifcationsEnabled boolean can be set to true to enable
--     insights notifications for the new group. Notifications may only be
--     enabled on a group with InsightsEnabled set to true.
--
-- 'filterExpression', 'createGroup_filterExpression' - The filter expression defining criteria by which to group traces.
--
-- 'tags', 'createGroup_tags' - A map that contains one or more tag keys and tag values to attach to an
-- X-Ray group. For more information about ways to use tags, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS resources>
-- in the /AWS General Reference/.
--
-- The following restrictions apply to tags:
--
-- -   Maximum number of user-applied tags per resource: 50
--
-- -   Maximum tag key length: 128 Unicode characters
--
-- -   Maximum tag value length: 256 Unicode characters
--
-- -   Valid values for key and value: a-z, A-Z, 0-9, space, and the
--     following characters: _ . : \/ = + - and \@
--
-- -   Tag keys and values are case sensitive.
--
-- -   Don\'t use @aws:@ as a prefix for keys; it\'s reserved for AWS use.
--
-- 'groupName', 'createGroup_groupName' - The case-sensitive name of the new group. Default is a reserved name and
-- names must be unique.
newCreateGroup ::
  -- | 'groupName'
  Prelude.Text ->
  CreateGroup
newCreateGroup pGroupName_ =
  CreateGroup'
    { insightsConfiguration =
        Prelude.Nothing,
      filterExpression = Prelude.Nothing,
      tags = Prelude.Nothing,
      groupName = pGroupName_
    }

-- | The structure containing configurations related to insights.
--
-- -   The InsightsEnabled boolean can be set to true to enable insights
--     for the new group or false to disable insights for the new group.
--
-- -   The NotifcationsEnabled boolean can be set to true to enable
--     insights notifications for the new group. Notifications may only be
--     enabled on a group with InsightsEnabled set to true.
createGroup_insightsConfiguration :: Lens.Lens' CreateGroup (Prelude.Maybe InsightsConfiguration)
createGroup_insightsConfiguration = Lens.lens (\CreateGroup' {insightsConfiguration} -> insightsConfiguration) (\s@CreateGroup' {} a -> s {insightsConfiguration = a} :: CreateGroup)

-- | The filter expression defining criteria by which to group traces.
createGroup_filterExpression :: Lens.Lens' CreateGroup (Prelude.Maybe Prelude.Text)
createGroup_filterExpression = Lens.lens (\CreateGroup' {filterExpression} -> filterExpression) (\s@CreateGroup' {} a -> s {filterExpression = a} :: CreateGroup)

-- | A map that contains one or more tag keys and tag values to attach to an
-- X-Ray group. For more information about ways to use tags, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS resources>
-- in the /AWS General Reference/.
--
-- The following restrictions apply to tags:
--
-- -   Maximum number of user-applied tags per resource: 50
--
-- -   Maximum tag key length: 128 Unicode characters
--
-- -   Maximum tag value length: 256 Unicode characters
--
-- -   Valid values for key and value: a-z, A-Z, 0-9, space, and the
--     following characters: _ . : \/ = + - and \@
--
-- -   Tag keys and values are case sensitive.
--
-- -   Don\'t use @aws:@ as a prefix for keys; it\'s reserved for AWS use.
createGroup_tags :: Lens.Lens' CreateGroup (Prelude.Maybe [Tag])
createGroup_tags = Lens.lens (\CreateGroup' {tags} -> tags) (\s@CreateGroup' {} a -> s {tags = a} :: CreateGroup) Prelude.. Lens.mapping Prelude._Coerce

-- | The case-sensitive name of the new group. Default is a reserved name and
-- names must be unique.
createGroup_groupName :: Lens.Lens' CreateGroup Prelude.Text
createGroup_groupName = Lens.lens (\CreateGroup' {groupName} -> groupName) (\s@CreateGroup' {} a -> s {groupName = a} :: CreateGroup)

instance Prelude.AWSRequest CreateGroup where
  type Rs CreateGroup = CreateGroupResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateGroupResponse'
            Prelude.<$> (x Prelude..?> "Group")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateGroup

instance Prelude.NFData CreateGroup

instance Prelude.ToHeaders CreateGroup where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToJSON CreateGroup where
  toJSON CreateGroup' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("InsightsConfiguration" Prelude..=)
              Prelude.<$> insightsConfiguration,
            ("FilterExpression" Prelude..=)
              Prelude.<$> filterExpression,
            ("Tags" Prelude..=) Prelude.<$> tags,
            Prelude.Just ("GroupName" Prelude..= groupName)
          ]
      )

instance Prelude.ToPath CreateGroup where
  toPath = Prelude.const "/CreateGroup"

instance Prelude.ToQuery CreateGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateGroupResponse' smart constructor.
data CreateGroupResponse = CreateGroupResponse'
  { -- | The group that was created. Contains the name of the group that was
    -- created, the Amazon Resource Name (ARN) of the group that was generated
    -- based on the group name, the filter expression, and the insight
    -- configuration that was assigned to the group.
    group' :: Prelude.Maybe Group,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'group'', 'createGroupResponse_group' - The group that was created. Contains the name of the group that was
-- created, the Amazon Resource Name (ARN) of the group that was generated
-- based on the group name, the filter expression, and the insight
-- configuration that was assigned to the group.
--
-- 'httpStatus', 'createGroupResponse_httpStatus' - The response's http status code.
newCreateGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateGroupResponse
newCreateGroupResponse pHttpStatus_ =
  CreateGroupResponse'
    { group' = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The group that was created. Contains the name of the group that was
-- created, the Amazon Resource Name (ARN) of the group that was generated
-- based on the group name, the filter expression, and the insight
-- configuration that was assigned to the group.
createGroupResponse_group :: Lens.Lens' CreateGroupResponse (Prelude.Maybe Group)
createGroupResponse_group = Lens.lens (\CreateGroupResponse' {group'} -> group') (\s@CreateGroupResponse' {} a -> s {group' = a} :: CreateGroupResponse)

-- | The response's http status code.
createGroupResponse_httpStatus :: Lens.Lens' CreateGroupResponse Prelude.Int
createGroupResponse_httpStatus = Lens.lens (\CreateGroupResponse' {httpStatus} -> httpStatus) (\s@CreateGroupResponse' {} a -> s {httpStatus = a} :: CreateGroupResponse)

instance Prelude.NFData CreateGroupResponse
