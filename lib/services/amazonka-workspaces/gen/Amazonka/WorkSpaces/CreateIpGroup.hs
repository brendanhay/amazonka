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
-- Module      : Amazonka.WorkSpaces.CreateIpGroup
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an IP access control group.
--
-- An IP access control group provides you with the ability to control the
-- IP addresses from which users are allowed to access their WorkSpaces. To
-- specify the CIDR address ranges, add rules to your IP access control
-- group and then associate the group with your directory. You can add
-- rules when you create the group or at any time using AuthorizeIpRules.
--
-- There is a default IP access control group associated with your
-- directory. If you don\'t associate an IP access control group with your
-- directory, the default group is used. The default group includes a
-- default rule that allows users to access their WorkSpaces from anywhere.
-- You cannot modify the default IP access control group for your
-- directory.
module Amazonka.WorkSpaces.CreateIpGroup
  ( -- * Creating a Request
    CreateIpGroup (..),
    newCreateIpGroup,

    -- * Request Lenses
    createIpGroup_groupDesc,
    createIpGroup_tags,
    createIpGroup_userRules,
    createIpGroup_groupName,

    -- * Destructuring the Response
    CreateIpGroupResponse (..),
    newCreateIpGroupResponse,

    -- * Response Lenses
    createIpGroupResponse_groupId,
    createIpGroupResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkSpaces.Types

-- | /See:/ 'newCreateIpGroup' smart constructor.
data CreateIpGroup = CreateIpGroup'
  { -- | The description of the group.
    groupDesc :: Prelude.Maybe Prelude.Text,
    -- | The tags. Each WorkSpaces resource can have a maximum of 50 tags.
    tags :: Prelude.Maybe [Tag],
    -- | The rules to add to the group.
    userRules :: Prelude.Maybe [IpRuleItem],
    -- | The name of the group.
    groupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateIpGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groupDesc', 'createIpGroup_groupDesc' - The description of the group.
--
-- 'tags', 'createIpGroup_tags' - The tags. Each WorkSpaces resource can have a maximum of 50 tags.
--
-- 'userRules', 'createIpGroup_userRules' - The rules to add to the group.
--
-- 'groupName', 'createIpGroup_groupName' - The name of the group.
newCreateIpGroup ::
  -- | 'groupName'
  Prelude.Text ->
  CreateIpGroup
newCreateIpGroup pGroupName_ =
  CreateIpGroup'
    { groupDesc = Prelude.Nothing,
      tags = Prelude.Nothing,
      userRules = Prelude.Nothing,
      groupName = pGroupName_
    }

-- | The description of the group.
createIpGroup_groupDesc :: Lens.Lens' CreateIpGroup (Prelude.Maybe Prelude.Text)
createIpGroup_groupDesc = Lens.lens (\CreateIpGroup' {groupDesc} -> groupDesc) (\s@CreateIpGroup' {} a -> s {groupDesc = a} :: CreateIpGroup)

-- | The tags. Each WorkSpaces resource can have a maximum of 50 tags.
createIpGroup_tags :: Lens.Lens' CreateIpGroup (Prelude.Maybe [Tag])
createIpGroup_tags = Lens.lens (\CreateIpGroup' {tags} -> tags) (\s@CreateIpGroup' {} a -> s {tags = a} :: CreateIpGroup) Prelude.. Lens.mapping Lens.coerced

-- | The rules to add to the group.
createIpGroup_userRules :: Lens.Lens' CreateIpGroup (Prelude.Maybe [IpRuleItem])
createIpGroup_userRules = Lens.lens (\CreateIpGroup' {userRules} -> userRules) (\s@CreateIpGroup' {} a -> s {userRules = a} :: CreateIpGroup) Prelude.. Lens.mapping Lens.coerced

-- | The name of the group.
createIpGroup_groupName :: Lens.Lens' CreateIpGroup Prelude.Text
createIpGroup_groupName = Lens.lens (\CreateIpGroup' {groupName} -> groupName) (\s@CreateIpGroup' {} a -> s {groupName = a} :: CreateIpGroup)

instance Core.AWSRequest CreateIpGroup where
  type
    AWSResponse CreateIpGroup =
      CreateIpGroupResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateIpGroupResponse'
            Prelude.<$> (x Data..?> "GroupId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateIpGroup where
  hashWithSalt _salt CreateIpGroup' {..} =
    _salt `Prelude.hashWithSalt` groupDesc
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` userRules
      `Prelude.hashWithSalt` groupName

instance Prelude.NFData CreateIpGroup where
  rnf CreateIpGroup' {..} =
    Prelude.rnf groupDesc
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf userRules
      `Prelude.seq` Prelude.rnf groupName

instance Data.ToHeaders CreateIpGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "WorkspacesService.CreateIpGroup" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateIpGroup where
  toJSON CreateIpGroup' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("GroupDesc" Data..=) Prelude.<$> groupDesc,
            ("Tags" Data..=) Prelude.<$> tags,
            ("UserRules" Data..=) Prelude.<$> userRules,
            Prelude.Just ("GroupName" Data..= groupName)
          ]
      )

instance Data.ToPath CreateIpGroup where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateIpGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateIpGroupResponse' smart constructor.
data CreateIpGroupResponse = CreateIpGroupResponse'
  { -- | The identifier of the group.
    groupId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateIpGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groupId', 'createIpGroupResponse_groupId' - The identifier of the group.
--
-- 'httpStatus', 'createIpGroupResponse_httpStatus' - The response's http status code.
newCreateIpGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateIpGroupResponse
newCreateIpGroupResponse pHttpStatus_ =
  CreateIpGroupResponse'
    { groupId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The identifier of the group.
createIpGroupResponse_groupId :: Lens.Lens' CreateIpGroupResponse (Prelude.Maybe Prelude.Text)
createIpGroupResponse_groupId = Lens.lens (\CreateIpGroupResponse' {groupId} -> groupId) (\s@CreateIpGroupResponse' {} a -> s {groupId = a} :: CreateIpGroupResponse)

-- | The response's http status code.
createIpGroupResponse_httpStatus :: Lens.Lens' CreateIpGroupResponse Prelude.Int
createIpGroupResponse_httpStatus = Lens.lens (\CreateIpGroupResponse' {httpStatus} -> httpStatus) (\s@CreateIpGroupResponse' {} a -> s {httpStatus = a} :: CreateIpGroupResponse)

instance Prelude.NFData CreateIpGroupResponse where
  rnf CreateIpGroupResponse' {..} =
    Prelude.rnf groupId
      `Prelude.seq` Prelude.rnf httpStatus
