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
-- Module      : Network.AWS.CognitoIdentityProvider.GetGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a group.
--
-- Calling this action requires developer credentials.
module Network.AWS.CognitoIdentityProvider.GetGroup
  ( -- * Creating a Request
    GetGroup (..),
    newGetGroup,

    -- * Request Lenses
    getGroup_groupName,
    getGroup_userPoolId,

    -- * Destructuring the Response
    GetGroupResponse (..),
    newGetGroupResponse,

    -- * Response Lenses
    getGroupResponse_group,
    getGroupResponse_httpStatus,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetGroup' smart constructor.
data GetGroup = GetGroup'
  { -- | The name of the group.
    groupName :: Core.Text,
    -- | The user pool ID for the user pool.
    userPoolId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groupName', 'getGroup_groupName' - The name of the group.
--
-- 'userPoolId', 'getGroup_userPoolId' - The user pool ID for the user pool.
newGetGroup ::
  -- | 'groupName'
  Core.Text ->
  -- | 'userPoolId'
  Core.Text ->
  GetGroup
newGetGroup pGroupName_ pUserPoolId_ =
  GetGroup'
    { groupName = pGroupName_,
      userPoolId = pUserPoolId_
    }

-- | The name of the group.
getGroup_groupName :: Lens.Lens' GetGroup Core.Text
getGroup_groupName = Lens.lens (\GetGroup' {groupName} -> groupName) (\s@GetGroup' {} a -> s {groupName = a} :: GetGroup)

-- | The user pool ID for the user pool.
getGroup_userPoolId :: Lens.Lens' GetGroup Core.Text
getGroup_userPoolId = Lens.lens (\GetGroup' {userPoolId} -> userPoolId) (\s@GetGroup' {} a -> s {userPoolId = a} :: GetGroup)

instance Core.AWSRequest GetGroup where
  type AWSResponse GetGroup = GetGroupResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetGroupResponse'
            Core.<$> (x Core..?> "Group")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetGroup

instance Core.NFData GetGroup

instance Core.ToHeaders GetGroup where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSCognitoIdentityProviderService.GetGroup" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetGroup where
  toJSON GetGroup' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("GroupName" Core..= groupName),
            Core.Just ("UserPoolId" Core..= userPoolId)
          ]
      )

instance Core.ToPath GetGroup where
  toPath = Core.const "/"

instance Core.ToQuery GetGroup where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetGroupResponse' smart constructor.
data GetGroupResponse = GetGroupResponse'
  { -- | The group object for the group.
    group' :: Core.Maybe GroupType,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'group'', 'getGroupResponse_group' - The group object for the group.
--
-- 'httpStatus', 'getGroupResponse_httpStatus' - The response's http status code.
newGetGroupResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetGroupResponse
newGetGroupResponse pHttpStatus_ =
  GetGroupResponse'
    { group' = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The group object for the group.
getGroupResponse_group :: Lens.Lens' GetGroupResponse (Core.Maybe GroupType)
getGroupResponse_group = Lens.lens (\GetGroupResponse' {group'} -> group') (\s@GetGroupResponse' {} a -> s {group' = a} :: GetGroupResponse)

-- | The response's http status code.
getGroupResponse_httpStatus :: Lens.Lens' GetGroupResponse Core.Int
getGroupResponse_httpStatus = Lens.lens (\GetGroupResponse' {httpStatus} -> httpStatus) (\s@GetGroupResponse' {} a -> s {httpStatus = a} :: GetGroupResponse)

instance Core.NFData GetGroupResponse
