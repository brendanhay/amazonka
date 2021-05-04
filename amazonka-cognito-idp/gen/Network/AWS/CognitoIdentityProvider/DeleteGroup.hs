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
-- Module      : Network.AWS.CognitoIdentityProvider.DeleteGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a group. Currently only groups with no members can be deleted.
--
-- Calling this action requires developer credentials.
module Network.AWS.CognitoIdentityProvider.DeleteGroup
  ( -- * Creating a Request
    DeleteGroup (..),
    newDeleteGroup,

    -- * Request Lenses
    deleteGroup_groupName,
    deleteGroup_userPoolId,

    -- * Destructuring the Response
    DeleteGroupResponse (..),
    newDeleteGroupResponse,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteGroup' smart constructor.
data DeleteGroup = DeleteGroup'
  { -- | The name of the group.
    groupName :: Prelude.Text,
    -- | The user pool ID for the user pool.
    userPoolId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groupName', 'deleteGroup_groupName' - The name of the group.
--
-- 'userPoolId', 'deleteGroup_userPoolId' - The user pool ID for the user pool.
newDeleteGroup ::
  -- | 'groupName'
  Prelude.Text ->
  -- | 'userPoolId'
  Prelude.Text ->
  DeleteGroup
newDeleteGroup pGroupName_ pUserPoolId_ =
  DeleteGroup'
    { groupName = pGroupName_,
      userPoolId = pUserPoolId_
    }

-- | The name of the group.
deleteGroup_groupName :: Lens.Lens' DeleteGroup Prelude.Text
deleteGroup_groupName = Lens.lens (\DeleteGroup' {groupName} -> groupName) (\s@DeleteGroup' {} a -> s {groupName = a} :: DeleteGroup)

-- | The user pool ID for the user pool.
deleteGroup_userPoolId :: Lens.Lens' DeleteGroup Prelude.Text
deleteGroup_userPoolId = Lens.lens (\DeleteGroup' {userPoolId} -> userPoolId) (\s@DeleteGroup' {} a -> s {userPoolId = a} :: DeleteGroup)

instance Prelude.AWSRequest DeleteGroup where
  type Rs DeleteGroup = DeleteGroupResponse
  request = Request.postJSON defaultService
  response = Response.receiveNull DeleteGroupResponse'

instance Prelude.Hashable DeleteGroup

instance Prelude.NFData DeleteGroup

instance Prelude.ToHeaders DeleteGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSCognitoIdentityProviderService.DeleteGroup" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteGroup where
  toJSON DeleteGroup' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("GroupName" Prelude..= groupName),
            Prelude.Just ("UserPoolId" Prelude..= userPoolId)
          ]
      )

instance Prelude.ToPath DeleteGroup where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteGroupResponse' smart constructor.
data DeleteGroupResponse = DeleteGroupResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteGroupResponse ::
  DeleteGroupResponse
newDeleteGroupResponse = DeleteGroupResponse'

instance Prelude.NFData DeleteGroupResponse
