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
-- Module      : Amazonka.CognitoIdentityProvider.DeleteGroup
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a group.
--
-- Calling this action requires developer credentials.
module Amazonka.CognitoIdentityProvider.DeleteGroup
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

import Amazonka.CognitoIdentityProvider.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteGroup' smart constructor.
data DeleteGroup = DeleteGroup'
  { -- | The name of the group.
    groupName :: Prelude.Text,
    -- | The user pool ID for the user pool.
    userPoolId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Core.AWSRequest DeleteGroup where
  type AWSResponse DeleteGroup = DeleteGroupResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response = Response.receiveNull DeleteGroupResponse'

instance Prelude.Hashable DeleteGroup where
  hashWithSalt _salt DeleteGroup' {..} =
    _salt `Prelude.hashWithSalt` groupName
      `Prelude.hashWithSalt` userPoolId

instance Prelude.NFData DeleteGroup where
  rnf DeleteGroup' {..} =
    Prelude.rnf groupName
      `Prelude.seq` Prelude.rnf userPoolId

instance Data.ToHeaders DeleteGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSCognitoIdentityProviderService.DeleteGroup" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteGroup where
  toJSON DeleteGroup' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("GroupName" Data..= groupName),
            Prelude.Just ("UserPoolId" Data..= userPoolId)
          ]
      )

instance Data.ToPath DeleteGroup where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteGroupResponse' smart constructor.
data DeleteGroupResponse = DeleteGroupResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteGroupResponse ::
  DeleteGroupResponse
newDeleteGroupResponse = DeleteGroupResponse'

instance Prelude.NFData DeleteGroupResponse where
  rnf _ = ()
