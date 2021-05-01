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
-- Module      : Network.AWS.IAM.DeleteUserPermissionsBoundary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the permissions boundary for the specified IAM user.
--
-- Deleting the permissions boundary for a user might increase its
-- permissions by allowing the user to perform all the actions granted in
-- its permissions policies.
module Network.AWS.IAM.DeleteUserPermissionsBoundary
  ( -- * Creating a Request
    DeleteUserPermissionsBoundary (..),
    newDeleteUserPermissionsBoundary,

    -- * Request Lenses
    deleteUserPermissionsBoundary_userName,

    -- * Destructuring the Response
    DeleteUserPermissionsBoundaryResponse (..),
    newDeleteUserPermissionsBoundaryResponse,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteUserPermissionsBoundary' smart constructor.
data DeleteUserPermissionsBoundary = DeleteUserPermissionsBoundary'
  { -- | The name (friendly name, not ARN) of the IAM user from which you want to
    -- remove the permissions boundary.
    userName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteUserPermissionsBoundary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userName', 'deleteUserPermissionsBoundary_userName' - The name (friendly name, not ARN) of the IAM user from which you want to
-- remove the permissions boundary.
newDeleteUserPermissionsBoundary ::
  -- | 'userName'
  Prelude.Text ->
  DeleteUserPermissionsBoundary
newDeleteUserPermissionsBoundary pUserName_ =
  DeleteUserPermissionsBoundary'
    { userName =
        pUserName_
    }

-- | The name (friendly name, not ARN) of the IAM user from which you want to
-- remove the permissions boundary.
deleteUserPermissionsBoundary_userName :: Lens.Lens' DeleteUserPermissionsBoundary Prelude.Text
deleteUserPermissionsBoundary_userName = Lens.lens (\DeleteUserPermissionsBoundary' {userName} -> userName) (\s@DeleteUserPermissionsBoundary' {} a -> s {userName = a} :: DeleteUserPermissionsBoundary)

instance
  Prelude.AWSRequest
    DeleteUserPermissionsBoundary
  where
  type
    Rs DeleteUserPermissionsBoundary =
      DeleteUserPermissionsBoundaryResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull
      DeleteUserPermissionsBoundaryResponse'

instance
  Prelude.Hashable
    DeleteUserPermissionsBoundary

instance Prelude.NFData DeleteUserPermissionsBoundary

instance
  Prelude.ToHeaders
    DeleteUserPermissionsBoundary
  where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DeleteUserPermissionsBoundary where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
    DeleteUserPermissionsBoundary
  where
  toQuery DeleteUserPermissionsBoundary' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ( "DeleteUserPermissionsBoundary" ::
                         Prelude.ByteString
                     ),
        "Version"
          Prelude.=: ("2010-05-08" :: Prelude.ByteString),
        "UserName" Prelude.=: userName
      ]

-- | /See:/ 'newDeleteUserPermissionsBoundaryResponse' smart constructor.
data DeleteUserPermissionsBoundaryResponse = DeleteUserPermissionsBoundaryResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteUserPermissionsBoundaryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteUserPermissionsBoundaryResponse ::
  DeleteUserPermissionsBoundaryResponse
newDeleteUserPermissionsBoundaryResponse =
  DeleteUserPermissionsBoundaryResponse'

instance
  Prelude.NFData
    DeleteUserPermissionsBoundaryResponse
