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
-- Module      : Network.AWS.Cloud9.DeleteEnvironmentMembership
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an environment member from an AWS Cloud9 development
-- environment.
module Network.AWS.Cloud9.DeleteEnvironmentMembership
  ( -- * Creating a Request
    DeleteEnvironmentMembership (..),
    newDeleteEnvironmentMembership,

    -- * Request Lenses
    deleteEnvironmentMembership_environmentId,
    deleteEnvironmentMembership_userArn,

    -- * Destructuring the Response
    DeleteEnvironmentMembershipResponse (..),
    newDeleteEnvironmentMembershipResponse,

    -- * Response Lenses
    deleteEnvironmentMembershipResponse_httpStatus,
  )
where

import Network.AWS.Cloud9.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteEnvironmentMembership' smart constructor.
data DeleteEnvironmentMembership = DeleteEnvironmentMembership'
  { -- | The ID of the environment to delete the environment member from.
    environmentId :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the environment member to delete from
    -- the environment.
    userArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteEnvironmentMembership' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'environmentId', 'deleteEnvironmentMembership_environmentId' - The ID of the environment to delete the environment member from.
--
-- 'userArn', 'deleteEnvironmentMembership_userArn' - The Amazon Resource Name (ARN) of the environment member to delete from
-- the environment.
newDeleteEnvironmentMembership ::
  -- | 'environmentId'
  Prelude.Text ->
  -- | 'userArn'
  Prelude.Text ->
  DeleteEnvironmentMembership
newDeleteEnvironmentMembership
  pEnvironmentId_
  pUserArn_ =
    DeleteEnvironmentMembership'
      { environmentId =
          pEnvironmentId_,
        userArn = pUserArn_
      }

-- | The ID of the environment to delete the environment member from.
deleteEnvironmentMembership_environmentId :: Lens.Lens' DeleteEnvironmentMembership Prelude.Text
deleteEnvironmentMembership_environmentId = Lens.lens (\DeleteEnvironmentMembership' {environmentId} -> environmentId) (\s@DeleteEnvironmentMembership' {} a -> s {environmentId = a} :: DeleteEnvironmentMembership)

-- | The Amazon Resource Name (ARN) of the environment member to delete from
-- the environment.
deleteEnvironmentMembership_userArn :: Lens.Lens' DeleteEnvironmentMembership Prelude.Text
deleteEnvironmentMembership_userArn = Lens.lens (\DeleteEnvironmentMembership' {userArn} -> userArn) (\s@DeleteEnvironmentMembership' {} a -> s {userArn = a} :: DeleteEnvironmentMembership)

instance
  Prelude.AWSRequest
    DeleteEnvironmentMembership
  where
  type
    Rs DeleteEnvironmentMembership =
      DeleteEnvironmentMembershipResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteEnvironmentMembershipResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteEnvironmentMembership

instance Prelude.NFData DeleteEnvironmentMembership

instance
  Prelude.ToHeaders
    DeleteEnvironmentMembership
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSCloud9WorkspaceManagementService.DeleteEnvironmentMembership" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteEnvironmentMembership where
  toJSON DeleteEnvironmentMembership' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("environmentId" Prelude..= environmentId),
            Prelude.Just ("userArn" Prelude..= userArn)
          ]
      )

instance Prelude.ToPath DeleteEnvironmentMembership where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteEnvironmentMembership where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteEnvironmentMembershipResponse' smart constructor.
data DeleteEnvironmentMembershipResponse = DeleteEnvironmentMembershipResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteEnvironmentMembershipResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteEnvironmentMembershipResponse_httpStatus' - The response's http status code.
newDeleteEnvironmentMembershipResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteEnvironmentMembershipResponse
newDeleteEnvironmentMembershipResponse pHttpStatus_ =
  DeleteEnvironmentMembershipResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteEnvironmentMembershipResponse_httpStatus :: Lens.Lens' DeleteEnvironmentMembershipResponse Prelude.Int
deleteEnvironmentMembershipResponse_httpStatus = Lens.lens (\DeleteEnvironmentMembershipResponse' {httpStatus} -> httpStatus) (\s@DeleteEnvironmentMembershipResponse' {} a -> s {httpStatus = a} :: DeleteEnvironmentMembershipResponse)

instance
  Prelude.NFData
    DeleteEnvironmentMembershipResponse
