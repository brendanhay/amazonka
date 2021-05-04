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
-- Module      : Network.AWS.IAM.PutUserPermissionsBoundary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds or updates the policy that is specified as the IAM user\'s
-- permissions boundary. You can use an AWS managed policy or a customer
-- managed policy to set the boundary for a user. Use the boundary to
-- control the maximum permissions that the user can have. Setting a
-- permissions boundary is an advanced feature that can affect the
-- permissions for the user.
--
-- Policies that are used as permissions boundaries do not provide
-- permissions. You must also attach a permissions policy to the user. To
-- learn how the effective permissions for a user are evaluated, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_evaluation-logic.html IAM JSON policy evaluation logic>
-- in the IAM User Guide.
module Network.AWS.IAM.PutUserPermissionsBoundary
  ( -- * Creating a Request
    PutUserPermissionsBoundary (..),
    newPutUserPermissionsBoundary,

    -- * Request Lenses
    putUserPermissionsBoundary_userName,
    putUserPermissionsBoundary_permissionsBoundary,

    -- * Destructuring the Response
    PutUserPermissionsBoundaryResponse (..),
    newPutUserPermissionsBoundaryResponse,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newPutUserPermissionsBoundary' smart constructor.
data PutUserPermissionsBoundary = PutUserPermissionsBoundary'
  { -- | The name (friendly name, not ARN) of the IAM user for which you want to
    -- set the permissions boundary.
    userName :: Prelude.Text,
    -- | The ARN of the policy that is used to set the permissions boundary for
    -- the user.
    permissionsBoundary :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PutUserPermissionsBoundary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userName', 'putUserPermissionsBoundary_userName' - The name (friendly name, not ARN) of the IAM user for which you want to
-- set the permissions boundary.
--
-- 'permissionsBoundary', 'putUserPermissionsBoundary_permissionsBoundary' - The ARN of the policy that is used to set the permissions boundary for
-- the user.
newPutUserPermissionsBoundary ::
  -- | 'userName'
  Prelude.Text ->
  -- | 'permissionsBoundary'
  Prelude.Text ->
  PutUserPermissionsBoundary
newPutUserPermissionsBoundary
  pUserName_
  pPermissionsBoundary_ =
    PutUserPermissionsBoundary'
      { userName = pUserName_,
        permissionsBoundary = pPermissionsBoundary_
      }

-- | The name (friendly name, not ARN) of the IAM user for which you want to
-- set the permissions boundary.
putUserPermissionsBoundary_userName :: Lens.Lens' PutUserPermissionsBoundary Prelude.Text
putUserPermissionsBoundary_userName = Lens.lens (\PutUserPermissionsBoundary' {userName} -> userName) (\s@PutUserPermissionsBoundary' {} a -> s {userName = a} :: PutUserPermissionsBoundary)

-- | The ARN of the policy that is used to set the permissions boundary for
-- the user.
putUserPermissionsBoundary_permissionsBoundary :: Lens.Lens' PutUserPermissionsBoundary Prelude.Text
putUserPermissionsBoundary_permissionsBoundary = Lens.lens (\PutUserPermissionsBoundary' {permissionsBoundary} -> permissionsBoundary) (\s@PutUserPermissionsBoundary' {} a -> s {permissionsBoundary = a} :: PutUserPermissionsBoundary)

instance
  Prelude.AWSRequest
    PutUserPermissionsBoundary
  where
  type
    Rs PutUserPermissionsBoundary =
      PutUserPermissionsBoundaryResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull
      PutUserPermissionsBoundaryResponse'

instance Prelude.Hashable PutUserPermissionsBoundary

instance Prelude.NFData PutUserPermissionsBoundary

instance Prelude.ToHeaders PutUserPermissionsBoundary where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath PutUserPermissionsBoundary where
  toPath = Prelude.const "/"

instance Prelude.ToQuery PutUserPermissionsBoundary where
  toQuery PutUserPermissionsBoundary' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("PutUserPermissionsBoundary" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2010-05-08" :: Prelude.ByteString),
        "UserName" Prelude.=: userName,
        "PermissionsBoundary" Prelude.=: permissionsBoundary
      ]

-- | /See:/ 'newPutUserPermissionsBoundaryResponse' smart constructor.
data PutUserPermissionsBoundaryResponse = PutUserPermissionsBoundaryResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PutUserPermissionsBoundaryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newPutUserPermissionsBoundaryResponse ::
  PutUserPermissionsBoundaryResponse
newPutUserPermissionsBoundaryResponse =
  PutUserPermissionsBoundaryResponse'

instance
  Prelude.NFData
    PutUserPermissionsBoundaryResponse
