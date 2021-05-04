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
-- Module      : Network.AWS.IAM.RemoveRoleFromInstanceProfile
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the specified IAM role from the specified EC2 instance profile.
--
-- Make sure that you do not have any Amazon EC2 instances running with the
-- role you are about to remove from the instance profile. Removing a role
-- from an instance profile that is associated with a running instance
-- might break any applications running on the instance.
--
-- For more information about IAM roles, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/WorkingWithRoles.html Working with roles>.
-- For more information about instance profiles, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/AboutInstanceProfiles.html About instance profiles>.
module Network.AWS.IAM.RemoveRoleFromInstanceProfile
  ( -- * Creating a Request
    RemoveRoleFromInstanceProfile (..),
    newRemoveRoleFromInstanceProfile,

    -- * Request Lenses
    removeRoleFromInstanceProfile_instanceProfileName,
    removeRoleFromInstanceProfile_roleName,

    -- * Destructuring the Response
    RemoveRoleFromInstanceProfileResponse (..),
    newRemoveRoleFromInstanceProfileResponse,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newRemoveRoleFromInstanceProfile' smart constructor.
data RemoveRoleFromInstanceProfile = RemoveRoleFromInstanceProfile'
  { -- | The name of the instance profile to update.
    --
    -- This parameter allows (through its
    -- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
    -- consisting of upper and lowercase alphanumeric characters with no
    -- spaces. You can also include any of the following characters: _+=,.\@-
    instanceProfileName :: Prelude.Text,
    -- | The name of the role to remove.
    --
    -- This parameter allows (through its
    -- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
    -- consisting of upper and lowercase alphanumeric characters with no
    -- spaces. You can also include any of the following characters: _+=,.\@-
    roleName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RemoveRoleFromInstanceProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceProfileName', 'removeRoleFromInstanceProfile_instanceProfileName' - The name of the instance profile to update.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
--
-- 'roleName', 'removeRoleFromInstanceProfile_roleName' - The name of the role to remove.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
newRemoveRoleFromInstanceProfile ::
  -- | 'instanceProfileName'
  Prelude.Text ->
  -- | 'roleName'
  Prelude.Text ->
  RemoveRoleFromInstanceProfile
newRemoveRoleFromInstanceProfile
  pInstanceProfileName_
  pRoleName_ =
    RemoveRoleFromInstanceProfile'
      { instanceProfileName =
          pInstanceProfileName_,
        roleName = pRoleName_
      }

-- | The name of the instance profile to update.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
removeRoleFromInstanceProfile_instanceProfileName :: Lens.Lens' RemoveRoleFromInstanceProfile Prelude.Text
removeRoleFromInstanceProfile_instanceProfileName = Lens.lens (\RemoveRoleFromInstanceProfile' {instanceProfileName} -> instanceProfileName) (\s@RemoveRoleFromInstanceProfile' {} a -> s {instanceProfileName = a} :: RemoveRoleFromInstanceProfile)

-- | The name of the role to remove.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
removeRoleFromInstanceProfile_roleName :: Lens.Lens' RemoveRoleFromInstanceProfile Prelude.Text
removeRoleFromInstanceProfile_roleName = Lens.lens (\RemoveRoleFromInstanceProfile' {roleName} -> roleName) (\s@RemoveRoleFromInstanceProfile' {} a -> s {roleName = a} :: RemoveRoleFromInstanceProfile)

instance
  Prelude.AWSRequest
    RemoveRoleFromInstanceProfile
  where
  type
    Rs RemoveRoleFromInstanceProfile =
      RemoveRoleFromInstanceProfileResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull
      RemoveRoleFromInstanceProfileResponse'

instance
  Prelude.Hashable
    RemoveRoleFromInstanceProfile

instance Prelude.NFData RemoveRoleFromInstanceProfile

instance
  Prelude.ToHeaders
    RemoveRoleFromInstanceProfile
  where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath RemoveRoleFromInstanceProfile where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
    RemoveRoleFromInstanceProfile
  where
  toQuery RemoveRoleFromInstanceProfile' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ( "RemoveRoleFromInstanceProfile" ::
                         Prelude.ByteString
                     ),
        "Version"
          Prelude.=: ("2010-05-08" :: Prelude.ByteString),
        "InstanceProfileName" Prelude.=: instanceProfileName,
        "RoleName" Prelude.=: roleName
      ]

-- | /See:/ 'newRemoveRoleFromInstanceProfileResponse' smart constructor.
data RemoveRoleFromInstanceProfileResponse = RemoveRoleFromInstanceProfileResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RemoveRoleFromInstanceProfileResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newRemoveRoleFromInstanceProfileResponse ::
  RemoveRoleFromInstanceProfileResponse
newRemoveRoleFromInstanceProfileResponse =
  RemoveRoleFromInstanceProfileResponse'

instance
  Prelude.NFData
    RemoveRoleFromInstanceProfileResponse
