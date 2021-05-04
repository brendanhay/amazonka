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
-- Module      : Network.AWS.IAM.AddRoleToInstanceProfile
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds the specified IAM role to the specified instance profile. An
-- instance profile can contain only one role, and this quota cannot be
-- increased. You can remove the existing role and then add a different
-- role to an instance profile. You must then wait for the change to appear
-- across all of AWS because of
-- <https://en.wikipedia.org/wiki/Eventual_consistency eventual consistency>.
-- To force the change, you must
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DisassociateIamInstanceProfile.html disassociate the instance profile>
-- and then
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_AssociateIamInstanceProfile.html associate the instance profile>,
-- or you can stop your instance and then restart it.
--
-- The caller of this operation must be granted the @PassRole@ permission
-- on the IAM role by a permissions policy.
--
-- For more information about roles, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/WorkingWithRoles.html Working with roles>.
-- For more information about instance profiles, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/AboutInstanceProfiles.html About instance profiles>.
module Network.AWS.IAM.AddRoleToInstanceProfile
  ( -- * Creating a Request
    AddRoleToInstanceProfile (..),
    newAddRoleToInstanceProfile,

    -- * Request Lenses
    addRoleToInstanceProfile_instanceProfileName,
    addRoleToInstanceProfile_roleName,

    -- * Destructuring the Response
    AddRoleToInstanceProfileResponse (..),
    newAddRoleToInstanceProfileResponse,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newAddRoleToInstanceProfile' smart constructor.
data AddRoleToInstanceProfile = AddRoleToInstanceProfile'
  { -- | The name of the instance profile to update.
    --
    -- This parameter allows (through its
    -- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
    -- consisting of upper and lowercase alphanumeric characters with no
    -- spaces. You can also include any of the following characters: _+=,.\@-
    instanceProfileName :: Prelude.Text,
    -- | The name of the role to add.
    --
    -- This parameter allows (through its
    -- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
    -- consisting of upper and lowercase alphanumeric characters with no
    -- spaces. You can also include any of the following characters: _+=,.\@-
    roleName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AddRoleToInstanceProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceProfileName', 'addRoleToInstanceProfile_instanceProfileName' - The name of the instance profile to update.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
--
-- 'roleName', 'addRoleToInstanceProfile_roleName' - The name of the role to add.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
newAddRoleToInstanceProfile ::
  -- | 'instanceProfileName'
  Prelude.Text ->
  -- | 'roleName'
  Prelude.Text ->
  AddRoleToInstanceProfile
newAddRoleToInstanceProfile
  pInstanceProfileName_
  pRoleName_ =
    AddRoleToInstanceProfile'
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
addRoleToInstanceProfile_instanceProfileName :: Lens.Lens' AddRoleToInstanceProfile Prelude.Text
addRoleToInstanceProfile_instanceProfileName = Lens.lens (\AddRoleToInstanceProfile' {instanceProfileName} -> instanceProfileName) (\s@AddRoleToInstanceProfile' {} a -> s {instanceProfileName = a} :: AddRoleToInstanceProfile)

-- | The name of the role to add.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
addRoleToInstanceProfile_roleName :: Lens.Lens' AddRoleToInstanceProfile Prelude.Text
addRoleToInstanceProfile_roleName = Lens.lens (\AddRoleToInstanceProfile' {roleName} -> roleName) (\s@AddRoleToInstanceProfile' {} a -> s {roleName = a} :: AddRoleToInstanceProfile)

instance Prelude.AWSRequest AddRoleToInstanceProfile where
  type
    Rs AddRoleToInstanceProfile =
      AddRoleToInstanceProfileResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull
      AddRoleToInstanceProfileResponse'

instance Prelude.Hashable AddRoleToInstanceProfile

instance Prelude.NFData AddRoleToInstanceProfile

instance Prelude.ToHeaders AddRoleToInstanceProfile where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath AddRoleToInstanceProfile where
  toPath = Prelude.const "/"

instance Prelude.ToQuery AddRoleToInstanceProfile where
  toQuery AddRoleToInstanceProfile' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("AddRoleToInstanceProfile" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2010-05-08" :: Prelude.ByteString),
        "InstanceProfileName" Prelude.=: instanceProfileName,
        "RoleName" Prelude.=: roleName
      ]

-- | /See:/ 'newAddRoleToInstanceProfileResponse' smart constructor.
data AddRoleToInstanceProfileResponse = AddRoleToInstanceProfileResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AddRoleToInstanceProfileResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newAddRoleToInstanceProfileResponse ::
  AddRoleToInstanceProfileResponse
newAddRoleToInstanceProfileResponse =
  AddRoleToInstanceProfileResponse'

instance
  Prelude.NFData
    AddRoleToInstanceProfileResponse
