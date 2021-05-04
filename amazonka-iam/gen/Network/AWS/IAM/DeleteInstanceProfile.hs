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
-- Module      : Network.AWS.IAM.DeleteInstanceProfile
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified instance profile. The instance profile must not
-- have an associated role.
--
-- Make sure that you do not have any Amazon EC2 instances running with the
-- instance profile you are about to delete. Deleting a role or instance
-- profile that is associated with a running instance will break any
-- applications running on the instance.
--
-- For more information about instance profiles, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/AboutInstanceProfiles.html About instance profiles>.
module Network.AWS.IAM.DeleteInstanceProfile
  ( -- * Creating a Request
    DeleteInstanceProfile (..),
    newDeleteInstanceProfile,

    -- * Request Lenses
    deleteInstanceProfile_instanceProfileName,

    -- * Destructuring the Response
    DeleteInstanceProfileResponse (..),
    newDeleteInstanceProfileResponse,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteInstanceProfile' smart constructor.
data DeleteInstanceProfile = DeleteInstanceProfile'
  { -- | The name of the instance profile to delete.
    --
    -- This parameter allows (through its
    -- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
    -- consisting of upper and lowercase alphanumeric characters with no
    -- spaces. You can also include any of the following characters: _+=,.\@-
    instanceProfileName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteInstanceProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceProfileName', 'deleteInstanceProfile_instanceProfileName' - The name of the instance profile to delete.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
newDeleteInstanceProfile ::
  -- | 'instanceProfileName'
  Prelude.Text ->
  DeleteInstanceProfile
newDeleteInstanceProfile pInstanceProfileName_ =
  DeleteInstanceProfile'
    { instanceProfileName =
        pInstanceProfileName_
    }

-- | The name of the instance profile to delete.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
deleteInstanceProfile_instanceProfileName :: Lens.Lens' DeleteInstanceProfile Prelude.Text
deleteInstanceProfile_instanceProfileName = Lens.lens (\DeleteInstanceProfile' {instanceProfileName} -> instanceProfileName) (\s@DeleteInstanceProfile' {} a -> s {instanceProfileName = a} :: DeleteInstanceProfile)

instance Prelude.AWSRequest DeleteInstanceProfile where
  type
    Rs DeleteInstanceProfile =
      DeleteInstanceProfileResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull DeleteInstanceProfileResponse'

instance Prelude.Hashable DeleteInstanceProfile

instance Prelude.NFData DeleteInstanceProfile

instance Prelude.ToHeaders DeleteInstanceProfile where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DeleteInstanceProfile where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteInstanceProfile where
  toQuery DeleteInstanceProfile' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("DeleteInstanceProfile" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2010-05-08" :: Prelude.ByteString),
        "InstanceProfileName" Prelude.=: instanceProfileName
      ]

-- | /See:/ 'newDeleteInstanceProfileResponse' smart constructor.
data DeleteInstanceProfileResponse = DeleteInstanceProfileResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteInstanceProfileResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteInstanceProfileResponse ::
  DeleteInstanceProfileResponse
newDeleteInstanceProfileResponse =
  DeleteInstanceProfileResponse'

instance Prelude.NFData DeleteInstanceProfileResponse
