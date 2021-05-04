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
-- Module      : Network.AWS.OpsWorks.UpdateMyUserProfile
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a user\'s SSH public key.
--
-- __Required Permissions__: To use this action, an IAM user must have
-- self-management enabled or an attached policy that explicitly grants
-- permissions. For more information about user permissions, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
module Network.AWS.OpsWorks.UpdateMyUserProfile
  ( -- * Creating a Request
    UpdateMyUserProfile (..),
    newUpdateMyUserProfile,

    -- * Request Lenses
    updateMyUserProfile_sshPublicKey,

    -- * Destructuring the Response
    UpdateMyUserProfileResponse (..),
    newUpdateMyUserProfileResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateMyUserProfile' smart constructor.
data UpdateMyUserProfile = UpdateMyUserProfile'
  { -- | The user\'s SSH public key.
    sshPublicKey :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateMyUserProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sshPublicKey', 'updateMyUserProfile_sshPublicKey' - The user\'s SSH public key.
newUpdateMyUserProfile ::
  UpdateMyUserProfile
newUpdateMyUserProfile =
  UpdateMyUserProfile'
    { sshPublicKey =
        Prelude.Nothing
    }

-- | The user\'s SSH public key.
updateMyUserProfile_sshPublicKey :: Lens.Lens' UpdateMyUserProfile (Prelude.Maybe Prelude.Text)
updateMyUserProfile_sshPublicKey = Lens.lens (\UpdateMyUserProfile' {sshPublicKey} -> sshPublicKey) (\s@UpdateMyUserProfile' {} a -> s {sshPublicKey = a} :: UpdateMyUserProfile)

instance Prelude.AWSRequest UpdateMyUserProfile where
  type
    Rs UpdateMyUserProfile =
      UpdateMyUserProfileResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull UpdateMyUserProfileResponse'

instance Prelude.Hashable UpdateMyUserProfile

instance Prelude.NFData UpdateMyUserProfile

instance Prelude.ToHeaders UpdateMyUserProfile where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "OpsWorks_20130218.UpdateMyUserProfile" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON UpdateMyUserProfile where
  toJSON UpdateMyUserProfile' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("SshPublicKey" Prelude..=)
              Prelude.<$> sshPublicKey
          ]
      )

instance Prelude.ToPath UpdateMyUserProfile where
  toPath = Prelude.const "/"

instance Prelude.ToQuery UpdateMyUserProfile where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateMyUserProfileResponse' smart constructor.
data UpdateMyUserProfileResponse = UpdateMyUserProfileResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateMyUserProfileResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdateMyUserProfileResponse ::
  UpdateMyUserProfileResponse
newUpdateMyUserProfileResponse =
  UpdateMyUserProfileResponse'

instance Prelude.NFData UpdateMyUserProfileResponse
