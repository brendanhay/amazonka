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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateMyUserProfile' smart constructor.
data UpdateMyUserProfile = UpdateMyUserProfile'
  { -- | The user\'s SSH public key.
    sshPublicKey :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  UpdateMyUserProfile' {sshPublicKey = Core.Nothing}

-- | The user\'s SSH public key.
updateMyUserProfile_sshPublicKey :: Lens.Lens' UpdateMyUserProfile (Core.Maybe Core.Text)
updateMyUserProfile_sshPublicKey = Lens.lens (\UpdateMyUserProfile' {sshPublicKey} -> sshPublicKey) (\s@UpdateMyUserProfile' {} a -> s {sshPublicKey = a} :: UpdateMyUserProfile)

instance Core.AWSRequest UpdateMyUserProfile where
  type
    AWSResponse UpdateMyUserProfile =
      UpdateMyUserProfileResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull UpdateMyUserProfileResponse'

instance Core.Hashable UpdateMyUserProfile

instance Core.NFData UpdateMyUserProfile

instance Core.ToHeaders UpdateMyUserProfile where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "OpsWorks_20130218.UpdateMyUserProfile" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateMyUserProfile where
  toJSON UpdateMyUserProfile' {..} =
    Core.object
      ( Core.catMaybes
          [("SshPublicKey" Core..=) Core.<$> sshPublicKey]
      )

instance Core.ToPath UpdateMyUserProfile where
  toPath = Core.const "/"

instance Core.ToQuery UpdateMyUserProfile where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateMyUserProfileResponse' smart constructor.
data UpdateMyUserProfileResponse = UpdateMyUserProfileResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateMyUserProfileResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdateMyUserProfileResponse ::
  UpdateMyUserProfileResponse
newUpdateMyUserProfileResponse =
  UpdateMyUserProfileResponse'

instance Core.NFData UpdateMyUserProfileResponse
