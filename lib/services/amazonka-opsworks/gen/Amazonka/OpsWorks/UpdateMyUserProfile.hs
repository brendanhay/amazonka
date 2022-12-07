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
-- Module      : Amazonka.OpsWorks.UpdateMyUserProfile
-- Copyright   : (c) 2013-2022 Brendan Hay
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
module Amazonka.OpsWorks.UpdateMyUserProfile
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpsWorks.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateMyUserProfile' smart constructor.
data UpdateMyUserProfile = UpdateMyUserProfile'
  { -- | The user\'s SSH public key.
    sshPublicKey :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Core.AWSRequest UpdateMyUserProfile where
  type
    AWSResponse UpdateMyUserProfile =
      UpdateMyUserProfileResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull UpdateMyUserProfileResponse'

instance Prelude.Hashable UpdateMyUserProfile where
  hashWithSalt _salt UpdateMyUserProfile' {..} =
    _salt `Prelude.hashWithSalt` sshPublicKey

instance Prelude.NFData UpdateMyUserProfile where
  rnf UpdateMyUserProfile' {..} =
    Prelude.rnf sshPublicKey

instance Data.ToHeaders UpdateMyUserProfile where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "OpsWorks_20130218.UpdateMyUserProfile" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateMyUserProfile where
  toJSON UpdateMyUserProfile' {..} =
    Data.object
      ( Prelude.catMaybes
          [("SshPublicKey" Data..=) Prelude.<$> sshPublicKey]
      )

instance Data.ToPath UpdateMyUserProfile where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateMyUserProfile where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateMyUserProfileResponse' smart constructor.
data UpdateMyUserProfileResponse = UpdateMyUserProfileResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateMyUserProfileResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdateMyUserProfileResponse ::
  UpdateMyUserProfileResponse
newUpdateMyUserProfileResponse =
  UpdateMyUserProfileResponse'

instance Prelude.NFData UpdateMyUserProfileResponse where
  rnf _ = ()
