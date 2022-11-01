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
-- Module      : Amazonka.Connect.UpdateSecurityProfile
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This API is in preview release for Amazon Connect and is subject to
-- change.
--
-- Updates a security profile.
module Amazonka.Connect.UpdateSecurityProfile
  ( -- * Creating a Request
    UpdateSecurityProfile (..),
    newUpdateSecurityProfile,

    -- * Request Lenses
    updateSecurityProfile_permissions,
    updateSecurityProfile_description,
    updateSecurityProfile_securityProfileId,
    updateSecurityProfile_instanceId,

    -- * Destructuring the Response
    UpdateSecurityProfileResponse (..),
    newUpdateSecurityProfileResponse,
  )
where

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateSecurityProfile' smart constructor.
data UpdateSecurityProfile = UpdateSecurityProfile'
  { -- | The permissions granted to a security profile. For a list of valid
    -- permissions, see
    -- <https://docs.aws.amazon.com/connect/latest/adminguide/security-profile-list.html List of security profile permissions>.
    permissions :: Prelude.Maybe [Prelude.Text],
    -- | The description of the security profile.
    description :: Prelude.Maybe Prelude.Text,
    -- | The identifier for the security profle.
    securityProfileId :: Prelude.Text,
    -- | The identifier of the Amazon Connect instance. You can find the
    -- instanceId in the ARN of the instance.
    instanceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateSecurityProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'permissions', 'updateSecurityProfile_permissions' - The permissions granted to a security profile. For a list of valid
-- permissions, see
-- <https://docs.aws.amazon.com/connect/latest/adminguide/security-profile-list.html List of security profile permissions>.
--
-- 'description', 'updateSecurityProfile_description' - The description of the security profile.
--
-- 'securityProfileId', 'updateSecurityProfile_securityProfileId' - The identifier for the security profle.
--
-- 'instanceId', 'updateSecurityProfile_instanceId' - The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
newUpdateSecurityProfile ::
  -- | 'securityProfileId'
  Prelude.Text ->
  -- | 'instanceId'
  Prelude.Text ->
  UpdateSecurityProfile
newUpdateSecurityProfile
  pSecurityProfileId_
  pInstanceId_ =
    UpdateSecurityProfile'
      { permissions =
          Prelude.Nothing,
        description = Prelude.Nothing,
        securityProfileId = pSecurityProfileId_,
        instanceId = pInstanceId_
      }

-- | The permissions granted to a security profile. For a list of valid
-- permissions, see
-- <https://docs.aws.amazon.com/connect/latest/adminguide/security-profile-list.html List of security profile permissions>.
updateSecurityProfile_permissions :: Lens.Lens' UpdateSecurityProfile (Prelude.Maybe [Prelude.Text])
updateSecurityProfile_permissions = Lens.lens (\UpdateSecurityProfile' {permissions} -> permissions) (\s@UpdateSecurityProfile' {} a -> s {permissions = a} :: UpdateSecurityProfile) Prelude.. Lens.mapping Lens.coerced

-- | The description of the security profile.
updateSecurityProfile_description :: Lens.Lens' UpdateSecurityProfile (Prelude.Maybe Prelude.Text)
updateSecurityProfile_description = Lens.lens (\UpdateSecurityProfile' {description} -> description) (\s@UpdateSecurityProfile' {} a -> s {description = a} :: UpdateSecurityProfile)

-- | The identifier for the security profle.
updateSecurityProfile_securityProfileId :: Lens.Lens' UpdateSecurityProfile Prelude.Text
updateSecurityProfile_securityProfileId = Lens.lens (\UpdateSecurityProfile' {securityProfileId} -> securityProfileId) (\s@UpdateSecurityProfile' {} a -> s {securityProfileId = a} :: UpdateSecurityProfile)

-- | The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
updateSecurityProfile_instanceId :: Lens.Lens' UpdateSecurityProfile Prelude.Text
updateSecurityProfile_instanceId = Lens.lens (\UpdateSecurityProfile' {instanceId} -> instanceId) (\s@UpdateSecurityProfile' {} a -> s {instanceId = a} :: UpdateSecurityProfile)

instance Core.AWSRequest UpdateSecurityProfile where
  type
    AWSResponse UpdateSecurityProfile =
      UpdateSecurityProfileResponse
  service _ = defaultService
  request srv = Request.postJSON srv
  response =
    Response.receiveNull UpdateSecurityProfileResponse'

instance Prelude.Hashable UpdateSecurityProfile where
  hashWithSalt _salt UpdateSecurityProfile' {..} =
    _salt `Prelude.hashWithSalt` permissions
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` securityProfileId
      `Prelude.hashWithSalt` instanceId

instance Prelude.NFData UpdateSecurityProfile where
  rnf UpdateSecurityProfile' {..} =
    Prelude.rnf permissions
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf securityProfileId
      `Prelude.seq` Prelude.rnf instanceId

instance Core.ToHeaders UpdateSecurityProfile where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateSecurityProfile where
  toJSON UpdateSecurityProfile' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Permissions" Core..=) Prelude.<$> permissions,
            ("Description" Core..=) Prelude.<$> description
          ]
      )

instance Core.ToPath UpdateSecurityProfile where
  toPath UpdateSecurityProfile' {..} =
    Prelude.mconcat
      [ "/security-profiles/",
        Core.toBS instanceId,
        "/",
        Core.toBS securityProfileId
      ]

instance Core.ToQuery UpdateSecurityProfile where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateSecurityProfileResponse' smart constructor.
data UpdateSecurityProfileResponse = UpdateSecurityProfileResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateSecurityProfileResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdateSecurityProfileResponse ::
  UpdateSecurityProfileResponse
newUpdateSecurityProfileResponse =
  UpdateSecurityProfileResponse'

instance Prelude.NFData UpdateSecurityProfileResponse where
  rnf _ = ()
