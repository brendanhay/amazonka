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
-- Module      : Amazonka.Connect.UpdateUserSecurityProfiles
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Assigns the specified security profiles to the specified user.
module Amazonka.Connect.UpdateUserSecurityProfiles
  ( -- * Creating a Request
    UpdateUserSecurityProfiles (..),
    newUpdateUserSecurityProfiles,

    -- * Request Lenses
    updateUserSecurityProfiles_securityProfileIds,
    updateUserSecurityProfiles_userId,
    updateUserSecurityProfiles_instanceId,

    -- * Destructuring the Response
    UpdateUserSecurityProfilesResponse (..),
    newUpdateUserSecurityProfilesResponse,
  )
where

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateUserSecurityProfiles' smart constructor.
data UpdateUserSecurityProfiles = UpdateUserSecurityProfiles'
  { -- | The identifiers of the security profiles for the user.
    securityProfileIds :: Prelude.NonEmpty Prelude.Text,
    -- | The identifier of the user account.
    userId :: Prelude.Text,
    -- | The identifier of the Amazon Connect instance. You can find the
    -- instanceId in the ARN of the instance.
    instanceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateUserSecurityProfiles' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'securityProfileIds', 'updateUserSecurityProfiles_securityProfileIds' - The identifiers of the security profiles for the user.
--
-- 'userId', 'updateUserSecurityProfiles_userId' - The identifier of the user account.
--
-- 'instanceId', 'updateUserSecurityProfiles_instanceId' - The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
newUpdateUserSecurityProfiles ::
  -- | 'securityProfileIds'
  Prelude.NonEmpty Prelude.Text ->
  -- | 'userId'
  Prelude.Text ->
  -- | 'instanceId'
  Prelude.Text ->
  UpdateUserSecurityProfiles
newUpdateUserSecurityProfiles
  pSecurityProfileIds_
  pUserId_
  pInstanceId_ =
    UpdateUserSecurityProfiles'
      { securityProfileIds =
          Lens.coerced Lens.# pSecurityProfileIds_,
        userId = pUserId_,
        instanceId = pInstanceId_
      }

-- | The identifiers of the security profiles for the user.
updateUserSecurityProfiles_securityProfileIds :: Lens.Lens' UpdateUserSecurityProfiles (Prelude.NonEmpty Prelude.Text)
updateUserSecurityProfiles_securityProfileIds = Lens.lens (\UpdateUserSecurityProfiles' {securityProfileIds} -> securityProfileIds) (\s@UpdateUserSecurityProfiles' {} a -> s {securityProfileIds = a} :: UpdateUserSecurityProfiles) Prelude.. Lens.coerced

-- | The identifier of the user account.
updateUserSecurityProfiles_userId :: Lens.Lens' UpdateUserSecurityProfiles Prelude.Text
updateUserSecurityProfiles_userId = Lens.lens (\UpdateUserSecurityProfiles' {userId} -> userId) (\s@UpdateUserSecurityProfiles' {} a -> s {userId = a} :: UpdateUserSecurityProfiles)

-- | The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
updateUserSecurityProfiles_instanceId :: Lens.Lens' UpdateUserSecurityProfiles Prelude.Text
updateUserSecurityProfiles_instanceId = Lens.lens (\UpdateUserSecurityProfiles' {instanceId} -> instanceId) (\s@UpdateUserSecurityProfiles' {} a -> s {instanceId = a} :: UpdateUserSecurityProfiles)

instance Core.AWSRequest UpdateUserSecurityProfiles where
  type
    AWSResponse UpdateUserSecurityProfiles =
      UpdateUserSecurityProfilesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull
      UpdateUserSecurityProfilesResponse'

instance Prelude.Hashable UpdateUserSecurityProfiles where
  hashWithSalt _salt UpdateUserSecurityProfiles' {..} =
    _salt
      `Prelude.hashWithSalt` securityProfileIds
      `Prelude.hashWithSalt` userId
      `Prelude.hashWithSalt` instanceId

instance Prelude.NFData UpdateUserSecurityProfiles where
  rnf UpdateUserSecurityProfiles' {..} =
    Prelude.rnf securityProfileIds
      `Prelude.seq` Prelude.rnf userId
      `Prelude.seq` Prelude.rnf instanceId

instance Data.ToHeaders UpdateUserSecurityProfiles where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateUserSecurityProfiles where
  toJSON UpdateUserSecurityProfiles' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("SecurityProfileIds" Data..= securityProfileIds)
          ]
      )

instance Data.ToPath UpdateUserSecurityProfiles where
  toPath UpdateUserSecurityProfiles' {..} =
    Prelude.mconcat
      [ "/users/",
        Data.toBS instanceId,
        "/",
        Data.toBS userId,
        "/security-profiles"
      ]

instance Data.ToQuery UpdateUserSecurityProfiles where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateUserSecurityProfilesResponse' smart constructor.
data UpdateUserSecurityProfilesResponse = UpdateUserSecurityProfilesResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateUserSecurityProfilesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdateUserSecurityProfilesResponse ::
  UpdateUserSecurityProfilesResponse
newUpdateUserSecurityProfilesResponse =
  UpdateUserSecurityProfilesResponse'

instance
  Prelude.NFData
    UpdateUserSecurityProfilesResponse
  where
  rnf _ = ()
