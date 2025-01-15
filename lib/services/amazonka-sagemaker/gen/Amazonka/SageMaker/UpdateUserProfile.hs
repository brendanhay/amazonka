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
-- Module      : Amazonka.SageMaker.UpdateUserProfile
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a user profile.
module Amazonka.SageMaker.UpdateUserProfile
  ( -- * Creating a Request
    UpdateUserProfile (..),
    newUpdateUserProfile,

    -- * Request Lenses
    updateUserProfile_userSettings,
    updateUserProfile_domainId,
    updateUserProfile_userProfileName,

    -- * Destructuring the Response
    UpdateUserProfileResponse (..),
    newUpdateUserProfileResponse,

    -- * Response Lenses
    updateUserProfileResponse_userProfileArn,
    updateUserProfileResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newUpdateUserProfile' smart constructor.
data UpdateUserProfile = UpdateUserProfile'
  { -- | A collection of settings.
    userSettings :: Prelude.Maybe UserSettings,
    -- | The domain ID.
    domainId :: Prelude.Text,
    -- | The user profile name.
    userProfileName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateUserProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userSettings', 'updateUserProfile_userSettings' - A collection of settings.
--
-- 'domainId', 'updateUserProfile_domainId' - The domain ID.
--
-- 'userProfileName', 'updateUserProfile_userProfileName' - The user profile name.
newUpdateUserProfile ::
  -- | 'domainId'
  Prelude.Text ->
  -- | 'userProfileName'
  Prelude.Text ->
  UpdateUserProfile
newUpdateUserProfile pDomainId_ pUserProfileName_ =
  UpdateUserProfile'
    { userSettings = Prelude.Nothing,
      domainId = pDomainId_,
      userProfileName = pUserProfileName_
    }

-- | A collection of settings.
updateUserProfile_userSettings :: Lens.Lens' UpdateUserProfile (Prelude.Maybe UserSettings)
updateUserProfile_userSettings = Lens.lens (\UpdateUserProfile' {userSettings} -> userSettings) (\s@UpdateUserProfile' {} a -> s {userSettings = a} :: UpdateUserProfile)

-- | The domain ID.
updateUserProfile_domainId :: Lens.Lens' UpdateUserProfile Prelude.Text
updateUserProfile_domainId = Lens.lens (\UpdateUserProfile' {domainId} -> domainId) (\s@UpdateUserProfile' {} a -> s {domainId = a} :: UpdateUserProfile)

-- | The user profile name.
updateUserProfile_userProfileName :: Lens.Lens' UpdateUserProfile Prelude.Text
updateUserProfile_userProfileName = Lens.lens (\UpdateUserProfile' {userProfileName} -> userProfileName) (\s@UpdateUserProfile' {} a -> s {userProfileName = a} :: UpdateUserProfile)

instance Core.AWSRequest UpdateUserProfile where
  type
    AWSResponse UpdateUserProfile =
      UpdateUserProfileResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateUserProfileResponse'
            Prelude.<$> (x Data..?> "UserProfileArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateUserProfile where
  hashWithSalt _salt UpdateUserProfile' {..} =
    _salt
      `Prelude.hashWithSalt` userSettings
      `Prelude.hashWithSalt` domainId
      `Prelude.hashWithSalt` userProfileName

instance Prelude.NFData UpdateUserProfile where
  rnf UpdateUserProfile' {..} =
    Prelude.rnf userSettings `Prelude.seq`
      Prelude.rnf domainId `Prelude.seq`
        Prelude.rnf userProfileName

instance Data.ToHeaders UpdateUserProfile where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SageMaker.UpdateUserProfile" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateUserProfile where
  toJSON UpdateUserProfile' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("UserSettings" Data..=) Prelude.<$> userSettings,
            Prelude.Just ("DomainId" Data..= domainId),
            Prelude.Just
              ("UserProfileName" Data..= userProfileName)
          ]
      )

instance Data.ToPath UpdateUserProfile where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateUserProfile where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateUserProfileResponse' smart constructor.
data UpdateUserProfileResponse = UpdateUserProfileResponse'
  { -- | The user profile Amazon Resource Name (ARN).
    userProfileArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateUserProfileResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userProfileArn', 'updateUserProfileResponse_userProfileArn' - The user profile Amazon Resource Name (ARN).
--
-- 'httpStatus', 'updateUserProfileResponse_httpStatus' - The response's http status code.
newUpdateUserProfileResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateUserProfileResponse
newUpdateUserProfileResponse pHttpStatus_ =
  UpdateUserProfileResponse'
    { userProfileArn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The user profile Amazon Resource Name (ARN).
updateUserProfileResponse_userProfileArn :: Lens.Lens' UpdateUserProfileResponse (Prelude.Maybe Prelude.Text)
updateUserProfileResponse_userProfileArn = Lens.lens (\UpdateUserProfileResponse' {userProfileArn} -> userProfileArn) (\s@UpdateUserProfileResponse' {} a -> s {userProfileArn = a} :: UpdateUserProfileResponse)

-- | The response's http status code.
updateUserProfileResponse_httpStatus :: Lens.Lens' UpdateUserProfileResponse Prelude.Int
updateUserProfileResponse_httpStatus = Lens.lens (\UpdateUserProfileResponse' {httpStatus} -> httpStatus) (\s@UpdateUserProfileResponse' {} a -> s {httpStatus = a} :: UpdateUserProfileResponse)

instance Prelude.NFData UpdateUserProfileResponse where
  rnf UpdateUserProfileResponse' {..} =
    Prelude.rnf userProfileArn `Prelude.seq`
      Prelude.rnf httpStatus
