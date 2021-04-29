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
-- Module      : Network.AWS.SageMaker.UpdateUserProfile
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a user profile.
module Network.AWS.SageMaker.UpdateUserProfile
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newUpdateUserProfile' smart constructor.
data UpdateUserProfile = UpdateUserProfile'
  { -- | A collection of settings.
    userSettings :: Prelude.Maybe UserSettings,
    -- | The domain ID.
    domainId :: Prelude.Text,
    -- | The user profile name.
    userProfileName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest UpdateUserProfile where
  type Rs UpdateUserProfile = UpdateUserProfileResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateUserProfileResponse'
            Prelude.<$> (x Prelude..?> "UserProfileArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateUserProfile

instance Prelude.NFData UpdateUserProfile

instance Prelude.ToHeaders UpdateUserProfile where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "SageMaker.UpdateUserProfile" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON UpdateUserProfile where
  toJSON UpdateUserProfile' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("UserSettings" Prelude..=)
              Prelude.<$> userSettings,
            Prelude.Just ("DomainId" Prelude..= domainId),
            Prelude.Just
              ("UserProfileName" Prelude..= userProfileName)
          ]
      )

instance Prelude.ToPath UpdateUserProfile where
  toPath = Prelude.const "/"

instance Prelude.ToQuery UpdateUserProfile where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateUserProfileResponse' smart constructor.
data UpdateUserProfileResponse = UpdateUserProfileResponse'
  { -- | The user profile Amazon Resource Name (ARN).
    userProfileArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.NFData UpdateUserProfileResponse
