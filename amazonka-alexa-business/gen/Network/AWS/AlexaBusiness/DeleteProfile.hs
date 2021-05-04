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
-- Module      : Network.AWS.AlexaBusiness.DeleteProfile
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a room profile by the profile ARN.
module Network.AWS.AlexaBusiness.DeleteProfile
  ( -- * Creating a Request
    DeleteProfile (..),
    newDeleteProfile,

    -- * Request Lenses
    deleteProfile_profileArn,

    -- * Destructuring the Response
    DeleteProfileResponse (..),
    newDeleteProfileResponse,

    -- * Response Lenses
    deleteProfileResponse_httpStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteProfile' smart constructor.
data DeleteProfile = DeleteProfile'
  { -- | The ARN of the room profile to delete. Required.
    profileArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'profileArn', 'deleteProfile_profileArn' - The ARN of the room profile to delete. Required.
newDeleteProfile ::
  DeleteProfile
newDeleteProfile =
  DeleteProfile' {profileArn = Prelude.Nothing}

-- | The ARN of the room profile to delete. Required.
deleteProfile_profileArn :: Lens.Lens' DeleteProfile (Prelude.Maybe Prelude.Text)
deleteProfile_profileArn = Lens.lens (\DeleteProfile' {profileArn} -> profileArn) (\s@DeleteProfile' {} a -> s {profileArn = a} :: DeleteProfile)

instance Prelude.AWSRequest DeleteProfile where
  type Rs DeleteProfile = DeleteProfileResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteProfileResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteProfile

instance Prelude.NFData DeleteProfile

instance Prelude.ToHeaders DeleteProfile where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AlexaForBusiness.DeleteProfile" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteProfile where
  toJSON DeleteProfile' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [("ProfileArn" Prelude..=) Prelude.<$> profileArn]
      )

instance Prelude.ToPath DeleteProfile where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteProfile where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteProfileResponse' smart constructor.
data DeleteProfileResponse = DeleteProfileResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteProfileResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteProfileResponse_httpStatus' - The response's http status code.
newDeleteProfileResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteProfileResponse
newDeleteProfileResponse pHttpStatus_ =
  DeleteProfileResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteProfileResponse_httpStatus :: Lens.Lens' DeleteProfileResponse Prelude.Int
deleteProfileResponse_httpStatus = Lens.lens (\DeleteProfileResponse' {httpStatus} -> httpStatus) (\s@DeleteProfileResponse' {} a -> s {httpStatus = a} :: DeleteProfileResponse)

instance Prelude.NFData DeleteProfileResponse
