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
-- Module      : Amazonka.WorkSpacesWeb.DeleteUserSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes user settings.
module Amazonka.WorkSpacesWeb.DeleteUserSettings
  ( -- * Creating a Request
    DeleteUserSettings (..),
    newDeleteUserSettings,

    -- * Request Lenses
    deleteUserSettings_userSettingsArn,

    -- * Destructuring the Response
    DeleteUserSettingsResponse (..),
    newDeleteUserSettingsResponse,

    -- * Response Lenses
    deleteUserSettingsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkSpacesWeb.Types

-- | /See:/ 'newDeleteUserSettings' smart constructor.
data DeleteUserSettings = DeleteUserSettings'
  { -- | The ARN of the user settings.
    userSettingsArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteUserSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userSettingsArn', 'deleteUserSettings_userSettingsArn' - The ARN of the user settings.
newDeleteUserSettings ::
  -- | 'userSettingsArn'
  Prelude.Text ->
  DeleteUserSettings
newDeleteUserSettings pUserSettingsArn_ =
  DeleteUserSettings'
    { userSettingsArn =
        pUserSettingsArn_
    }

-- | The ARN of the user settings.
deleteUserSettings_userSettingsArn :: Lens.Lens' DeleteUserSettings Prelude.Text
deleteUserSettings_userSettingsArn = Lens.lens (\DeleteUserSettings' {userSettingsArn} -> userSettingsArn) (\s@DeleteUserSettings' {} a -> s {userSettingsArn = a} :: DeleteUserSettings)

instance Core.AWSRequest DeleteUserSettings where
  type
    AWSResponse DeleteUserSettings =
      DeleteUserSettingsResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteUserSettingsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteUserSettings where
  hashWithSalt _salt DeleteUserSettings' {..} =
    _salt `Prelude.hashWithSalt` userSettingsArn

instance Prelude.NFData DeleteUserSettings where
  rnf DeleteUserSettings' {..} =
    Prelude.rnf userSettingsArn

instance Data.ToHeaders DeleteUserSettings where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteUserSettings where
  toPath DeleteUserSettings' {..} =
    Prelude.mconcat
      ["/userSettings/", Data.toBS userSettingsArn]

instance Data.ToQuery DeleteUserSettings where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteUserSettingsResponse' smart constructor.
data DeleteUserSettingsResponse = DeleteUserSettingsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteUserSettingsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteUserSettingsResponse_httpStatus' - The response's http status code.
newDeleteUserSettingsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteUserSettingsResponse
newDeleteUserSettingsResponse pHttpStatus_ =
  DeleteUserSettingsResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteUserSettingsResponse_httpStatus :: Lens.Lens' DeleteUserSettingsResponse Prelude.Int
deleteUserSettingsResponse_httpStatus = Lens.lens (\DeleteUserSettingsResponse' {httpStatus} -> httpStatus) (\s@DeleteUserSettingsResponse' {} a -> s {httpStatus = a} :: DeleteUserSettingsResponse)

instance Prelude.NFData DeleteUserSettingsResponse where
  rnf DeleteUserSettingsResponse' {..} =
    Prelude.rnf httpStatus
