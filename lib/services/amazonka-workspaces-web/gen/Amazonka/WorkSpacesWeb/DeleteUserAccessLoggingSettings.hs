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
-- Module      : Amazonka.WorkSpacesWeb.DeleteUserAccessLoggingSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes user access logging settings.
module Amazonka.WorkSpacesWeb.DeleteUserAccessLoggingSettings
  ( -- * Creating a Request
    DeleteUserAccessLoggingSettings (..),
    newDeleteUserAccessLoggingSettings,

    -- * Request Lenses
    deleteUserAccessLoggingSettings_userAccessLoggingSettingsArn,

    -- * Destructuring the Response
    DeleteUserAccessLoggingSettingsResponse (..),
    newDeleteUserAccessLoggingSettingsResponse,

    -- * Response Lenses
    deleteUserAccessLoggingSettingsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkSpacesWeb.Types

-- | /See:/ 'newDeleteUserAccessLoggingSettings' smart constructor.
data DeleteUserAccessLoggingSettings = DeleteUserAccessLoggingSettings'
  { -- | The ARN of the user access logging settings.
    userAccessLoggingSettingsArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteUserAccessLoggingSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userAccessLoggingSettingsArn', 'deleteUserAccessLoggingSettings_userAccessLoggingSettingsArn' - The ARN of the user access logging settings.
newDeleteUserAccessLoggingSettings ::
  -- | 'userAccessLoggingSettingsArn'
  Prelude.Text ->
  DeleteUserAccessLoggingSettings
newDeleteUserAccessLoggingSettings
  pUserAccessLoggingSettingsArn_ =
    DeleteUserAccessLoggingSettings'
      { userAccessLoggingSettingsArn =
          pUserAccessLoggingSettingsArn_
      }

-- | The ARN of the user access logging settings.
deleteUserAccessLoggingSettings_userAccessLoggingSettingsArn :: Lens.Lens' DeleteUserAccessLoggingSettings Prelude.Text
deleteUserAccessLoggingSettings_userAccessLoggingSettingsArn = Lens.lens (\DeleteUserAccessLoggingSettings' {userAccessLoggingSettingsArn} -> userAccessLoggingSettingsArn) (\s@DeleteUserAccessLoggingSettings' {} a -> s {userAccessLoggingSettingsArn = a} :: DeleteUserAccessLoggingSettings)

instance
  Core.AWSRequest
    DeleteUserAccessLoggingSettings
  where
  type
    AWSResponse DeleteUserAccessLoggingSettings =
      DeleteUserAccessLoggingSettingsResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteUserAccessLoggingSettingsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DeleteUserAccessLoggingSettings
  where
  hashWithSalt
    _salt
    DeleteUserAccessLoggingSettings' {..} =
      _salt
        `Prelude.hashWithSalt` userAccessLoggingSettingsArn

instance
  Prelude.NFData
    DeleteUserAccessLoggingSettings
  where
  rnf DeleteUserAccessLoggingSettings' {..} =
    Prelude.rnf userAccessLoggingSettingsArn

instance
  Data.ToHeaders
    DeleteUserAccessLoggingSettings
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteUserAccessLoggingSettings where
  toPath DeleteUserAccessLoggingSettings' {..} =
    Prelude.mconcat
      [ "/userAccessLoggingSettings/",
        Data.toBS userAccessLoggingSettingsArn
      ]

instance Data.ToQuery DeleteUserAccessLoggingSettings where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteUserAccessLoggingSettingsResponse' smart constructor.
data DeleteUserAccessLoggingSettingsResponse = DeleteUserAccessLoggingSettingsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteUserAccessLoggingSettingsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteUserAccessLoggingSettingsResponse_httpStatus' - The response's http status code.
newDeleteUserAccessLoggingSettingsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteUserAccessLoggingSettingsResponse
newDeleteUserAccessLoggingSettingsResponse
  pHttpStatus_ =
    DeleteUserAccessLoggingSettingsResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
deleteUserAccessLoggingSettingsResponse_httpStatus :: Lens.Lens' DeleteUserAccessLoggingSettingsResponse Prelude.Int
deleteUserAccessLoggingSettingsResponse_httpStatus = Lens.lens (\DeleteUserAccessLoggingSettingsResponse' {httpStatus} -> httpStatus) (\s@DeleteUserAccessLoggingSettingsResponse' {} a -> s {httpStatus = a} :: DeleteUserAccessLoggingSettingsResponse)

instance
  Prelude.NFData
    DeleteUserAccessLoggingSettingsResponse
  where
  rnf DeleteUserAccessLoggingSettingsResponse' {..} =
    Prelude.rnf httpStatus
