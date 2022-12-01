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
-- Module      : Amazonka.ChimeSDKIdentity.UpdateAppInstanceUser
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the details of an @AppInstanceUser@. You can update names and
-- metadata.
module Amazonka.ChimeSDKIdentity.UpdateAppInstanceUser
  ( -- * Creating a Request
    UpdateAppInstanceUser (..),
    newUpdateAppInstanceUser,

    -- * Request Lenses
    updateAppInstanceUser_appInstanceUserArn,
    updateAppInstanceUser_name,
    updateAppInstanceUser_metadata,

    -- * Destructuring the Response
    UpdateAppInstanceUserResponse (..),
    newUpdateAppInstanceUserResponse,

    -- * Response Lenses
    updateAppInstanceUserResponse_appInstanceUserArn,
    updateAppInstanceUserResponse_httpStatus,
  )
where

import Amazonka.ChimeSDKIdentity.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateAppInstanceUser' smart constructor.
data UpdateAppInstanceUser = UpdateAppInstanceUser'
  { -- | The ARN of the @AppInstanceUser@.
    appInstanceUserArn :: Prelude.Text,
    -- | The name of the @AppInstanceUser@.
    name :: Core.Sensitive Prelude.Text,
    -- | The metadata of the @AppInstanceUser@.
    metadata :: Core.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateAppInstanceUser' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appInstanceUserArn', 'updateAppInstanceUser_appInstanceUserArn' - The ARN of the @AppInstanceUser@.
--
-- 'name', 'updateAppInstanceUser_name' - The name of the @AppInstanceUser@.
--
-- 'metadata', 'updateAppInstanceUser_metadata' - The metadata of the @AppInstanceUser@.
newUpdateAppInstanceUser ::
  -- | 'appInstanceUserArn'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'metadata'
  Prelude.Text ->
  UpdateAppInstanceUser
newUpdateAppInstanceUser
  pAppInstanceUserArn_
  pName_
  pMetadata_ =
    UpdateAppInstanceUser'
      { appInstanceUserArn =
          pAppInstanceUserArn_,
        name = Core._Sensitive Lens.# pName_,
        metadata = Core._Sensitive Lens.# pMetadata_
      }

-- | The ARN of the @AppInstanceUser@.
updateAppInstanceUser_appInstanceUserArn :: Lens.Lens' UpdateAppInstanceUser Prelude.Text
updateAppInstanceUser_appInstanceUserArn = Lens.lens (\UpdateAppInstanceUser' {appInstanceUserArn} -> appInstanceUserArn) (\s@UpdateAppInstanceUser' {} a -> s {appInstanceUserArn = a} :: UpdateAppInstanceUser)

-- | The name of the @AppInstanceUser@.
updateAppInstanceUser_name :: Lens.Lens' UpdateAppInstanceUser Prelude.Text
updateAppInstanceUser_name = Lens.lens (\UpdateAppInstanceUser' {name} -> name) (\s@UpdateAppInstanceUser' {} a -> s {name = a} :: UpdateAppInstanceUser) Prelude.. Core._Sensitive

-- | The metadata of the @AppInstanceUser@.
updateAppInstanceUser_metadata :: Lens.Lens' UpdateAppInstanceUser Prelude.Text
updateAppInstanceUser_metadata = Lens.lens (\UpdateAppInstanceUser' {metadata} -> metadata) (\s@UpdateAppInstanceUser' {} a -> s {metadata = a} :: UpdateAppInstanceUser) Prelude.. Core._Sensitive

instance Core.AWSRequest UpdateAppInstanceUser where
  type
    AWSResponse UpdateAppInstanceUser =
      UpdateAppInstanceUserResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateAppInstanceUserResponse'
            Prelude.<$> (x Core..?> "AppInstanceUserArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateAppInstanceUser where
  hashWithSalt _salt UpdateAppInstanceUser' {..} =
    _salt `Prelude.hashWithSalt` appInstanceUserArn
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` metadata

instance Prelude.NFData UpdateAppInstanceUser where
  rnf UpdateAppInstanceUser' {..} =
    Prelude.rnf appInstanceUserArn
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf metadata

instance Core.ToHeaders UpdateAppInstanceUser where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToJSON UpdateAppInstanceUser where
  toJSON UpdateAppInstanceUser' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Name" Core..= name),
            Prelude.Just ("Metadata" Core..= metadata)
          ]
      )

instance Core.ToPath UpdateAppInstanceUser where
  toPath UpdateAppInstanceUser' {..} =
    Prelude.mconcat
      [ "/app-instance-users/",
        Core.toBS appInstanceUserArn
      ]

instance Core.ToQuery UpdateAppInstanceUser where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateAppInstanceUserResponse' smart constructor.
data UpdateAppInstanceUserResponse = UpdateAppInstanceUserResponse'
  { -- | The ARN of the @AppInstanceUser@.
    appInstanceUserArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateAppInstanceUserResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appInstanceUserArn', 'updateAppInstanceUserResponse_appInstanceUserArn' - The ARN of the @AppInstanceUser@.
--
-- 'httpStatus', 'updateAppInstanceUserResponse_httpStatus' - The response's http status code.
newUpdateAppInstanceUserResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateAppInstanceUserResponse
newUpdateAppInstanceUserResponse pHttpStatus_ =
  UpdateAppInstanceUserResponse'
    { appInstanceUserArn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the @AppInstanceUser@.
updateAppInstanceUserResponse_appInstanceUserArn :: Lens.Lens' UpdateAppInstanceUserResponse (Prelude.Maybe Prelude.Text)
updateAppInstanceUserResponse_appInstanceUserArn = Lens.lens (\UpdateAppInstanceUserResponse' {appInstanceUserArn} -> appInstanceUserArn) (\s@UpdateAppInstanceUserResponse' {} a -> s {appInstanceUserArn = a} :: UpdateAppInstanceUserResponse)

-- | The response's http status code.
updateAppInstanceUserResponse_httpStatus :: Lens.Lens' UpdateAppInstanceUserResponse Prelude.Int
updateAppInstanceUserResponse_httpStatus = Lens.lens (\UpdateAppInstanceUserResponse' {httpStatus} -> httpStatus) (\s@UpdateAppInstanceUserResponse' {} a -> s {httpStatus = a} :: UpdateAppInstanceUserResponse)

instance Prelude.NFData UpdateAppInstanceUserResponse where
  rnf UpdateAppInstanceUserResponse' {..} =
    Prelude.rnf appInstanceUserArn
      `Prelude.seq` Prelude.rnf httpStatus
