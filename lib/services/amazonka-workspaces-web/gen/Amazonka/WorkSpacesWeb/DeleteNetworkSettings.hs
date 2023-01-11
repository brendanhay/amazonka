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
-- Module      : Amazonka.WorkSpacesWeb.DeleteNetworkSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes network settings.
module Amazonka.WorkSpacesWeb.DeleteNetworkSettings
  ( -- * Creating a Request
    DeleteNetworkSettings (..),
    newDeleteNetworkSettings,

    -- * Request Lenses
    deleteNetworkSettings_networkSettingsArn,

    -- * Destructuring the Response
    DeleteNetworkSettingsResponse (..),
    newDeleteNetworkSettingsResponse,

    -- * Response Lenses
    deleteNetworkSettingsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkSpacesWeb.Types

-- | /See:/ 'newDeleteNetworkSettings' smart constructor.
data DeleteNetworkSettings = DeleteNetworkSettings'
  { -- | The ARN of the network settings.
    networkSettingsArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteNetworkSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'networkSettingsArn', 'deleteNetworkSettings_networkSettingsArn' - The ARN of the network settings.
newDeleteNetworkSettings ::
  -- | 'networkSettingsArn'
  Prelude.Text ->
  DeleteNetworkSettings
newDeleteNetworkSettings pNetworkSettingsArn_ =
  DeleteNetworkSettings'
    { networkSettingsArn =
        pNetworkSettingsArn_
    }

-- | The ARN of the network settings.
deleteNetworkSettings_networkSettingsArn :: Lens.Lens' DeleteNetworkSettings Prelude.Text
deleteNetworkSettings_networkSettingsArn = Lens.lens (\DeleteNetworkSettings' {networkSettingsArn} -> networkSettingsArn) (\s@DeleteNetworkSettings' {} a -> s {networkSettingsArn = a} :: DeleteNetworkSettings)

instance Core.AWSRequest DeleteNetworkSettings where
  type
    AWSResponse DeleteNetworkSettings =
      DeleteNetworkSettingsResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteNetworkSettingsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteNetworkSettings where
  hashWithSalt _salt DeleteNetworkSettings' {..} =
    _salt `Prelude.hashWithSalt` networkSettingsArn

instance Prelude.NFData DeleteNetworkSettings where
  rnf DeleteNetworkSettings' {..} =
    Prelude.rnf networkSettingsArn

instance Data.ToHeaders DeleteNetworkSettings where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteNetworkSettings where
  toPath DeleteNetworkSettings' {..} =
    Prelude.mconcat
      ["/networkSettings/", Data.toBS networkSettingsArn]

instance Data.ToQuery DeleteNetworkSettings where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteNetworkSettingsResponse' smart constructor.
data DeleteNetworkSettingsResponse = DeleteNetworkSettingsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteNetworkSettingsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteNetworkSettingsResponse_httpStatus' - The response's http status code.
newDeleteNetworkSettingsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteNetworkSettingsResponse
newDeleteNetworkSettingsResponse pHttpStatus_ =
  DeleteNetworkSettingsResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteNetworkSettingsResponse_httpStatus :: Lens.Lens' DeleteNetworkSettingsResponse Prelude.Int
deleteNetworkSettingsResponse_httpStatus = Lens.lens (\DeleteNetworkSettingsResponse' {httpStatus} -> httpStatus) (\s@DeleteNetworkSettingsResponse' {} a -> s {httpStatus = a} :: DeleteNetworkSettingsResponse)

instance Prelude.NFData DeleteNetworkSettingsResponse where
  rnf DeleteNetworkSettingsResponse' {..} =
    Prelude.rnf httpStatus
