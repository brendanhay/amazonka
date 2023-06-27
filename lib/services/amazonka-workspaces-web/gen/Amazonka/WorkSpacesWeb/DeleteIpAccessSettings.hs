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
-- Module      : Amazonka.WorkSpacesWeb.DeleteIpAccessSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes IP access settings.
module Amazonka.WorkSpacesWeb.DeleteIpAccessSettings
  ( -- * Creating a Request
    DeleteIpAccessSettings (..),
    newDeleteIpAccessSettings,

    -- * Request Lenses
    deleteIpAccessSettings_ipAccessSettingsArn,

    -- * Destructuring the Response
    DeleteIpAccessSettingsResponse (..),
    newDeleteIpAccessSettingsResponse,

    -- * Response Lenses
    deleteIpAccessSettingsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkSpacesWeb.Types

-- | /See:/ 'newDeleteIpAccessSettings' smart constructor.
data DeleteIpAccessSettings = DeleteIpAccessSettings'
  { -- | The ARN of the IP access settings.
    ipAccessSettingsArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteIpAccessSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ipAccessSettingsArn', 'deleteIpAccessSettings_ipAccessSettingsArn' - The ARN of the IP access settings.
newDeleteIpAccessSettings ::
  -- | 'ipAccessSettingsArn'
  Prelude.Text ->
  DeleteIpAccessSettings
newDeleteIpAccessSettings pIpAccessSettingsArn_ =
  DeleteIpAccessSettings'
    { ipAccessSettingsArn =
        pIpAccessSettingsArn_
    }

-- | The ARN of the IP access settings.
deleteIpAccessSettings_ipAccessSettingsArn :: Lens.Lens' DeleteIpAccessSettings Prelude.Text
deleteIpAccessSettings_ipAccessSettingsArn = Lens.lens (\DeleteIpAccessSettings' {ipAccessSettingsArn} -> ipAccessSettingsArn) (\s@DeleteIpAccessSettings' {} a -> s {ipAccessSettingsArn = a} :: DeleteIpAccessSettings)

instance Core.AWSRequest DeleteIpAccessSettings where
  type
    AWSResponse DeleteIpAccessSettings =
      DeleteIpAccessSettingsResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteIpAccessSettingsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteIpAccessSettings where
  hashWithSalt _salt DeleteIpAccessSettings' {..} =
    _salt `Prelude.hashWithSalt` ipAccessSettingsArn

instance Prelude.NFData DeleteIpAccessSettings where
  rnf DeleteIpAccessSettings' {..} =
    Prelude.rnf ipAccessSettingsArn

instance Data.ToHeaders DeleteIpAccessSettings where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteIpAccessSettings where
  toPath DeleteIpAccessSettings' {..} =
    Prelude.mconcat
      ["/ipAccessSettings/", Data.toBS ipAccessSettingsArn]

instance Data.ToQuery DeleteIpAccessSettings where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteIpAccessSettingsResponse' smart constructor.
data DeleteIpAccessSettingsResponse = DeleteIpAccessSettingsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteIpAccessSettingsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteIpAccessSettingsResponse_httpStatus' - The response's http status code.
newDeleteIpAccessSettingsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteIpAccessSettingsResponse
newDeleteIpAccessSettingsResponse pHttpStatus_ =
  DeleteIpAccessSettingsResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteIpAccessSettingsResponse_httpStatus :: Lens.Lens' DeleteIpAccessSettingsResponse Prelude.Int
deleteIpAccessSettingsResponse_httpStatus = Lens.lens (\DeleteIpAccessSettingsResponse' {httpStatus} -> httpStatus) (\s@DeleteIpAccessSettingsResponse' {} a -> s {httpStatus = a} :: DeleteIpAccessSettingsResponse)

instance
  Prelude.NFData
    DeleteIpAccessSettingsResponse
  where
  rnf DeleteIpAccessSettingsResponse' {..} =
    Prelude.rnf httpStatus
