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
-- Module      : Amazonka.WorkSpacesWeb.DeleteBrowserSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes browser settings.
module Amazonka.WorkSpacesWeb.DeleteBrowserSettings
  ( -- * Creating a Request
    DeleteBrowserSettings (..),
    newDeleteBrowserSettings,

    -- * Request Lenses
    deleteBrowserSettings_browserSettingsArn,

    -- * Destructuring the Response
    DeleteBrowserSettingsResponse (..),
    newDeleteBrowserSettingsResponse,

    -- * Response Lenses
    deleteBrowserSettingsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkSpacesWeb.Types

-- | /See:/ 'newDeleteBrowserSettings' smart constructor.
data DeleteBrowserSettings = DeleteBrowserSettings'
  { -- | The ARN of the browser settings.
    browserSettingsArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteBrowserSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'browserSettingsArn', 'deleteBrowserSettings_browserSettingsArn' - The ARN of the browser settings.
newDeleteBrowserSettings ::
  -- | 'browserSettingsArn'
  Prelude.Text ->
  DeleteBrowserSettings
newDeleteBrowserSettings pBrowserSettingsArn_ =
  DeleteBrowserSettings'
    { browserSettingsArn =
        pBrowserSettingsArn_
    }

-- | The ARN of the browser settings.
deleteBrowserSettings_browserSettingsArn :: Lens.Lens' DeleteBrowserSettings Prelude.Text
deleteBrowserSettings_browserSettingsArn = Lens.lens (\DeleteBrowserSettings' {browserSettingsArn} -> browserSettingsArn) (\s@DeleteBrowserSettings' {} a -> s {browserSettingsArn = a} :: DeleteBrowserSettings)

instance Core.AWSRequest DeleteBrowserSettings where
  type
    AWSResponse DeleteBrowserSettings =
      DeleteBrowserSettingsResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteBrowserSettingsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteBrowserSettings where
  hashWithSalt _salt DeleteBrowserSettings' {..} =
    _salt `Prelude.hashWithSalt` browserSettingsArn

instance Prelude.NFData DeleteBrowserSettings where
  rnf DeleteBrowserSettings' {..} =
    Prelude.rnf browserSettingsArn

instance Data.ToHeaders DeleteBrowserSettings where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteBrowserSettings where
  toPath DeleteBrowserSettings' {..} =
    Prelude.mconcat
      ["/browserSettings/", Data.toBS browserSettingsArn]

instance Data.ToQuery DeleteBrowserSettings where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteBrowserSettingsResponse' smart constructor.
data DeleteBrowserSettingsResponse = DeleteBrowserSettingsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteBrowserSettingsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteBrowserSettingsResponse_httpStatus' - The response's http status code.
newDeleteBrowserSettingsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteBrowserSettingsResponse
newDeleteBrowserSettingsResponse pHttpStatus_ =
  DeleteBrowserSettingsResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteBrowserSettingsResponse_httpStatus :: Lens.Lens' DeleteBrowserSettingsResponse Prelude.Int
deleteBrowserSettingsResponse_httpStatus = Lens.lens (\DeleteBrowserSettingsResponse' {httpStatus} -> httpStatus) (\s@DeleteBrowserSettingsResponse' {} a -> s {httpStatus = a} :: DeleteBrowserSettingsResponse)

instance Prelude.NFData DeleteBrowserSettingsResponse where
  rnf DeleteBrowserSettingsResponse' {..} =
    Prelude.rnf httpStatus
