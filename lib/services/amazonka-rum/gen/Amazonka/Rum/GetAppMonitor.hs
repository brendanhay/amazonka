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
-- Module      : Amazonka.Rum.GetAppMonitor
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the complete configuration information for one app monitor.
module Amazonka.Rum.GetAppMonitor
  ( -- * Creating a Request
    GetAppMonitor (..),
    newGetAppMonitor,

    -- * Request Lenses
    getAppMonitor_name,

    -- * Destructuring the Response
    GetAppMonitorResponse (..),
    newGetAppMonitorResponse,

    -- * Response Lenses
    getAppMonitorResponse_appMonitor,
    getAppMonitorResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Rum.Types

-- | /See:/ 'newGetAppMonitor' smart constructor.
data GetAppMonitor = GetAppMonitor'
  { -- | The app monitor to retrieve information for.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetAppMonitor' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'getAppMonitor_name' - The app monitor to retrieve information for.
newGetAppMonitor ::
  -- | 'name'
  Prelude.Text ->
  GetAppMonitor
newGetAppMonitor pName_ =
  GetAppMonitor' {name = pName_}

-- | The app monitor to retrieve information for.
getAppMonitor_name :: Lens.Lens' GetAppMonitor Prelude.Text
getAppMonitor_name = Lens.lens (\GetAppMonitor' {name} -> name) (\s@GetAppMonitor' {} a -> s {name = a} :: GetAppMonitor)

instance Core.AWSRequest GetAppMonitor where
  type
    AWSResponse GetAppMonitor =
      GetAppMonitorResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetAppMonitorResponse'
            Prelude.<$> (x Data..?> "AppMonitor")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetAppMonitor where
  hashWithSalt _salt GetAppMonitor' {..} =
    _salt `Prelude.hashWithSalt` name

instance Prelude.NFData GetAppMonitor where
  rnf GetAppMonitor' {..} = Prelude.rnf name

instance Data.ToHeaders GetAppMonitor where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetAppMonitor where
  toPath GetAppMonitor' {..} =
    Prelude.mconcat ["/appmonitor/", Data.toBS name]

instance Data.ToQuery GetAppMonitor where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetAppMonitorResponse' smart constructor.
data GetAppMonitorResponse = GetAppMonitorResponse'
  { -- | A structure containing all the configuration information for the app
    -- monitor.
    appMonitor :: Prelude.Maybe AppMonitor,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetAppMonitorResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appMonitor', 'getAppMonitorResponse_appMonitor' - A structure containing all the configuration information for the app
-- monitor.
--
-- 'httpStatus', 'getAppMonitorResponse_httpStatus' - The response's http status code.
newGetAppMonitorResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetAppMonitorResponse
newGetAppMonitorResponse pHttpStatus_ =
  GetAppMonitorResponse'
    { appMonitor =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A structure containing all the configuration information for the app
-- monitor.
getAppMonitorResponse_appMonitor :: Lens.Lens' GetAppMonitorResponse (Prelude.Maybe AppMonitor)
getAppMonitorResponse_appMonitor = Lens.lens (\GetAppMonitorResponse' {appMonitor} -> appMonitor) (\s@GetAppMonitorResponse' {} a -> s {appMonitor = a} :: GetAppMonitorResponse)

-- | The response's http status code.
getAppMonitorResponse_httpStatus :: Lens.Lens' GetAppMonitorResponse Prelude.Int
getAppMonitorResponse_httpStatus = Lens.lens (\GetAppMonitorResponse' {httpStatus} -> httpStatus) (\s@GetAppMonitorResponse' {} a -> s {httpStatus = a} :: GetAppMonitorResponse)

instance Prelude.NFData GetAppMonitorResponse where
  rnf GetAppMonitorResponse' {..} =
    Prelude.rnf appMonitor
      `Prelude.seq` Prelude.rnf httpStatus
