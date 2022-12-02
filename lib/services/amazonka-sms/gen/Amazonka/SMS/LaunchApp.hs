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
-- Module      : Amazonka.SMS.LaunchApp
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Launches the specified application as a stack in CloudFormation.
module Amazonka.SMS.LaunchApp
  ( -- * Creating a Request
    LaunchApp (..),
    newLaunchApp,

    -- * Request Lenses
    launchApp_appId,

    -- * Destructuring the Response
    LaunchAppResponse (..),
    newLaunchAppResponse,

    -- * Response Lenses
    launchAppResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SMS.Types

-- | /See:/ 'newLaunchApp' smart constructor.
data LaunchApp = LaunchApp'
  { -- | The ID of the application.
    appId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LaunchApp' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appId', 'launchApp_appId' - The ID of the application.
newLaunchApp ::
  LaunchApp
newLaunchApp = LaunchApp' {appId = Prelude.Nothing}

-- | The ID of the application.
launchApp_appId :: Lens.Lens' LaunchApp (Prelude.Maybe Prelude.Text)
launchApp_appId = Lens.lens (\LaunchApp' {appId} -> appId) (\s@LaunchApp' {} a -> s {appId = a} :: LaunchApp)

instance Core.AWSRequest LaunchApp where
  type AWSResponse LaunchApp = LaunchAppResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          LaunchAppResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable LaunchApp where
  hashWithSalt _salt LaunchApp' {..} =
    _salt `Prelude.hashWithSalt` appId

instance Prelude.NFData LaunchApp where
  rnf LaunchApp' {..} = Prelude.rnf appId

instance Data.ToHeaders LaunchApp where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSServerMigrationService_V2016_10_24.LaunchApp" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON LaunchApp where
  toJSON LaunchApp' {..} =
    Data.object
      ( Prelude.catMaybes
          [("appId" Data..=) Prelude.<$> appId]
      )

instance Data.ToPath LaunchApp where
  toPath = Prelude.const "/"

instance Data.ToQuery LaunchApp where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newLaunchAppResponse' smart constructor.
data LaunchAppResponse = LaunchAppResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LaunchAppResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'launchAppResponse_httpStatus' - The response's http status code.
newLaunchAppResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  LaunchAppResponse
newLaunchAppResponse pHttpStatus_ =
  LaunchAppResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
launchAppResponse_httpStatus :: Lens.Lens' LaunchAppResponse Prelude.Int
launchAppResponse_httpStatus = Lens.lens (\LaunchAppResponse' {httpStatus} -> httpStatus) (\s@LaunchAppResponse' {} a -> s {httpStatus = a} :: LaunchAppResponse)

instance Prelude.NFData LaunchAppResponse where
  rnf LaunchAppResponse' {..} = Prelude.rnf httpStatus
