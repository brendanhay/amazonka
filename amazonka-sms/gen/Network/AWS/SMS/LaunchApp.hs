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
-- Module      : Network.AWS.SMS.LaunchApp
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Launches the specified application as a stack in AWS CloudFormation.
module Network.AWS.SMS.LaunchApp
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SMS.Types

-- | /See:/ 'newLaunchApp' smart constructor.
data LaunchApp = LaunchApp'
  { -- | The ID of the application.
    appId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest LaunchApp where
  type Rs LaunchApp = LaunchAppResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          LaunchAppResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable LaunchApp

instance Prelude.NFData LaunchApp

instance Prelude.ToHeaders LaunchApp where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSServerMigrationService_V2016_10_24.LaunchApp" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON LaunchApp where
  toJSON LaunchApp' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [("appId" Prelude..=) Prelude.<$> appId]
      )

instance Prelude.ToPath LaunchApp where
  toPath = Prelude.const "/"

instance Prelude.ToQuery LaunchApp where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newLaunchAppResponse' smart constructor.
data LaunchAppResponse = LaunchAppResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.NFData LaunchAppResponse
