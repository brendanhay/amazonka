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
-- Module      : Amazonka.KinesisAnalyticsV2.StartApplication
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts the specified Kinesis Data Analytics application. After creating
-- an application, you must exclusively call this operation to start your
-- application.
module Amazonka.KinesisAnalyticsV2.StartApplication
  ( -- * Creating a Request
    StartApplication (..),
    newStartApplication,

    -- * Request Lenses
    startApplication_runConfiguration,
    startApplication_applicationName,

    -- * Destructuring the Response
    StartApplicationResponse (..),
    newStartApplicationResponse,

    -- * Response Lenses
    startApplicationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.KinesisAnalyticsV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartApplication' smart constructor.
data StartApplication = StartApplication'
  { -- | Identifies the run configuration (start parameters) of a Kinesis Data
    -- Analytics application.
    runConfiguration :: Prelude.Maybe RunConfiguration,
    -- | The name of the application.
    applicationName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartApplication' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'runConfiguration', 'startApplication_runConfiguration' - Identifies the run configuration (start parameters) of a Kinesis Data
-- Analytics application.
--
-- 'applicationName', 'startApplication_applicationName' - The name of the application.
newStartApplication ::
  -- | 'applicationName'
  Prelude.Text ->
  StartApplication
newStartApplication pApplicationName_ =
  StartApplication'
    { runConfiguration =
        Prelude.Nothing,
      applicationName = pApplicationName_
    }

-- | Identifies the run configuration (start parameters) of a Kinesis Data
-- Analytics application.
startApplication_runConfiguration :: Lens.Lens' StartApplication (Prelude.Maybe RunConfiguration)
startApplication_runConfiguration = Lens.lens (\StartApplication' {runConfiguration} -> runConfiguration) (\s@StartApplication' {} a -> s {runConfiguration = a} :: StartApplication)

-- | The name of the application.
startApplication_applicationName :: Lens.Lens' StartApplication Prelude.Text
startApplication_applicationName = Lens.lens (\StartApplication' {applicationName} -> applicationName) (\s@StartApplication' {} a -> s {applicationName = a} :: StartApplication)

instance Core.AWSRequest StartApplication where
  type
    AWSResponse StartApplication =
      StartApplicationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          StartApplicationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartApplication where
  hashWithSalt _salt StartApplication' {..} =
    _salt `Prelude.hashWithSalt` runConfiguration
      `Prelude.hashWithSalt` applicationName

instance Prelude.NFData StartApplication where
  rnf StartApplication' {..} =
    Prelude.rnf runConfiguration
      `Prelude.seq` Prelude.rnf applicationName

instance Core.ToHeaders StartApplication where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "KinesisAnalytics_20180523.StartApplication" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON StartApplication where
  toJSON StartApplication' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("RunConfiguration" Core..=)
              Prelude.<$> runConfiguration,
            Prelude.Just
              ("ApplicationName" Core..= applicationName)
          ]
      )

instance Core.ToPath StartApplication where
  toPath = Prelude.const "/"

instance Core.ToQuery StartApplication where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartApplicationResponse' smart constructor.
data StartApplicationResponse = StartApplicationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartApplicationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'startApplicationResponse_httpStatus' - The response's http status code.
newStartApplicationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartApplicationResponse
newStartApplicationResponse pHttpStatus_ =
  StartApplicationResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
startApplicationResponse_httpStatus :: Lens.Lens' StartApplicationResponse Prelude.Int
startApplicationResponse_httpStatus = Lens.lens (\StartApplicationResponse' {httpStatus} -> httpStatus) (\s@StartApplicationResponse' {} a -> s {httpStatus = a} :: StartApplicationResponse)

instance Prelude.NFData StartApplicationResponse where
  rnf StartApplicationResponse' {..} =
    Prelude.rnf httpStatus
