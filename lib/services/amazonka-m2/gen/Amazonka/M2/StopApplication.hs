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
-- Module      : Amazonka.M2.StopApplication
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a running application.
module Amazonka.M2.StopApplication
  ( -- * Creating a Request
    StopApplication (..),
    newStopApplication,

    -- * Request Lenses
    stopApplication_forceStop,
    stopApplication_applicationId,

    -- * Destructuring the Response
    StopApplicationResponse (..),
    newStopApplicationResponse,

    -- * Response Lenses
    stopApplicationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.M2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStopApplication' smart constructor.
data StopApplication = StopApplication'
  { -- | Stopping an application process can take a long time. Setting this
    -- parameter to true lets you force stop the application so you don\'t need
    -- to wait until the process finishes to apply another action on the
    -- application. The default value is false.
    forceStop :: Prelude.Maybe Prelude.Bool,
    -- | The unique identifier of the application you want to stop.
    applicationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopApplication' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'forceStop', 'stopApplication_forceStop' - Stopping an application process can take a long time. Setting this
-- parameter to true lets you force stop the application so you don\'t need
-- to wait until the process finishes to apply another action on the
-- application. The default value is false.
--
-- 'applicationId', 'stopApplication_applicationId' - The unique identifier of the application you want to stop.
newStopApplication ::
  -- | 'applicationId'
  Prelude.Text ->
  StopApplication
newStopApplication pApplicationId_ =
  StopApplication'
    { forceStop = Prelude.Nothing,
      applicationId = pApplicationId_
    }

-- | Stopping an application process can take a long time. Setting this
-- parameter to true lets you force stop the application so you don\'t need
-- to wait until the process finishes to apply another action on the
-- application. The default value is false.
stopApplication_forceStop :: Lens.Lens' StopApplication (Prelude.Maybe Prelude.Bool)
stopApplication_forceStop = Lens.lens (\StopApplication' {forceStop} -> forceStop) (\s@StopApplication' {} a -> s {forceStop = a} :: StopApplication)

-- | The unique identifier of the application you want to stop.
stopApplication_applicationId :: Lens.Lens' StopApplication Prelude.Text
stopApplication_applicationId = Lens.lens (\StopApplication' {applicationId} -> applicationId) (\s@StopApplication' {} a -> s {applicationId = a} :: StopApplication)

instance Core.AWSRequest StopApplication where
  type
    AWSResponse StopApplication =
      StopApplicationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          StopApplicationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StopApplication where
  hashWithSalt _salt StopApplication' {..} =
    _salt
      `Prelude.hashWithSalt` forceStop
      `Prelude.hashWithSalt` applicationId

instance Prelude.NFData StopApplication where
  rnf StopApplication' {..} =
    Prelude.rnf forceStop
      `Prelude.seq` Prelude.rnf applicationId

instance Data.ToHeaders StopApplication where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StopApplication where
  toJSON StopApplication' {..} =
    Data.object
      ( Prelude.catMaybes
          [("forceStop" Data..=) Prelude.<$> forceStop]
      )

instance Data.ToPath StopApplication where
  toPath StopApplication' {..} =
    Prelude.mconcat
      ["/applications/", Data.toBS applicationId, "/stop"]

instance Data.ToQuery StopApplication where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStopApplicationResponse' smart constructor.
data StopApplicationResponse = StopApplicationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopApplicationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'stopApplicationResponse_httpStatus' - The response's http status code.
newStopApplicationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StopApplicationResponse
newStopApplicationResponse pHttpStatus_ =
  StopApplicationResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
stopApplicationResponse_httpStatus :: Lens.Lens' StopApplicationResponse Prelude.Int
stopApplicationResponse_httpStatus = Lens.lens (\StopApplicationResponse' {httpStatus} -> httpStatus) (\s@StopApplicationResponse' {} a -> s {httpStatus = a} :: StopApplicationResponse)

instance Prelude.NFData StopApplicationResponse where
  rnf StopApplicationResponse' {..} =
    Prelude.rnf httpStatus
