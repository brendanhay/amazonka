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
-- Module      : Amazonka.M2.StartApplication
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts an application that is currently stopped.
module Amazonka.M2.StartApplication
  ( -- * Creating a Request
    StartApplication (..),
    newStartApplication,

    -- * Request Lenses
    startApplication_applicationId,

    -- * Destructuring the Response
    StartApplicationResponse (..),
    newStartApplicationResponse,

    -- * Response Lenses
    startApplicationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.M2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartApplication' smart constructor.
data StartApplication = StartApplication'
  { -- | The unique identifier of the application you want to start.
    applicationId :: Prelude.Text
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
-- 'applicationId', 'startApplication_applicationId' - The unique identifier of the application you want to start.
newStartApplication ::
  -- | 'applicationId'
  Prelude.Text ->
  StartApplication
newStartApplication pApplicationId_ =
  StartApplication' {applicationId = pApplicationId_}

-- | The unique identifier of the application you want to start.
startApplication_applicationId :: Lens.Lens' StartApplication Prelude.Text
startApplication_applicationId = Lens.lens (\StartApplication' {applicationId} -> applicationId) (\s@StartApplication' {} a -> s {applicationId = a} :: StartApplication)

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
    _salt `Prelude.hashWithSalt` applicationId

instance Prelude.NFData StartApplication where
  rnf StartApplication' {..} = Prelude.rnf applicationId

instance Data.ToHeaders StartApplication where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartApplication where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath StartApplication where
  toPath StartApplication' {..} =
    Prelude.mconcat
      ["/applications/", Data.toBS applicationId, "/start"]

instance Data.ToQuery StartApplication where
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
