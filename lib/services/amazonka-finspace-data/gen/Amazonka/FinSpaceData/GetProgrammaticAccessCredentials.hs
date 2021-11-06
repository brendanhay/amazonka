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
-- Module      : Amazonka.FinSpaceData.GetProgrammaticAccessCredentials
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Request programmatic credentials to use with Habanero SDK.
module Amazonka.FinSpaceData.GetProgrammaticAccessCredentials
  ( -- * Creating a Request
    GetProgrammaticAccessCredentials (..),
    newGetProgrammaticAccessCredentials,

    -- * Request Lenses
    getProgrammaticAccessCredentials_durationInMinutes,
    getProgrammaticAccessCredentials_environmentId,

    -- * Destructuring the Response
    GetProgrammaticAccessCredentialsResponse (..),
    newGetProgrammaticAccessCredentialsResponse,

    -- * Response Lenses
    getProgrammaticAccessCredentialsResponse_credentials,
    getProgrammaticAccessCredentialsResponse_durationInMinutes,
    getProgrammaticAccessCredentialsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.FinSpaceData.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetProgrammaticAccessCredentials' smart constructor.
data GetProgrammaticAccessCredentials = GetProgrammaticAccessCredentials'
  { -- | The time duration in which the credentials remain valid.
    durationInMinutes :: Prelude.Maybe Prelude.Natural,
    -- | The habanero environment identifier.
    environmentId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetProgrammaticAccessCredentials' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'durationInMinutes', 'getProgrammaticAccessCredentials_durationInMinutes' - The time duration in which the credentials remain valid.
--
-- 'environmentId', 'getProgrammaticAccessCredentials_environmentId' - The habanero environment identifier.
newGetProgrammaticAccessCredentials ::
  -- | 'environmentId'
  Prelude.Text ->
  GetProgrammaticAccessCredentials
newGetProgrammaticAccessCredentials pEnvironmentId_ =
  GetProgrammaticAccessCredentials'
    { durationInMinutes =
        Prelude.Nothing,
      environmentId = pEnvironmentId_
    }

-- | The time duration in which the credentials remain valid.
getProgrammaticAccessCredentials_durationInMinutes :: Lens.Lens' GetProgrammaticAccessCredentials (Prelude.Maybe Prelude.Natural)
getProgrammaticAccessCredentials_durationInMinutes = Lens.lens (\GetProgrammaticAccessCredentials' {durationInMinutes} -> durationInMinutes) (\s@GetProgrammaticAccessCredentials' {} a -> s {durationInMinutes = a} :: GetProgrammaticAccessCredentials)

-- | The habanero environment identifier.
getProgrammaticAccessCredentials_environmentId :: Lens.Lens' GetProgrammaticAccessCredentials Prelude.Text
getProgrammaticAccessCredentials_environmentId = Lens.lens (\GetProgrammaticAccessCredentials' {environmentId} -> environmentId) (\s@GetProgrammaticAccessCredentials' {} a -> s {environmentId = a} :: GetProgrammaticAccessCredentials)

instance
  Core.AWSRequest
    GetProgrammaticAccessCredentials
  where
  type
    AWSResponse GetProgrammaticAccessCredentials =
      GetProgrammaticAccessCredentialsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetProgrammaticAccessCredentialsResponse'
            Prelude.<$> (x Core..?> "credentials")
            Prelude.<*> (x Core..?> "durationInMinutes")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetProgrammaticAccessCredentials

instance
  Prelude.NFData
    GetProgrammaticAccessCredentials

instance
  Core.ToHeaders
    GetProgrammaticAccessCredentials
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetProgrammaticAccessCredentials where
  toPath = Prelude.const "/credentials/programmatic"

instance
  Core.ToQuery
    GetProgrammaticAccessCredentials
  where
  toQuery GetProgrammaticAccessCredentials' {..} =
    Prelude.mconcat
      [ "durationInMinutes" Core.=: durationInMinutes,
        "environmentId" Core.=: environmentId
      ]

-- | /See:/ 'newGetProgrammaticAccessCredentialsResponse' smart constructor.
data GetProgrammaticAccessCredentialsResponse = GetProgrammaticAccessCredentialsResponse'
  { -- | Returns the programmatic credentials.
    credentials :: Prelude.Maybe Credentials,
    -- | Returns the duration in which the credentials will remain valid.
    durationInMinutes :: Prelude.Maybe Prelude.Natural,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetProgrammaticAccessCredentialsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'credentials', 'getProgrammaticAccessCredentialsResponse_credentials' - Returns the programmatic credentials.
--
-- 'durationInMinutes', 'getProgrammaticAccessCredentialsResponse_durationInMinutes' - Returns the duration in which the credentials will remain valid.
--
-- 'httpStatus', 'getProgrammaticAccessCredentialsResponse_httpStatus' - The response's http status code.
newGetProgrammaticAccessCredentialsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetProgrammaticAccessCredentialsResponse
newGetProgrammaticAccessCredentialsResponse
  pHttpStatus_ =
    GetProgrammaticAccessCredentialsResponse'
      { credentials =
          Prelude.Nothing,
        durationInMinutes =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Returns the programmatic credentials.
getProgrammaticAccessCredentialsResponse_credentials :: Lens.Lens' GetProgrammaticAccessCredentialsResponse (Prelude.Maybe Credentials)
getProgrammaticAccessCredentialsResponse_credentials = Lens.lens (\GetProgrammaticAccessCredentialsResponse' {credentials} -> credentials) (\s@GetProgrammaticAccessCredentialsResponse' {} a -> s {credentials = a} :: GetProgrammaticAccessCredentialsResponse)

-- | Returns the duration in which the credentials will remain valid.
getProgrammaticAccessCredentialsResponse_durationInMinutes :: Lens.Lens' GetProgrammaticAccessCredentialsResponse (Prelude.Maybe Prelude.Natural)
getProgrammaticAccessCredentialsResponse_durationInMinutes = Lens.lens (\GetProgrammaticAccessCredentialsResponse' {durationInMinutes} -> durationInMinutes) (\s@GetProgrammaticAccessCredentialsResponse' {} a -> s {durationInMinutes = a} :: GetProgrammaticAccessCredentialsResponse)

-- | The response's http status code.
getProgrammaticAccessCredentialsResponse_httpStatus :: Lens.Lens' GetProgrammaticAccessCredentialsResponse Prelude.Int
getProgrammaticAccessCredentialsResponse_httpStatus = Lens.lens (\GetProgrammaticAccessCredentialsResponse' {httpStatus} -> httpStatus) (\s@GetProgrammaticAccessCredentialsResponse' {} a -> s {httpStatus = a} :: GetProgrammaticAccessCredentialsResponse)

instance
  Prelude.NFData
    GetProgrammaticAccessCredentialsResponse
