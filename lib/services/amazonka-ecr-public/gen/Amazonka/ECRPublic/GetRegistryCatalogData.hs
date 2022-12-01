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
-- Module      : Amazonka.ECRPublic.GetRegistryCatalogData
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves catalog metadata for a public registry.
module Amazonka.ECRPublic.GetRegistryCatalogData
  ( -- * Creating a Request
    GetRegistryCatalogData (..),
    newGetRegistryCatalogData,

    -- * Destructuring the Response
    GetRegistryCatalogDataResponse (..),
    newGetRegistryCatalogDataResponse,

    -- * Response Lenses
    getRegistryCatalogDataResponse_httpStatus,
    getRegistryCatalogDataResponse_registryCatalogData,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.ECRPublic.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetRegistryCatalogData' smart constructor.
data GetRegistryCatalogData = GetRegistryCatalogData'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetRegistryCatalogData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newGetRegistryCatalogData ::
  GetRegistryCatalogData
newGetRegistryCatalogData = GetRegistryCatalogData'

instance Core.AWSRequest GetRegistryCatalogData where
  type
    AWSResponse GetRegistryCatalogData =
      GetRegistryCatalogDataResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetRegistryCatalogDataResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "registryCatalogData")
      )

instance Prelude.Hashable GetRegistryCatalogData where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance Prelude.NFData GetRegistryCatalogData where
  rnf _ = ()

instance Core.ToHeaders GetRegistryCatalogData where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SpencerFrontendService.GetRegistryCatalogData" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetRegistryCatalogData where
  toJSON = Prelude.const (Core.Object Prelude.mempty)

instance Core.ToPath GetRegistryCatalogData where
  toPath = Prelude.const "/"

instance Core.ToQuery GetRegistryCatalogData where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetRegistryCatalogDataResponse' smart constructor.
data GetRegistryCatalogDataResponse = GetRegistryCatalogDataResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The catalog metadata for the public registry.
    registryCatalogData :: RegistryCatalogData
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetRegistryCatalogDataResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getRegistryCatalogDataResponse_httpStatus' - The response's http status code.
--
-- 'registryCatalogData', 'getRegistryCatalogDataResponse_registryCatalogData' - The catalog metadata for the public registry.
newGetRegistryCatalogDataResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'registryCatalogData'
  RegistryCatalogData ->
  GetRegistryCatalogDataResponse
newGetRegistryCatalogDataResponse
  pHttpStatus_
  pRegistryCatalogData_ =
    GetRegistryCatalogDataResponse'
      { httpStatus =
          pHttpStatus_,
        registryCatalogData = pRegistryCatalogData_
      }

-- | The response's http status code.
getRegistryCatalogDataResponse_httpStatus :: Lens.Lens' GetRegistryCatalogDataResponse Prelude.Int
getRegistryCatalogDataResponse_httpStatus = Lens.lens (\GetRegistryCatalogDataResponse' {httpStatus} -> httpStatus) (\s@GetRegistryCatalogDataResponse' {} a -> s {httpStatus = a} :: GetRegistryCatalogDataResponse)

-- | The catalog metadata for the public registry.
getRegistryCatalogDataResponse_registryCatalogData :: Lens.Lens' GetRegistryCatalogDataResponse RegistryCatalogData
getRegistryCatalogDataResponse_registryCatalogData = Lens.lens (\GetRegistryCatalogDataResponse' {registryCatalogData} -> registryCatalogData) (\s@GetRegistryCatalogDataResponse' {} a -> s {registryCatalogData = a} :: GetRegistryCatalogDataResponse)

instance
  Prelude.NFData
    GetRegistryCatalogDataResponse
  where
  rnf GetRegistryCatalogDataResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf registryCatalogData
