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
-- Module      : Amazonka.ECRPublic.PutRegistryCatalogData
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create or updates the catalog data for a public registry.
module Amazonka.ECRPublic.PutRegistryCatalogData
  ( -- * Creating a Request
    PutRegistryCatalogData (..),
    newPutRegistryCatalogData,

    -- * Request Lenses
    putRegistryCatalogData_displayName,

    -- * Destructuring the Response
    PutRegistryCatalogDataResponse (..),
    newPutRegistryCatalogDataResponse,

    -- * Response Lenses
    putRegistryCatalogDataResponse_httpStatus,
    putRegistryCatalogDataResponse_registryCatalogData,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ECRPublic.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutRegistryCatalogData' smart constructor.
data PutRegistryCatalogData = PutRegistryCatalogData'
  { -- | The display name for a public registry. The display name is shown as the
    -- repository author in the Amazon ECR Public Gallery.
    --
    -- The registry display name is only publicly visible in the Amazon ECR
    -- Public Gallery for verified accounts.
    displayName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutRegistryCatalogData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'displayName', 'putRegistryCatalogData_displayName' - The display name for a public registry. The display name is shown as the
-- repository author in the Amazon ECR Public Gallery.
--
-- The registry display name is only publicly visible in the Amazon ECR
-- Public Gallery for verified accounts.
newPutRegistryCatalogData ::
  PutRegistryCatalogData
newPutRegistryCatalogData =
  PutRegistryCatalogData'
    { displayName =
        Prelude.Nothing
    }

-- | The display name for a public registry. The display name is shown as the
-- repository author in the Amazon ECR Public Gallery.
--
-- The registry display name is only publicly visible in the Amazon ECR
-- Public Gallery for verified accounts.
putRegistryCatalogData_displayName :: Lens.Lens' PutRegistryCatalogData (Prelude.Maybe Prelude.Text)
putRegistryCatalogData_displayName = Lens.lens (\PutRegistryCatalogData' {displayName} -> displayName) (\s@PutRegistryCatalogData' {} a -> s {displayName = a} :: PutRegistryCatalogData)

instance Core.AWSRequest PutRegistryCatalogData where
  type
    AWSResponse PutRegistryCatalogData =
      PutRegistryCatalogDataResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          PutRegistryCatalogDataResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "registryCatalogData")
      )

instance Prelude.Hashable PutRegistryCatalogData where
  hashWithSalt _salt PutRegistryCatalogData' {..} =
    _salt `Prelude.hashWithSalt` displayName

instance Prelude.NFData PutRegistryCatalogData where
  rnf PutRegistryCatalogData' {..} =
    Prelude.rnf displayName

instance Data.ToHeaders PutRegistryCatalogData where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SpencerFrontendService.PutRegistryCatalogData" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PutRegistryCatalogData where
  toJSON PutRegistryCatalogData' {..} =
    Data.object
      ( Prelude.catMaybes
          [("displayName" Data..=) Prelude.<$> displayName]
      )

instance Data.ToPath PutRegistryCatalogData where
  toPath = Prelude.const "/"

instance Data.ToQuery PutRegistryCatalogData where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutRegistryCatalogDataResponse' smart constructor.
data PutRegistryCatalogDataResponse = PutRegistryCatalogDataResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The catalog data for the public registry.
    registryCatalogData :: RegistryCatalogData
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutRegistryCatalogDataResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'putRegistryCatalogDataResponse_httpStatus' - The response's http status code.
--
-- 'registryCatalogData', 'putRegistryCatalogDataResponse_registryCatalogData' - The catalog data for the public registry.
newPutRegistryCatalogDataResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'registryCatalogData'
  RegistryCatalogData ->
  PutRegistryCatalogDataResponse
newPutRegistryCatalogDataResponse
  pHttpStatus_
  pRegistryCatalogData_ =
    PutRegistryCatalogDataResponse'
      { httpStatus =
          pHttpStatus_,
        registryCatalogData = pRegistryCatalogData_
      }

-- | The response's http status code.
putRegistryCatalogDataResponse_httpStatus :: Lens.Lens' PutRegistryCatalogDataResponse Prelude.Int
putRegistryCatalogDataResponse_httpStatus = Lens.lens (\PutRegistryCatalogDataResponse' {httpStatus} -> httpStatus) (\s@PutRegistryCatalogDataResponse' {} a -> s {httpStatus = a} :: PutRegistryCatalogDataResponse)

-- | The catalog data for the public registry.
putRegistryCatalogDataResponse_registryCatalogData :: Lens.Lens' PutRegistryCatalogDataResponse RegistryCatalogData
putRegistryCatalogDataResponse_registryCatalogData = Lens.lens (\PutRegistryCatalogDataResponse' {registryCatalogData} -> registryCatalogData) (\s@PutRegistryCatalogDataResponse' {} a -> s {registryCatalogData = a} :: PutRegistryCatalogDataResponse)

instance
  Prelude.NFData
    PutRegistryCatalogDataResponse
  where
  rnf PutRegistryCatalogDataResponse' {..} =
    Prelude.rnf httpStatus `Prelude.seq`
      Prelude.rnf registryCatalogData
