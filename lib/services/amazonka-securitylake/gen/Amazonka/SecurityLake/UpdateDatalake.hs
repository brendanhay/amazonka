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
-- Module      : Amazonka.SecurityLake.UpdateDatalake
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Specifies where to store your security data and for how long. You can
-- add a rollup Region to consolidate data from multiple Amazon Web
-- Services Regions.
module Amazonka.SecurityLake.UpdateDatalake
  ( -- * Creating a Request
    UpdateDatalake (..),
    newUpdateDatalake,

    -- * Request Lenses
    updateDatalake_configurations,

    -- * Destructuring the Response
    UpdateDatalakeResponse (..),
    newUpdateDatalakeResponse,

    -- * Response Lenses
    updateDatalakeResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SecurityLake.Types

-- | /See:/ 'newUpdateDatalake' smart constructor.
data UpdateDatalake = UpdateDatalake'
  { -- | Specify the Region or Regions that will contribute data to the rollup
    -- region.
    configurations :: Prelude.HashMap Region LakeConfigurationRequest
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateDatalake' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configurations', 'updateDatalake_configurations' - Specify the Region or Regions that will contribute data to the rollup
-- region.
newUpdateDatalake ::
  UpdateDatalake
newUpdateDatalake =
  UpdateDatalake' {configurations = Prelude.mempty}

-- | Specify the Region or Regions that will contribute data to the rollup
-- region.
updateDatalake_configurations :: Lens.Lens' UpdateDatalake (Prelude.HashMap Region LakeConfigurationRequest)
updateDatalake_configurations = Lens.lens (\UpdateDatalake' {configurations} -> configurations) (\s@UpdateDatalake' {} a -> s {configurations = a} :: UpdateDatalake) Prelude.. Lens.coerced

instance Core.AWSRequest UpdateDatalake where
  type
    AWSResponse UpdateDatalake =
      UpdateDatalakeResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateDatalakeResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateDatalake where
  hashWithSalt _salt UpdateDatalake' {..} =
    _salt `Prelude.hashWithSalt` configurations

instance Prelude.NFData UpdateDatalake where
  rnf UpdateDatalake' {..} = Prelude.rnf configurations

instance Data.ToHeaders UpdateDatalake where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateDatalake where
  toJSON UpdateDatalake' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("configurations" Data..= configurations)
          ]
      )

instance Data.ToPath UpdateDatalake where
  toPath = Prelude.const "/v1/datalake"

instance Data.ToQuery UpdateDatalake where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateDatalakeResponse' smart constructor.
data UpdateDatalakeResponse = UpdateDatalakeResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateDatalakeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateDatalakeResponse_httpStatus' - The response's http status code.
newUpdateDatalakeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateDatalakeResponse
newUpdateDatalakeResponse pHttpStatus_ =
  UpdateDatalakeResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
updateDatalakeResponse_httpStatus :: Lens.Lens' UpdateDatalakeResponse Prelude.Int
updateDatalakeResponse_httpStatus = Lens.lens (\UpdateDatalakeResponse' {httpStatus} -> httpStatus) (\s@UpdateDatalakeResponse' {} a -> s {httpStatus = a} :: UpdateDatalakeResponse)

instance Prelude.NFData UpdateDatalakeResponse where
  rnf UpdateDatalakeResponse' {..} =
    Prelude.rnf httpStatus
