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
-- Module      : Amazonka.Discovery.UpdateApplication
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates metadata about an application.
module Amazonka.Discovery.UpdateApplication
  ( -- * Creating a Request
    UpdateApplication (..),
    newUpdateApplication,

    -- * Request Lenses
    updateApplication_description,
    updateApplication_name,
    updateApplication_configurationId,

    -- * Destructuring the Response
    UpdateApplicationResponse (..),
    newUpdateApplicationResponse,

    -- * Response Lenses
    updateApplicationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Discovery.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateApplication' smart constructor.
data UpdateApplication = UpdateApplication'
  { -- | New description of the application to be updated.
    description :: Prelude.Maybe Prelude.Text,
    -- | New name of the application to be updated.
    name :: Prelude.Maybe Prelude.Text,
    -- | Configuration ID of the application to be updated.
    configurationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateApplication' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'updateApplication_description' - New description of the application to be updated.
--
-- 'name', 'updateApplication_name' - New name of the application to be updated.
--
-- 'configurationId', 'updateApplication_configurationId' - Configuration ID of the application to be updated.
newUpdateApplication ::
  -- | 'configurationId'
  Prelude.Text ->
  UpdateApplication
newUpdateApplication pConfigurationId_ =
  UpdateApplication'
    { description = Prelude.Nothing,
      name = Prelude.Nothing,
      configurationId = pConfigurationId_
    }

-- | New description of the application to be updated.
updateApplication_description :: Lens.Lens' UpdateApplication (Prelude.Maybe Prelude.Text)
updateApplication_description = Lens.lens (\UpdateApplication' {description} -> description) (\s@UpdateApplication' {} a -> s {description = a} :: UpdateApplication)

-- | New name of the application to be updated.
updateApplication_name :: Lens.Lens' UpdateApplication (Prelude.Maybe Prelude.Text)
updateApplication_name = Lens.lens (\UpdateApplication' {name} -> name) (\s@UpdateApplication' {} a -> s {name = a} :: UpdateApplication)

-- | Configuration ID of the application to be updated.
updateApplication_configurationId :: Lens.Lens' UpdateApplication Prelude.Text
updateApplication_configurationId = Lens.lens (\UpdateApplication' {configurationId} -> configurationId) (\s@UpdateApplication' {} a -> s {configurationId = a} :: UpdateApplication)

instance Core.AWSRequest UpdateApplication where
  type
    AWSResponse UpdateApplication =
      UpdateApplicationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateApplicationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateApplication where
  hashWithSalt _salt UpdateApplication' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` configurationId

instance Prelude.NFData UpdateApplication where
  rnf UpdateApplication' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf configurationId

instance Data.ToHeaders UpdateApplication where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSPoseidonService_V2015_11_01.UpdateApplication" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateApplication where
  toJSON UpdateApplication' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("description" Data..=) Prelude.<$> description,
            ("name" Data..=) Prelude.<$> name,
            Prelude.Just
              ("configurationId" Data..= configurationId)
          ]
      )

instance Data.ToPath UpdateApplication where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateApplication where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateApplicationResponse' smart constructor.
data UpdateApplicationResponse = UpdateApplicationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateApplicationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateApplicationResponse_httpStatus' - The response's http status code.
newUpdateApplicationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateApplicationResponse
newUpdateApplicationResponse pHttpStatus_ =
  UpdateApplicationResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateApplicationResponse_httpStatus :: Lens.Lens' UpdateApplicationResponse Prelude.Int
updateApplicationResponse_httpStatus = Lens.lens (\UpdateApplicationResponse' {httpStatus} -> httpStatus) (\s@UpdateApplicationResponse' {} a -> s {httpStatus = a} :: UpdateApplicationResponse)

instance Prelude.NFData UpdateApplicationResponse where
  rnf UpdateApplicationResponse' {..} =
    Prelude.rnf httpStatus
