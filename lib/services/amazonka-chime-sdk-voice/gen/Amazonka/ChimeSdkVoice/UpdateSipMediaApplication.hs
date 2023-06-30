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
-- Module      : Amazonka.ChimeSdkVoice.UpdateSipMediaApplication
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- -- | Undocumented operation.
module Amazonka.ChimeSdkVoice.UpdateSipMediaApplication
  ( -- * Creating a Request
    UpdateSipMediaApplication (..),
    newUpdateSipMediaApplication,

    -- * Request Lenses
    updateSipMediaApplication_endpoints,
    updateSipMediaApplication_name,
    updateSipMediaApplication_sipMediaApplicationId,

    -- * Destructuring the Response
    UpdateSipMediaApplicationResponse (..),
    newUpdateSipMediaApplicationResponse,

    -- * Response Lenses
    updateSipMediaApplicationResponse_sipMediaApplication,
    updateSipMediaApplicationResponse_httpStatus,
  )
where

import Amazonka.ChimeSdkVoice.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateSipMediaApplication' smart constructor.
data UpdateSipMediaApplication = UpdateSipMediaApplication'
  { endpoints :: Prelude.Maybe (Prelude.NonEmpty SipMediaApplicationEndpoint),
    name :: Prelude.Maybe Prelude.Text,
    sipMediaApplicationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateSipMediaApplication' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endpoints', 'updateSipMediaApplication_endpoints' - Undocumented member.
--
-- 'name', 'updateSipMediaApplication_name' - Undocumented member.
--
-- 'sipMediaApplicationId', 'updateSipMediaApplication_sipMediaApplicationId' - Undocumented member.
newUpdateSipMediaApplication ::
  -- | 'sipMediaApplicationId'
  Prelude.Text ->
  UpdateSipMediaApplication
newUpdateSipMediaApplication pSipMediaApplicationId_ =
  UpdateSipMediaApplication'
    { endpoints =
        Prelude.Nothing,
      name = Prelude.Nothing,
      sipMediaApplicationId = pSipMediaApplicationId_
    }

-- | Undocumented member.
updateSipMediaApplication_endpoints :: Lens.Lens' UpdateSipMediaApplication (Prelude.Maybe (Prelude.NonEmpty SipMediaApplicationEndpoint))
updateSipMediaApplication_endpoints = Lens.lens (\UpdateSipMediaApplication' {endpoints} -> endpoints) (\s@UpdateSipMediaApplication' {} a -> s {endpoints = a} :: UpdateSipMediaApplication) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
updateSipMediaApplication_name :: Lens.Lens' UpdateSipMediaApplication (Prelude.Maybe Prelude.Text)
updateSipMediaApplication_name = Lens.lens (\UpdateSipMediaApplication' {name} -> name) (\s@UpdateSipMediaApplication' {} a -> s {name = a} :: UpdateSipMediaApplication)

-- | Undocumented member.
updateSipMediaApplication_sipMediaApplicationId :: Lens.Lens' UpdateSipMediaApplication Prelude.Text
updateSipMediaApplication_sipMediaApplicationId = Lens.lens (\UpdateSipMediaApplication' {sipMediaApplicationId} -> sipMediaApplicationId) (\s@UpdateSipMediaApplication' {} a -> s {sipMediaApplicationId = a} :: UpdateSipMediaApplication)

instance Core.AWSRequest UpdateSipMediaApplication where
  type
    AWSResponse UpdateSipMediaApplication =
      UpdateSipMediaApplicationResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateSipMediaApplicationResponse'
            Prelude.<$> (x Data..?> "SipMediaApplication")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateSipMediaApplication where
  hashWithSalt _salt UpdateSipMediaApplication' {..} =
    _salt
      `Prelude.hashWithSalt` endpoints
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` sipMediaApplicationId

instance Prelude.NFData UpdateSipMediaApplication where
  rnf UpdateSipMediaApplication' {..} =
    Prelude.rnf endpoints
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf sipMediaApplicationId

instance Data.ToHeaders UpdateSipMediaApplication where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON UpdateSipMediaApplication where
  toJSON UpdateSipMediaApplication' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Endpoints" Data..=) Prelude.<$> endpoints,
            ("Name" Data..=) Prelude.<$> name
          ]
      )

instance Data.ToPath UpdateSipMediaApplication where
  toPath UpdateSipMediaApplication' {..} =
    Prelude.mconcat
      [ "/sip-media-applications/",
        Data.toBS sipMediaApplicationId
      ]

instance Data.ToQuery UpdateSipMediaApplication where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateSipMediaApplicationResponse' smart constructor.
data UpdateSipMediaApplicationResponse = UpdateSipMediaApplicationResponse'
  { sipMediaApplication :: Prelude.Maybe SipMediaApplication,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateSipMediaApplicationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sipMediaApplication', 'updateSipMediaApplicationResponse_sipMediaApplication' - Undocumented member.
--
-- 'httpStatus', 'updateSipMediaApplicationResponse_httpStatus' - The response's http status code.
newUpdateSipMediaApplicationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateSipMediaApplicationResponse
newUpdateSipMediaApplicationResponse pHttpStatus_ =
  UpdateSipMediaApplicationResponse'
    { sipMediaApplication =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
updateSipMediaApplicationResponse_sipMediaApplication :: Lens.Lens' UpdateSipMediaApplicationResponse (Prelude.Maybe SipMediaApplication)
updateSipMediaApplicationResponse_sipMediaApplication = Lens.lens (\UpdateSipMediaApplicationResponse' {sipMediaApplication} -> sipMediaApplication) (\s@UpdateSipMediaApplicationResponse' {} a -> s {sipMediaApplication = a} :: UpdateSipMediaApplicationResponse)

-- | The response's http status code.
updateSipMediaApplicationResponse_httpStatus :: Lens.Lens' UpdateSipMediaApplicationResponse Prelude.Int
updateSipMediaApplicationResponse_httpStatus = Lens.lens (\UpdateSipMediaApplicationResponse' {httpStatus} -> httpStatus) (\s@UpdateSipMediaApplicationResponse' {} a -> s {httpStatus = a} :: UpdateSipMediaApplicationResponse)

instance
  Prelude.NFData
    UpdateSipMediaApplicationResponse
  where
  rnf UpdateSipMediaApplicationResponse' {..} =
    Prelude.rnf sipMediaApplication
      `Prelude.seq` Prelude.rnf httpStatus
