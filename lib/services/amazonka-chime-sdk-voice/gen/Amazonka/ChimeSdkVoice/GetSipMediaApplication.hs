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
-- Module      : Amazonka.ChimeSdkVoice.GetSipMediaApplication
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- -- | Undocumented operation.
module Amazonka.ChimeSdkVoice.GetSipMediaApplication
  ( -- * Creating a Request
    GetSipMediaApplication (..),
    newGetSipMediaApplication,

    -- * Request Lenses
    getSipMediaApplication_sipMediaApplicationId,

    -- * Destructuring the Response
    GetSipMediaApplicationResponse (..),
    newGetSipMediaApplicationResponse,

    -- * Response Lenses
    getSipMediaApplicationResponse_sipMediaApplication,
    getSipMediaApplicationResponse_httpStatus,
  )
where

import Amazonka.ChimeSdkVoice.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetSipMediaApplication' smart constructor.
data GetSipMediaApplication = GetSipMediaApplication'
  { sipMediaApplicationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSipMediaApplication' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sipMediaApplicationId', 'getSipMediaApplication_sipMediaApplicationId' - Undocumented member.
newGetSipMediaApplication ::
  -- | 'sipMediaApplicationId'
  Prelude.Text ->
  GetSipMediaApplication
newGetSipMediaApplication pSipMediaApplicationId_ =
  GetSipMediaApplication'
    { sipMediaApplicationId =
        pSipMediaApplicationId_
    }

-- | Undocumented member.
getSipMediaApplication_sipMediaApplicationId :: Lens.Lens' GetSipMediaApplication Prelude.Text
getSipMediaApplication_sipMediaApplicationId = Lens.lens (\GetSipMediaApplication' {sipMediaApplicationId} -> sipMediaApplicationId) (\s@GetSipMediaApplication' {} a -> s {sipMediaApplicationId = a} :: GetSipMediaApplication)

instance Core.AWSRequest GetSipMediaApplication where
  type
    AWSResponse GetSipMediaApplication =
      GetSipMediaApplicationResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSipMediaApplicationResponse'
            Prelude.<$> (x Data..?> "SipMediaApplication")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetSipMediaApplication where
  hashWithSalt _salt GetSipMediaApplication' {..} =
    _salt `Prelude.hashWithSalt` sipMediaApplicationId

instance Prelude.NFData GetSipMediaApplication where
  rnf GetSipMediaApplication' {..} =
    Prelude.rnf sipMediaApplicationId

instance Data.ToHeaders GetSipMediaApplication where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetSipMediaApplication where
  toPath GetSipMediaApplication' {..} =
    Prelude.mconcat
      [ "/sip-media-applications/",
        Data.toBS sipMediaApplicationId
      ]

instance Data.ToQuery GetSipMediaApplication where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetSipMediaApplicationResponse' smart constructor.
data GetSipMediaApplicationResponse = GetSipMediaApplicationResponse'
  { sipMediaApplication :: Prelude.Maybe SipMediaApplication,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSipMediaApplicationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sipMediaApplication', 'getSipMediaApplicationResponse_sipMediaApplication' - Undocumented member.
--
-- 'httpStatus', 'getSipMediaApplicationResponse_httpStatus' - The response's http status code.
newGetSipMediaApplicationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetSipMediaApplicationResponse
newGetSipMediaApplicationResponse pHttpStatus_ =
  GetSipMediaApplicationResponse'
    { sipMediaApplication =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
getSipMediaApplicationResponse_sipMediaApplication :: Lens.Lens' GetSipMediaApplicationResponse (Prelude.Maybe SipMediaApplication)
getSipMediaApplicationResponse_sipMediaApplication = Lens.lens (\GetSipMediaApplicationResponse' {sipMediaApplication} -> sipMediaApplication) (\s@GetSipMediaApplicationResponse' {} a -> s {sipMediaApplication = a} :: GetSipMediaApplicationResponse)

-- | The response's http status code.
getSipMediaApplicationResponse_httpStatus :: Lens.Lens' GetSipMediaApplicationResponse Prelude.Int
getSipMediaApplicationResponse_httpStatus = Lens.lens (\GetSipMediaApplicationResponse' {httpStatus} -> httpStatus) (\s@GetSipMediaApplicationResponse' {} a -> s {httpStatus = a} :: GetSipMediaApplicationResponse)

instance
  Prelude.NFData
    GetSipMediaApplicationResponse
  where
  rnf GetSipMediaApplicationResponse' {..} =
    Prelude.rnf sipMediaApplication
      `Prelude.seq` Prelude.rnf httpStatus
