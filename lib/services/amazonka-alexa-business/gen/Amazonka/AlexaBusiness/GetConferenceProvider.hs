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
-- Module      : Amazonka.AlexaBusiness.GetConferenceProvider
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets details about a specific conference provider.
module Amazonka.AlexaBusiness.GetConferenceProvider
  ( -- * Creating a Request
    GetConferenceProvider (..),
    newGetConferenceProvider,

    -- * Request Lenses
    getConferenceProvider_conferenceProviderArn,

    -- * Destructuring the Response
    GetConferenceProviderResponse (..),
    newGetConferenceProviderResponse,

    -- * Response Lenses
    getConferenceProviderResponse_conferenceProvider,
    getConferenceProviderResponse_httpStatus,
  )
where

import Amazonka.AlexaBusiness.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetConferenceProvider' smart constructor.
data GetConferenceProvider = GetConferenceProvider'
  { -- | The ARN of the newly created conference provider.
    conferenceProviderArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetConferenceProvider' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'conferenceProviderArn', 'getConferenceProvider_conferenceProviderArn' - The ARN of the newly created conference provider.
newGetConferenceProvider ::
  -- | 'conferenceProviderArn'
  Prelude.Text ->
  GetConferenceProvider
newGetConferenceProvider pConferenceProviderArn_ =
  GetConferenceProvider'
    { conferenceProviderArn =
        pConferenceProviderArn_
    }

-- | The ARN of the newly created conference provider.
getConferenceProvider_conferenceProviderArn :: Lens.Lens' GetConferenceProvider Prelude.Text
getConferenceProvider_conferenceProviderArn = Lens.lens (\GetConferenceProvider' {conferenceProviderArn} -> conferenceProviderArn) (\s@GetConferenceProvider' {} a -> s {conferenceProviderArn = a} :: GetConferenceProvider)

instance Core.AWSRequest GetConferenceProvider where
  type
    AWSResponse GetConferenceProvider =
      GetConferenceProviderResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetConferenceProviderResponse'
            Prelude.<$> (x Data..?> "ConferenceProvider")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetConferenceProvider where
  hashWithSalt _salt GetConferenceProvider' {..} =
    _salt `Prelude.hashWithSalt` conferenceProviderArn

instance Prelude.NFData GetConferenceProvider where
  rnf GetConferenceProvider' {..} =
    Prelude.rnf conferenceProviderArn

instance Data.ToHeaders GetConferenceProvider where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AlexaForBusiness.GetConferenceProvider" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetConferenceProvider where
  toJSON GetConferenceProvider' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "ConferenceProviderArn"
                  Data..= conferenceProviderArn
              )
          ]
      )

instance Data.ToPath GetConferenceProvider where
  toPath = Prelude.const "/"

instance Data.ToQuery GetConferenceProvider where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetConferenceProviderResponse' smart constructor.
data GetConferenceProviderResponse = GetConferenceProviderResponse'
  { -- | The conference provider.
    conferenceProvider :: Prelude.Maybe ConferenceProvider,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetConferenceProviderResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'conferenceProvider', 'getConferenceProviderResponse_conferenceProvider' - The conference provider.
--
-- 'httpStatus', 'getConferenceProviderResponse_httpStatus' - The response's http status code.
newGetConferenceProviderResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetConferenceProviderResponse
newGetConferenceProviderResponse pHttpStatus_ =
  GetConferenceProviderResponse'
    { conferenceProvider =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The conference provider.
getConferenceProviderResponse_conferenceProvider :: Lens.Lens' GetConferenceProviderResponse (Prelude.Maybe ConferenceProvider)
getConferenceProviderResponse_conferenceProvider = Lens.lens (\GetConferenceProviderResponse' {conferenceProvider} -> conferenceProvider) (\s@GetConferenceProviderResponse' {} a -> s {conferenceProvider = a} :: GetConferenceProviderResponse)

-- | The response's http status code.
getConferenceProviderResponse_httpStatus :: Lens.Lens' GetConferenceProviderResponse Prelude.Int
getConferenceProviderResponse_httpStatus = Lens.lens (\GetConferenceProviderResponse' {httpStatus} -> httpStatus) (\s@GetConferenceProviderResponse' {} a -> s {httpStatus = a} :: GetConferenceProviderResponse)

instance Prelude.NFData GetConferenceProviderResponse where
  rnf GetConferenceProviderResponse' {..} =
    Prelude.rnf conferenceProvider
      `Prelude.seq` Prelude.rnf httpStatus
