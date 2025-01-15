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
-- Module      : Amazonka.Nimble.GetStudio
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get a studio resource.
module Amazonka.Nimble.GetStudio
  ( -- * Creating a Request
    GetStudio (..),
    newGetStudio,

    -- * Request Lenses
    getStudio_studioId,

    -- * Destructuring the Response
    GetStudioResponse (..),
    newGetStudioResponse,

    -- * Response Lenses
    getStudioResponse_httpStatus,
    getStudioResponse_studio,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Nimble.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetStudio' smart constructor.
data GetStudio = GetStudio'
  { -- | The studio ID.
    studioId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetStudio' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'studioId', 'getStudio_studioId' - The studio ID.
newGetStudio ::
  -- | 'studioId'
  Prelude.Text ->
  GetStudio
newGetStudio pStudioId_ =
  GetStudio' {studioId = pStudioId_}

-- | The studio ID.
getStudio_studioId :: Lens.Lens' GetStudio Prelude.Text
getStudio_studioId = Lens.lens (\GetStudio' {studioId} -> studioId) (\s@GetStudio' {} a -> s {studioId = a} :: GetStudio)

instance Core.AWSRequest GetStudio where
  type AWSResponse GetStudio = GetStudioResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetStudioResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "studio")
      )

instance Prelude.Hashable GetStudio where
  hashWithSalt _salt GetStudio' {..} =
    _salt `Prelude.hashWithSalt` studioId

instance Prelude.NFData GetStudio where
  rnf GetStudio' {..} = Prelude.rnf studioId

instance Data.ToHeaders GetStudio where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetStudio where
  toPath GetStudio' {..} =
    Prelude.mconcat
      ["/2020-08-01/studios/", Data.toBS studioId]

instance Data.ToQuery GetStudio where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetStudioResponse' smart constructor.
data GetStudioResponse = GetStudioResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Information about a studio.
    studio :: Studio
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetStudioResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getStudioResponse_httpStatus' - The response's http status code.
--
-- 'studio', 'getStudioResponse_studio' - Information about a studio.
newGetStudioResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'studio'
  Studio ->
  GetStudioResponse
newGetStudioResponse pHttpStatus_ pStudio_ =
  GetStudioResponse'
    { httpStatus = pHttpStatus_,
      studio = pStudio_
    }

-- | The response's http status code.
getStudioResponse_httpStatus :: Lens.Lens' GetStudioResponse Prelude.Int
getStudioResponse_httpStatus = Lens.lens (\GetStudioResponse' {httpStatus} -> httpStatus) (\s@GetStudioResponse' {} a -> s {httpStatus = a} :: GetStudioResponse)

-- | Information about a studio.
getStudioResponse_studio :: Lens.Lens' GetStudioResponse Studio
getStudioResponse_studio = Lens.lens (\GetStudioResponse' {studio} -> studio) (\s@GetStudioResponse' {} a -> s {studio = a} :: GetStudioResponse)

instance Prelude.NFData GetStudioResponse where
  rnf GetStudioResponse' {..} =
    Prelude.rnf httpStatus `Prelude.seq`
      Prelude.rnf studio
