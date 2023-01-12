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
-- Module      : Amazonka.Nimble.GetStudioComponent
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a studio component resource.
module Amazonka.Nimble.GetStudioComponent
  ( -- * Creating a Request
    GetStudioComponent (..),
    newGetStudioComponent,

    -- * Request Lenses
    getStudioComponent_studioComponentId,
    getStudioComponent_studioId,

    -- * Destructuring the Response
    GetStudioComponentResponse (..),
    newGetStudioComponentResponse,

    -- * Response Lenses
    getStudioComponentResponse_studioComponent,
    getStudioComponentResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Nimble.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetStudioComponent' smart constructor.
data GetStudioComponent = GetStudioComponent'
  { -- | The studio component ID.
    studioComponentId :: Prelude.Text,
    -- | The studio ID.
    studioId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetStudioComponent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'studioComponentId', 'getStudioComponent_studioComponentId' - The studio component ID.
--
-- 'studioId', 'getStudioComponent_studioId' - The studio ID.
newGetStudioComponent ::
  -- | 'studioComponentId'
  Prelude.Text ->
  -- | 'studioId'
  Prelude.Text ->
  GetStudioComponent
newGetStudioComponent pStudioComponentId_ pStudioId_ =
  GetStudioComponent'
    { studioComponentId =
        pStudioComponentId_,
      studioId = pStudioId_
    }

-- | The studio component ID.
getStudioComponent_studioComponentId :: Lens.Lens' GetStudioComponent Prelude.Text
getStudioComponent_studioComponentId = Lens.lens (\GetStudioComponent' {studioComponentId} -> studioComponentId) (\s@GetStudioComponent' {} a -> s {studioComponentId = a} :: GetStudioComponent)

-- | The studio ID.
getStudioComponent_studioId :: Lens.Lens' GetStudioComponent Prelude.Text
getStudioComponent_studioId = Lens.lens (\GetStudioComponent' {studioId} -> studioId) (\s@GetStudioComponent' {} a -> s {studioId = a} :: GetStudioComponent)

instance Core.AWSRequest GetStudioComponent where
  type
    AWSResponse GetStudioComponent =
      GetStudioComponentResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetStudioComponentResponse'
            Prelude.<$> (x Data..?> "studioComponent")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetStudioComponent where
  hashWithSalt _salt GetStudioComponent' {..} =
    _salt `Prelude.hashWithSalt` studioComponentId
      `Prelude.hashWithSalt` studioId

instance Prelude.NFData GetStudioComponent where
  rnf GetStudioComponent' {..} =
    Prelude.rnf studioComponentId
      `Prelude.seq` Prelude.rnf studioId

instance Data.ToHeaders GetStudioComponent where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetStudioComponent where
  toPath GetStudioComponent' {..} =
    Prelude.mconcat
      [ "/2020-08-01/studios/",
        Data.toBS studioId,
        "/studio-components/",
        Data.toBS studioComponentId
      ]

instance Data.ToQuery GetStudioComponent where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetStudioComponentResponse' smart constructor.
data GetStudioComponentResponse = GetStudioComponentResponse'
  { -- | Information about the studio component.
    studioComponent :: Prelude.Maybe StudioComponent,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetStudioComponentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'studioComponent', 'getStudioComponentResponse_studioComponent' - Information about the studio component.
--
-- 'httpStatus', 'getStudioComponentResponse_httpStatus' - The response's http status code.
newGetStudioComponentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetStudioComponentResponse
newGetStudioComponentResponse pHttpStatus_ =
  GetStudioComponentResponse'
    { studioComponent =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the studio component.
getStudioComponentResponse_studioComponent :: Lens.Lens' GetStudioComponentResponse (Prelude.Maybe StudioComponent)
getStudioComponentResponse_studioComponent = Lens.lens (\GetStudioComponentResponse' {studioComponent} -> studioComponent) (\s@GetStudioComponentResponse' {} a -> s {studioComponent = a} :: GetStudioComponentResponse)

-- | The response's http status code.
getStudioComponentResponse_httpStatus :: Lens.Lens' GetStudioComponentResponse Prelude.Int
getStudioComponentResponse_httpStatus = Lens.lens (\GetStudioComponentResponse' {httpStatus} -> httpStatus) (\s@GetStudioComponentResponse' {} a -> s {httpStatus = a} :: GetStudioComponentResponse)

instance Prelude.NFData GetStudioComponentResponse where
  rnf GetStudioComponentResponse' {..} =
    Prelude.rnf studioComponent
      `Prelude.seq` Prelude.rnf httpStatus
