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
-- Copyright   : (c) 2013-2021 Brendan Hay
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
    getStudioComponent_studioId,
    getStudioComponent_studioComponentId,

    -- * Destructuring the Response
    GetStudioComponentResponse (..),
    newGetStudioComponentResponse,

    -- * Response Lenses
    getStudioComponentResponse_studioComponent,
    getStudioComponentResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Nimble.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetStudioComponent' smart constructor.
data GetStudioComponent = GetStudioComponent'
  { -- | The studio ID.
    studioId :: Prelude.Text,
    -- | The studio component ID.
    studioComponentId :: Prelude.Text
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
-- 'studioId', 'getStudioComponent_studioId' - The studio ID.
--
-- 'studioComponentId', 'getStudioComponent_studioComponentId' - The studio component ID.
newGetStudioComponent ::
  -- | 'studioId'
  Prelude.Text ->
  -- | 'studioComponentId'
  Prelude.Text ->
  GetStudioComponent
newGetStudioComponent pStudioId_ pStudioComponentId_ =
  GetStudioComponent'
    { studioId = pStudioId_,
      studioComponentId = pStudioComponentId_
    }

-- | The studio ID.
getStudioComponent_studioId :: Lens.Lens' GetStudioComponent Prelude.Text
getStudioComponent_studioId = Lens.lens (\GetStudioComponent' {studioId} -> studioId) (\s@GetStudioComponent' {} a -> s {studioId = a} :: GetStudioComponent)

-- | The studio component ID.
getStudioComponent_studioComponentId :: Lens.Lens' GetStudioComponent Prelude.Text
getStudioComponent_studioComponentId = Lens.lens (\GetStudioComponent' {studioComponentId} -> studioComponentId) (\s@GetStudioComponent' {} a -> s {studioComponentId = a} :: GetStudioComponent)

instance Core.AWSRequest GetStudioComponent where
  type
    AWSResponse GetStudioComponent =
      GetStudioComponentResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetStudioComponentResponse'
            Prelude.<$> (x Core..?> "studioComponent")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetStudioComponent

instance Prelude.NFData GetStudioComponent

instance Core.ToHeaders GetStudioComponent where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetStudioComponent where
  toPath GetStudioComponent' {..} =
    Prelude.mconcat
      [ "/2020-08-01/studios/",
        Core.toBS studioId,
        "/studio-components/",
        Core.toBS studioComponentId
      ]

instance Core.ToQuery GetStudioComponent where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetStudioComponentResponse' smart constructor.
data GetStudioComponentResponse = GetStudioComponentResponse'
  { -- | Information about the studio component.
    studioComponent :: Prelude.Maybe StudioComponent,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Prelude.NFData GetStudioComponentResponse
