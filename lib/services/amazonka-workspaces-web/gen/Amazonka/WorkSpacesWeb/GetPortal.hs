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
-- Module      : Amazonka.WorkSpacesWeb.GetPortal
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the web portal.
module Amazonka.WorkSpacesWeb.GetPortal
  ( -- * Creating a Request
    GetPortal (..),
    newGetPortal,

    -- * Request Lenses
    getPortal_portalArn,

    -- * Destructuring the Response
    GetPortalResponse (..),
    newGetPortalResponse,

    -- * Response Lenses
    getPortalResponse_portal,
    getPortalResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkSpacesWeb.Types

-- | /See:/ 'newGetPortal' smart constructor.
data GetPortal = GetPortal'
  { -- | The ARN of the web portal.
    portalArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetPortal' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'portalArn', 'getPortal_portalArn' - The ARN of the web portal.
newGetPortal ::
  -- | 'portalArn'
  Prelude.Text ->
  GetPortal
newGetPortal pPortalArn_ =
  GetPortal' {portalArn = pPortalArn_}

-- | The ARN of the web portal.
getPortal_portalArn :: Lens.Lens' GetPortal Prelude.Text
getPortal_portalArn = Lens.lens (\GetPortal' {portalArn} -> portalArn) (\s@GetPortal' {} a -> s {portalArn = a} :: GetPortal)

instance Core.AWSRequest GetPortal where
  type AWSResponse GetPortal = GetPortalResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetPortalResponse'
            Prelude.<$> (x Core..?> "portal")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetPortal where
  hashWithSalt _salt GetPortal' {..} =
    _salt `Prelude.hashWithSalt` portalArn

instance Prelude.NFData GetPortal where
  rnf GetPortal' {..} = Prelude.rnf portalArn

instance Core.ToHeaders GetPortal where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetPortal where
  toPath GetPortal' {..} =
    Prelude.mconcat ["/portals/", Core.toBS portalArn]

instance Core.ToQuery GetPortal where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetPortalResponse' smart constructor.
data GetPortalResponse = GetPortalResponse'
  { -- | The web portal.
    portal :: Prelude.Maybe Portal,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetPortalResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'portal', 'getPortalResponse_portal' - The web portal.
--
-- 'httpStatus', 'getPortalResponse_httpStatus' - The response's http status code.
newGetPortalResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetPortalResponse
newGetPortalResponse pHttpStatus_ =
  GetPortalResponse'
    { portal = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The web portal.
getPortalResponse_portal :: Lens.Lens' GetPortalResponse (Prelude.Maybe Portal)
getPortalResponse_portal = Lens.lens (\GetPortalResponse' {portal} -> portal) (\s@GetPortalResponse' {} a -> s {portal = a} :: GetPortalResponse)

-- | The response's http status code.
getPortalResponse_httpStatus :: Lens.Lens' GetPortalResponse Prelude.Int
getPortalResponse_httpStatus = Lens.lens (\GetPortalResponse' {httpStatus} -> httpStatus) (\s@GetPortalResponse' {} a -> s {httpStatus = a} :: GetPortalResponse)

instance Prelude.NFData GetPortalResponse where
  rnf GetPortalResponse' {..} =
    Prelude.rnf portal
      `Prelude.seq` Prelude.rnf httpStatus
