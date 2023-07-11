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
-- Module      : Amazonka.WorkSpacesWeb.UpdatePortal
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a web portal.
module Amazonka.WorkSpacesWeb.UpdatePortal
  ( -- * Creating a Request
    UpdatePortal (..),
    newUpdatePortal,

    -- * Request Lenses
    updatePortal_displayName,
    updatePortal_portalArn,

    -- * Destructuring the Response
    UpdatePortalResponse (..),
    newUpdatePortalResponse,

    -- * Response Lenses
    updatePortalResponse_portal,
    updatePortalResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkSpacesWeb.Types

-- | /See:/ 'newUpdatePortal' smart constructor.
data UpdatePortal = UpdatePortal'
  { -- | The name of the web portal. This is not visible to users who log into
    -- the web portal.
    displayName :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The ARN of the web portal.
    portalArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdatePortal' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'displayName', 'updatePortal_displayName' - The name of the web portal. This is not visible to users who log into
-- the web portal.
--
-- 'portalArn', 'updatePortal_portalArn' - The ARN of the web portal.
newUpdatePortal ::
  -- | 'portalArn'
  Prelude.Text ->
  UpdatePortal
newUpdatePortal pPortalArn_ =
  UpdatePortal'
    { displayName = Prelude.Nothing,
      portalArn = pPortalArn_
    }

-- | The name of the web portal. This is not visible to users who log into
-- the web portal.
updatePortal_displayName :: Lens.Lens' UpdatePortal (Prelude.Maybe Prelude.Text)
updatePortal_displayName = Lens.lens (\UpdatePortal' {displayName} -> displayName) (\s@UpdatePortal' {} a -> s {displayName = a} :: UpdatePortal) Prelude.. Lens.mapping Data._Sensitive

-- | The ARN of the web portal.
updatePortal_portalArn :: Lens.Lens' UpdatePortal Prelude.Text
updatePortal_portalArn = Lens.lens (\UpdatePortal' {portalArn} -> portalArn) (\s@UpdatePortal' {} a -> s {portalArn = a} :: UpdatePortal)

instance Core.AWSRequest UpdatePortal where
  type AWSResponse UpdatePortal = UpdatePortalResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdatePortalResponse'
            Prelude.<$> (x Data..?> "portal")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdatePortal where
  hashWithSalt _salt UpdatePortal' {..} =
    _salt
      `Prelude.hashWithSalt` displayName
      `Prelude.hashWithSalt` portalArn

instance Prelude.NFData UpdatePortal where
  rnf UpdatePortal' {..} =
    Prelude.rnf displayName
      `Prelude.seq` Prelude.rnf portalArn

instance Data.ToHeaders UpdatePortal where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdatePortal where
  toJSON UpdatePortal' {..} =
    Data.object
      ( Prelude.catMaybes
          [("displayName" Data..=) Prelude.<$> displayName]
      )

instance Data.ToPath UpdatePortal where
  toPath UpdatePortal' {..} =
    Prelude.mconcat ["/portals/", Data.toBS portalArn]

instance Data.ToQuery UpdatePortal where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdatePortalResponse' smart constructor.
data UpdatePortalResponse = UpdatePortalResponse'
  { -- | The web portal.
    portal :: Prelude.Maybe Portal,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdatePortalResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'portal', 'updatePortalResponse_portal' - The web portal.
--
-- 'httpStatus', 'updatePortalResponse_httpStatus' - The response's http status code.
newUpdatePortalResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdatePortalResponse
newUpdatePortalResponse pHttpStatus_ =
  UpdatePortalResponse'
    { portal = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The web portal.
updatePortalResponse_portal :: Lens.Lens' UpdatePortalResponse (Prelude.Maybe Portal)
updatePortalResponse_portal = Lens.lens (\UpdatePortalResponse' {portal} -> portal) (\s@UpdatePortalResponse' {} a -> s {portal = a} :: UpdatePortalResponse)

-- | The response's http status code.
updatePortalResponse_httpStatus :: Lens.Lens' UpdatePortalResponse Prelude.Int
updatePortalResponse_httpStatus = Lens.lens (\UpdatePortalResponse' {httpStatus} -> httpStatus) (\s@UpdatePortalResponse' {} a -> s {httpStatus = a} :: UpdatePortalResponse)

instance Prelude.NFData UpdatePortalResponse where
  rnf UpdatePortalResponse' {..} =
    Prelude.rnf portal
      `Prelude.seq` Prelude.rnf httpStatus
