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
-- Module      : Amazonka.WorkSpacesWeb.DisassociateNetworkSettings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates network settings from a web portal.
module Amazonka.WorkSpacesWeb.DisassociateNetworkSettings
  ( -- * Creating a Request
    DisassociateNetworkSettings (..),
    newDisassociateNetworkSettings,

    -- * Request Lenses
    disassociateNetworkSettings_portalArn,

    -- * Destructuring the Response
    DisassociateNetworkSettingsResponse (..),
    newDisassociateNetworkSettingsResponse,

    -- * Response Lenses
    disassociateNetworkSettingsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkSpacesWeb.Types

-- | /See:/ 'newDisassociateNetworkSettings' smart constructor.
data DisassociateNetworkSettings = DisassociateNetworkSettings'
  { -- | The ARN of the web portal.
    portalArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateNetworkSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'portalArn', 'disassociateNetworkSettings_portalArn' - The ARN of the web portal.
newDisassociateNetworkSettings ::
  -- | 'portalArn'
  Prelude.Text ->
  DisassociateNetworkSettings
newDisassociateNetworkSettings pPortalArn_ =
  DisassociateNetworkSettings'
    { portalArn =
        pPortalArn_
    }

-- | The ARN of the web portal.
disassociateNetworkSettings_portalArn :: Lens.Lens' DisassociateNetworkSettings Prelude.Text
disassociateNetworkSettings_portalArn = Lens.lens (\DisassociateNetworkSettings' {portalArn} -> portalArn) (\s@DisassociateNetworkSettings' {} a -> s {portalArn = a} :: DisassociateNetworkSettings)

instance Core.AWSRequest DisassociateNetworkSettings where
  type
    AWSResponse DisassociateNetworkSettings =
      DisassociateNetworkSettingsResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DisassociateNetworkSettingsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DisassociateNetworkSettings where
  hashWithSalt _salt DisassociateNetworkSettings' {..} =
    _salt `Prelude.hashWithSalt` portalArn

instance Prelude.NFData DisassociateNetworkSettings where
  rnf DisassociateNetworkSettings' {..} =
    Prelude.rnf portalArn

instance Core.ToHeaders DisassociateNetworkSettings where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DisassociateNetworkSettings where
  toPath DisassociateNetworkSettings' {..} =
    Prelude.mconcat
      [ "/portals/",
        Core.toBS portalArn,
        "/networkSettings"
      ]

instance Core.ToQuery DisassociateNetworkSettings where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisassociateNetworkSettingsResponse' smart constructor.
data DisassociateNetworkSettingsResponse = DisassociateNetworkSettingsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateNetworkSettingsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'disassociateNetworkSettingsResponse_httpStatus' - The response's http status code.
newDisassociateNetworkSettingsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DisassociateNetworkSettingsResponse
newDisassociateNetworkSettingsResponse pHttpStatus_ =
  DisassociateNetworkSettingsResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
disassociateNetworkSettingsResponse_httpStatus :: Lens.Lens' DisassociateNetworkSettingsResponse Prelude.Int
disassociateNetworkSettingsResponse_httpStatus = Lens.lens (\DisassociateNetworkSettingsResponse' {httpStatus} -> httpStatus) (\s@DisassociateNetworkSettingsResponse' {} a -> s {httpStatus = a} :: DisassociateNetworkSettingsResponse)

instance
  Prelude.NFData
    DisassociateNetworkSettingsResponse
  where
  rnf DisassociateNetworkSettingsResponse' {..} =
    Prelude.rnf httpStatus
