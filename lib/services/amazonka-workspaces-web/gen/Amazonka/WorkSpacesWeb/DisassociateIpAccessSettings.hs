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
-- Module      : Amazonka.WorkSpacesWeb.DisassociateIpAccessSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates IP access settings from a web portal.
module Amazonka.WorkSpacesWeb.DisassociateIpAccessSettings
  ( -- * Creating a Request
    DisassociateIpAccessSettings (..),
    newDisassociateIpAccessSettings,

    -- * Request Lenses
    disassociateIpAccessSettings_portalArn,

    -- * Destructuring the Response
    DisassociateIpAccessSettingsResponse (..),
    newDisassociateIpAccessSettingsResponse,

    -- * Response Lenses
    disassociateIpAccessSettingsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkSpacesWeb.Types

-- | /See:/ 'newDisassociateIpAccessSettings' smart constructor.
data DisassociateIpAccessSettings = DisassociateIpAccessSettings'
  { -- | The ARN of the web portal.
    portalArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateIpAccessSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'portalArn', 'disassociateIpAccessSettings_portalArn' - The ARN of the web portal.
newDisassociateIpAccessSettings ::
  -- | 'portalArn'
  Prelude.Text ->
  DisassociateIpAccessSettings
newDisassociateIpAccessSettings pPortalArn_ =
  DisassociateIpAccessSettings'
    { portalArn =
        pPortalArn_
    }

-- | The ARN of the web portal.
disassociateIpAccessSettings_portalArn :: Lens.Lens' DisassociateIpAccessSettings Prelude.Text
disassociateIpAccessSettings_portalArn = Lens.lens (\DisassociateIpAccessSettings' {portalArn} -> portalArn) (\s@DisassociateIpAccessSettings' {} a -> s {portalArn = a} :: DisassociateIpAccessSettings)

instance Core.AWSRequest DisassociateIpAccessSettings where
  type
    AWSResponse DisassociateIpAccessSettings =
      DisassociateIpAccessSettingsResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DisassociateIpAccessSettingsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DisassociateIpAccessSettings
  where
  hashWithSalt _salt DisassociateIpAccessSettings' {..} =
    _salt `Prelude.hashWithSalt` portalArn

instance Prelude.NFData DisassociateIpAccessSettings where
  rnf DisassociateIpAccessSettings' {..} =
    Prelude.rnf portalArn

instance Data.ToHeaders DisassociateIpAccessSettings where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DisassociateIpAccessSettings where
  toPath DisassociateIpAccessSettings' {..} =
    Prelude.mconcat
      [ "/portals/",
        Data.toBS portalArn,
        "/ipAccessSettings"
      ]

instance Data.ToQuery DisassociateIpAccessSettings where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisassociateIpAccessSettingsResponse' smart constructor.
data DisassociateIpAccessSettingsResponse = DisassociateIpAccessSettingsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateIpAccessSettingsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'disassociateIpAccessSettingsResponse_httpStatus' - The response's http status code.
newDisassociateIpAccessSettingsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DisassociateIpAccessSettingsResponse
newDisassociateIpAccessSettingsResponse pHttpStatus_ =
  DisassociateIpAccessSettingsResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
disassociateIpAccessSettingsResponse_httpStatus :: Lens.Lens' DisassociateIpAccessSettingsResponse Prelude.Int
disassociateIpAccessSettingsResponse_httpStatus = Lens.lens (\DisassociateIpAccessSettingsResponse' {httpStatus} -> httpStatus) (\s@DisassociateIpAccessSettingsResponse' {} a -> s {httpStatus = a} :: DisassociateIpAccessSettingsResponse)

instance
  Prelude.NFData
    DisassociateIpAccessSettingsResponse
  where
  rnf DisassociateIpAccessSettingsResponse' {..} =
    Prelude.rnf httpStatus
