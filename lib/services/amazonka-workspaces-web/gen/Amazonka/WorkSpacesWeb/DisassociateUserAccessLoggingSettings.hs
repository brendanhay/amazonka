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
-- Module      : Amazonka.WorkSpacesWeb.DisassociateUserAccessLoggingSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates user access logging settings from a web portal.
module Amazonka.WorkSpacesWeb.DisassociateUserAccessLoggingSettings
  ( -- * Creating a Request
    DisassociateUserAccessLoggingSettings (..),
    newDisassociateUserAccessLoggingSettings,

    -- * Request Lenses
    disassociateUserAccessLoggingSettings_portalArn,

    -- * Destructuring the Response
    DisassociateUserAccessLoggingSettingsResponse (..),
    newDisassociateUserAccessLoggingSettingsResponse,

    -- * Response Lenses
    disassociateUserAccessLoggingSettingsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkSpacesWeb.Types

-- | /See:/ 'newDisassociateUserAccessLoggingSettings' smart constructor.
data DisassociateUserAccessLoggingSettings = DisassociateUserAccessLoggingSettings'
  { -- | The ARN of the web portal.
    portalArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateUserAccessLoggingSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'portalArn', 'disassociateUserAccessLoggingSettings_portalArn' - The ARN of the web portal.
newDisassociateUserAccessLoggingSettings ::
  -- | 'portalArn'
  Prelude.Text ->
  DisassociateUserAccessLoggingSettings
newDisassociateUserAccessLoggingSettings pPortalArn_ =
  DisassociateUserAccessLoggingSettings'
    { portalArn =
        pPortalArn_
    }

-- | The ARN of the web portal.
disassociateUserAccessLoggingSettings_portalArn :: Lens.Lens' DisassociateUserAccessLoggingSettings Prelude.Text
disassociateUserAccessLoggingSettings_portalArn = Lens.lens (\DisassociateUserAccessLoggingSettings' {portalArn} -> portalArn) (\s@DisassociateUserAccessLoggingSettings' {} a -> s {portalArn = a} :: DisassociateUserAccessLoggingSettings)

instance
  Core.AWSRequest
    DisassociateUserAccessLoggingSettings
  where
  type
    AWSResponse
      DisassociateUserAccessLoggingSettings =
      DisassociateUserAccessLoggingSettingsResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DisassociateUserAccessLoggingSettingsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DisassociateUserAccessLoggingSettings
  where
  hashWithSalt
    _salt
    DisassociateUserAccessLoggingSettings' {..} =
      _salt `Prelude.hashWithSalt` portalArn

instance
  Prelude.NFData
    DisassociateUserAccessLoggingSettings
  where
  rnf DisassociateUserAccessLoggingSettings' {..} =
    Prelude.rnf portalArn

instance
  Data.ToHeaders
    DisassociateUserAccessLoggingSettings
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToPath
    DisassociateUserAccessLoggingSettings
  where
  toPath DisassociateUserAccessLoggingSettings' {..} =
    Prelude.mconcat
      [ "/portals/",
        Data.toBS portalArn,
        "/userAccessLoggingSettings"
      ]

instance
  Data.ToQuery
    DisassociateUserAccessLoggingSettings
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisassociateUserAccessLoggingSettingsResponse' smart constructor.
data DisassociateUserAccessLoggingSettingsResponse = DisassociateUserAccessLoggingSettingsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateUserAccessLoggingSettingsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'disassociateUserAccessLoggingSettingsResponse_httpStatus' - The response's http status code.
newDisassociateUserAccessLoggingSettingsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DisassociateUserAccessLoggingSettingsResponse
newDisassociateUserAccessLoggingSettingsResponse
  pHttpStatus_ =
    DisassociateUserAccessLoggingSettingsResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
disassociateUserAccessLoggingSettingsResponse_httpStatus :: Lens.Lens' DisassociateUserAccessLoggingSettingsResponse Prelude.Int
disassociateUserAccessLoggingSettingsResponse_httpStatus = Lens.lens (\DisassociateUserAccessLoggingSettingsResponse' {httpStatus} -> httpStatus) (\s@DisassociateUserAccessLoggingSettingsResponse' {} a -> s {httpStatus = a} :: DisassociateUserAccessLoggingSettingsResponse)

instance
  Prelude.NFData
    DisassociateUserAccessLoggingSettingsResponse
  where
  rnf
    DisassociateUserAccessLoggingSettingsResponse' {..} =
      Prelude.rnf httpStatus
