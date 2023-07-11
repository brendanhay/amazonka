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
-- Module      : Amazonka.WorkSpacesWeb.DisassociateUserSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates user settings from a web portal.
module Amazonka.WorkSpacesWeb.DisassociateUserSettings
  ( -- * Creating a Request
    DisassociateUserSettings (..),
    newDisassociateUserSettings,

    -- * Request Lenses
    disassociateUserSettings_portalArn,

    -- * Destructuring the Response
    DisassociateUserSettingsResponse (..),
    newDisassociateUserSettingsResponse,

    -- * Response Lenses
    disassociateUserSettingsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkSpacesWeb.Types

-- | /See:/ 'newDisassociateUserSettings' smart constructor.
data DisassociateUserSettings = DisassociateUserSettings'
  { -- | The ARN of the web portal.
    portalArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateUserSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'portalArn', 'disassociateUserSettings_portalArn' - The ARN of the web portal.
newDisassociateUserSettings ::
  -- | 'portalArn'
  Prelude.Text ->
  DisassociateUserSettings
newDisassociateUserSettings pPortalArn_ =
  DisassociateUserSettings' {portalArn = pPortalArn_}

-- | The ARN of the web portal.
disassociateUserSettings_portalArn :: Lens.Lens' DisassociateUserSettings Prelude.Text
disassociateUserSettings_portalArn = Lens.lens (\DisassociateUserSettings' {portalArn} -> portalArn) (\s@DisassociateUserSettings' {} a -> s {portalArn = a} :: DisassociateUserSettings)

instance Core.AWSRequest DisassociateUserSettings where
  type
    AWSResponse DisassociateUserSettings =
      DisassociateUserSettingsResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DisassociateUserSettingsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DisassociateUserSettings where
  hashWithSalt _salt DisassociateUserSettings' {..} =
    _salt `Prelude.hashWithSalt` portalArn

instance Prelude.NFData DisassociateUserSettings where
  rnf DisassociateUserSettings' {..} =
    Prelude.rnf portalArn

instance Data.ToHeaders DisassociateUserSettings where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DisassociateUserSettings where
  toPath DisassociateUserSettings' {..} =
    Prelude.mconcat
      ["/portals/", Data.toBS portalArn, "/userSettings"]

instance Data.ToQuery DisassociateUserSettings where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisassociateUserSettingsResponse' smart constructor.
data DisassociateUserSettingsResponse = DisassociateUserSettingsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateUserSettingsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'disassociateUserSettingsResponse_httpStatus' - The response's http status code.
newDisassociateUserSettingsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DisassociateUserSettingsResponse
newDisassociateUserSettingsResponse pHttpStatus_ =
  DisassociateUserSettingsResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
disassociateUserSettingsResponse_httpStatus :: Lens.Lens' DisassociateUserSettingsResponse Prelude.Int
disassociateUserSettingsResponse_httpStatus = Lens.lens (\DisassociateUserSettingsResponse' {httpStatus} -> httpStatus) (\s@DisassociateUserSettingsResponse' {} a -> s {httpStatus = a} :: DisassociateUserSettingsResponse)

instance
  Prelude.NFData
    DisassociateUserSettingsResponse
  where
  rnf DisassociateUserSettingsResponse' {..} =
    Prelude.rnf httpStatus
