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
-- Module      : Amazonka.WorkSpacesWeb.DisassociateBrowserSettings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates browser settings from a web portal.
module Amazonka.WorkSpacesWeb.DisassociateBrowserSettings
  ( -- * Creating a Request
    DisassociateBrowserSettings (..),
    newDisassociateBrowserSettings,

    -- * Request Lenses
    disassociateBrowserSettings_portalArn,

    -- * Destructuring the Response
    DisassociateBrowserSettingsResponse (..),
    newDisassociateBrowserSettingsResponse,

    -- * Response Lenses
    disassociateBrowserSettingsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkSpacesWeb.Types

-- | /See:/ 'newDisassociateBrowserSettings' smart constructor.
data DisassociateBrowserSettings = DisassociateBrowserSettings'
  { -- | The ARN of the web portal.
    portalArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateBrowserSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'portalArn', 'disassociateBrowserSettings_portalArn' - The ARN of the web portal.
newDisassociateBrowserSettings ::
  -- | 'portalArn'
  Prelude.Text ->
  DisassociateBrowserSettings
newDisassociateBrowserSettings pPortalArn_ =
  DisassociateBrowserSettings'
    { portalArn =
        pPortalArn_
    }

-- | The ARN of the web portal.
disassociateBrowserSettings_portalArn :: Lens.Lens' DisassociateBrowserSettings Prelude.Text
disassociateBrowserSettings_portalArn = Lens.lens (\DisassociateBrowserSettings' {portalArn} -> portalArn) (\s@DisassociateBrowserSettings' {} a -> s {portalArn = a} :: DisassociateBrowserSettings)

instance Core.AWSRequest DisassociateBrowserSettings where
  type
    AWSResponse DisassociateBrowserSettings =
      DisassociateBrowserSettingsResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DisassociateBrowserSettingsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DisassociateBrowserSettings where
  hashWithSalt _salt DisassociateBrowserSettings' {..} =
    _salt `Prelude.hashWithSalt` portalArn

instance Prelude.NFData DisassociateBrowserSettings where
  rnf DisassociateBrowserSettings' {..} =
    Prelude.rnf portalArn

instance Data.ToHeaders DisassociateBrowserSettings where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DisassociateBrowserSettings where
  toPath DisassociateBrowserSettings' {..} =
    Prelude.mconcat
      [ "/portals/",
        Data.toBS portalArn,
        "/browserSettings"
      ]

instance Data.ToQuery DisassociateBrowserSettings where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisassociateBrowserSettingsResponse' smart constructor.
data DisassociateBrowserSettingsResponse = DisassociateBrowserSettingsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateBrowserSettingsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'disassociateBrowserSettingsResponse_httpStatus' - The response's http status code.
newDisassociateBrowserSettingsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DisassociateBrowserSettingsResponse
newDisassociateBrowserSettingsResponse pHttpStatus_ =
  DisassociateBrowserSettingsResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
disassociateBrowserSettingsResponse_httpStatus :: Lens.Lens' DisassociateBrowserSettingsResponse Prelude.Int
disassociateBrowserSettingsResponse_httpStatus = Lens.lens (\DisassociateBrowserSettingsResponse' {httpStatus} -> httpStatus) (\s@DisassociateBrowserSettingsResponse' {} a -> s {httpStatus = a} :: DisassociateBrowserSettingsResponse)

instance
  Prelude.NFData
    DisassociateBrowserSettingsResponse
  where
  rnf DisassociateBrowserSettingsResponse' {..} =
    Prelude.rnf httpStatus
