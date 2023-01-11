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
-- Module      : Amazonka.WorkSpacesWeb.AssociateBrowserSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a browser settings resource with a web portal.
module Amazonka.WorkSpacesWeb.AssociateBrowserSettings
  ( -- * Creating a Request
    AssociateBrowserSettings (..),
    newAssociateBrowserSettings,

    -- * Request Lenses
    associateBrowserSettings_browserSettingsArn,
    associateBrowserSettings_portalArn,

    -- * Destructuring the Response
    AssociateBrowserSettingsResponse (..),
    newAssociateBrowserSettingsResponse,

    -- * Response Lenses
    associateBrowserSettingsResponse_httpStatus,
    associateBrowserSettingsResponse_browserSettingsArn,
    associateBrowserSettingsResponse_portalArn,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkSpacesWeb.Types

-- | /See:/ 'newAssociateBrowserSettings' smart constructor.
data AssociateBrowserSettings = AssociateBrowserSettings'
  { -- | The ARN of the browser settings.
    browserSettingsArn :: Prelude.Text,
    -- | The ARN of the web portal.
    portalArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateBrowserSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'browserSettingsArn', 'associateBrowserSettings_browserSettingsArn' - The ARN of the browser settings.
--
-- 'portalArn', 'associateBrowserSettings_portalArn' - The ARN of the web portal.
newAssociateBrowserSettings ::
  -- | 'browserSettingsArn'
  Prelude.Text ->
  -- | 'portalArn'
  Prelude.Text ->
  AssociateBrowserSettings
newAssociateBrowserSettings
  pBrowserSettingsArn_
  pPortalArn_ =
    AssociateBrowserSettings'
      { browserSettingsArn =
          pBrowserSettingsArn_,
        portalArn = pPortalArn_
      }

-- | The ARN of the browser settings.
associateBrowserSettings_browserSettingsArn :: Lens.Lens' AssociateBrowserSettings Prelude.Text
associateBrowserSettings_browserSettingsArn = Lens.lens (\AssociateBrowserSettings' {browserSettingsArn} -> browserSettingsArn) (\s@AssociateBrowserSettings' {} a -> s {browserSettingsArn = a} :: AssociateBrowserSettings)

-- | The ARN of the web portal.
associateBrowserSettings_portalArn :: Lens.Lens' AssociateBrowserSettings Prelude.Text
associateBrowserSettings_portalArn = Lens.lens (\AssociateBrowserSettings' {portalArn} -> portalArn) (\s@AssociateBrowserSettings' {} a -> s {portalArn = a} :: AssociateBrowserSettings)

instance Core.AWSRequest AssociateBrowserSettings where
  type
    AWSResponse AssociateBrowserSettings =
      AssociateBrowserSettingsResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          AssociateBrowserSettingsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "browserSettingsArn")
            Prelude.<*> (x Data..:> "portalArn")
      )

instance Prelude.Hashable AssociateBrowserSettings where
  hashWithSalt _salt AssociateBrowserSettings' {..} =
    _salt `Prelude.hashWithSalt` browserSettingsArn
      `Prelude.hashWithSalt` portalArn

instance Prelude.NFData AssociateBrowserSettings where
  rnf AssociateBrowserSettings' {..} =
    Prelude.rnf browserSettingsArn
      `Prelude.seq` Prelude.rnf portalArn

instance Data.ToHeaders AssociateBrowserSettings where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON AssociateBrowserSettings where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath AssociateBrowserSettings where
  toPath AssociateBrowserSettings' {..} =
    Prelude.mconcat
      [ "/portals/",
        Data.toBS portalArn,
        "/browserSettings"
      ]

instance Data.ToQuery AssociateBrowserSettings where
  toQuery AssociateBrowserSettings' {..} =
    Prelude.mconcat
      ["browserSettingsArn" Data.=: browserSettingsArn]

-- | /See:/ 'newAssociateBrowserSettingsResponse' smart constructor.
data AssociateBrowserSettingsResponse = AssociateBrowserSettingsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The ARN of the browser settings.
    browserSettingsArn :: Prelude.Text,
    -- | The ARN of the web portal.
    portalArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateBrowserSettingsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'associateBrowserSettingsResponse_httpStatus' - The response's http status code.
--
-- 'browserSettingsArn', 'associateBrowserSettingsResponse_browserSettingsArn' - The ARN of the browser settings.
--
-- 'portalArn', 'associateBrowserSettingsResponse_portalArn' - The ARN of the web portal.
newAssociateBrowserSettingsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'browserSettingsArn'
  Prelude.Text ->
  -- | 'portalArn'
  Prelude.Text ->
  AssociateBrowserSettingsResponse
newAssociateBrowserSettingsResponse
  pHttpStatus_
  pBrowserSettingsArn_
  pPortalArn_ =
    AssociateBrowserSettingsResponse'
      { httpStatus =
          pHttpStatus_,
        browserSettingsArn = pBrowserSettingsArn_,
        portalArn = pPortalArn_
      }

-- | The response's http status code.
associateBrowserSettingsResponse_httpStatus :: Lens.Lens' AssociateBrowserSettingsResponse Prelude.Int
associateBrowserSettingsResponse_httpStatus = Lens.lens (\AssociateBrowserSettingsResponse' {httpStatus} -> httpStatus) (\s@AssociateBrowserSettingsResponse' {} a -> s {httpStatus = a} :: AssociateBrowserSettingsResponse)

-- | The ARN of the browser settings.
associateBrowserSettingsResponse_browserSettingsArn :: Lens.Lens' AssociateBrowserSettingsResponse Prelude.Text
associateBrowserSettingsResponse_browserSettingsArn = Lens.lens (\AssociateBrowserSettingsResponse' {browserSettingsArn} -> browserSettingsArn) (\s@AssociateBrowserSettingsResponse' {} a -> s {browserSettingsArn = a} :: AssociateBrowserSettingsResponse)

-- | The ARN of the web portal.
associateBrowserSettingsResponse_portalArn :: Lens.Lens' AssociateBrowserSettingsResponse Prelude.Text
associateBrowserSettingsResponse_portalArn = Lens.lens (\AssociateBrowserSettingsResponse' {portalArn} -> portalArn) (\s@AssociateBrowserSettingsResponse' {} a -> s {portalArn = a} :: AssociateBrowserSettingsResponse)

instance
  Prelude.NFData
    AssociateBrowserSettingsResponse
  where
  rnf AssociateBrowserSettingsResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf browserSettingsArn
      `Prelude.seq` Prelude.rnf portalArn
