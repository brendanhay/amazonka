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
-- Module      : Amazonka.WorkSpacesWeb.AssociateUserAccessLoggingSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a user access logging settings resource with a web portal.
module Amazonka.WorkSpacesWeb.AssociateUserAccessLoggingSettings
  ( -- * Creating a Request
    AssociateUserAccessLoggingSettings (..),
    newAssociateUserAccessLoggingSettings,

    -- * Request Lenses
    associateUserAccessLoggingSettings_portalArn,
    associateUserAccessLoggingSettings_userAccessLoggingSettingsArn,

    -- * Destructuring the Response
    AssociateUserAccessLoggingSettingsResponse (..),
    newAssociateUserAccessLoggingSettingsResponse,

    -- * Response Lenses
    associateUserAccessLoggingSettingsResponse_httpStatus,
    associateUserAccessLoggingSettingsResponse_portalArn,
    associateUserAccessLoggingSettingsResponse_userAccessLoggingSettingsArn,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkSpacesWeb.Types

-- | /See:/ 'newAssociateUserAccessLoggingSettings' smart constructor.
data AssociateUserAccessLoggingSettings = AssociateUserAccessLoggingSettings'
  { -- | The ARN of the web portal.
    portalArn :: Prelude.Text,
    -- | The ARN of the user access logging settings.
    userAccessLoggingSettingsArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateUserAccessLoggingSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'portalArn', 'associateUserAccessLoggingSettings_portalArn' - The ARN of the web portal.
--
-- 'userAccessLoggingSettingsArn', 'associateUserAccessLoggingSettings_userAccessLoggingSettingsArn' - The ARN of the user access logging settings.
newAssociateUserAccessLoggingSettings ::
  -- | 'portalArn'
  Prelude.Text ->
  -- | 'userAccessLoggingSettingsArn'
  Prelude.Text ->
  AssociateUserAccessLoggingSettings
newAssociateUserAccessLoggingSettings
  pPortalArn_
  pUserAccessLoggingSettingsArn_ =
    AssociateUserAccessLoggingSettings'
      { portalArn =
          pPortalArn_,
        userAccessLoggingSettingsArn =
          pUserAccessLoggingSettingsArn_
      }

-- | The ARN of the web portal.
associateUserAccessLoggingSettings_portalArn :: Lens.Lens' AssociateUserAccessLoggingSettings Prelude.Text
associateUserAccessLoggingSettings_portalArn = Lens.lens (\AssociateUserAccessLoggingSettings' {portalArn} -> portalArn) (\s@AssociateUserAccessLoggingSettings' {} a -> s {portalArn = a} :: AssociateUserAccessLoggingSettings)

-- | The ARN of the user access logging settings.
associateUserAccessLoggingSettings_userAccessLoggingSettingsArn :: Lens.Lens' AssociateUserAccessLoggingSettings Prelude.Text
associateUserAccessLoggingSettings_userAccessLoggingSettingsArn = Lens.lens (\AssociateUserAccessLoggingSettings' {userAccessLoggingSettingsArn} -> userAccessLoggingSettingsArn) (\s@AssociateUserAccessLoggingSettings' {} a -> s {userAccessLoggingSettingsArn = a} :: AssociateUserAccessLoggingSettings)

instance
  Core.AWSRequest
    AssociateUserAccessLoggingSettings
  where
  type
    AWSResponse AssociateUserAccessLoggingSettings =
      AssociateUserAccessLoggingSettingsResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          AssociateUserAccessLoggingSettingsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "portalArn")
            Prelude.<*> (x Data..:> "userAccessLoggingSettingsArn")
      )

instance
  Prelude.Hashable
    AssociateUserAccessLoggingSettings
  where
  hashWithSalt
    _salt
    AssociateUserAccessLoggingSettings' {..} =
      _salt
        `Prelude.hashWithSalt` portalArn
        `Prelude.hashWithSalt` userAccessLoggingSettingsArn

instance
  Prelude.NFData
    AssociateUserAccessLoggingSettings
  where
  rnf AssociateUserAccessLoggingSettings' {..} =
    Prelude.rnf portalArn `Prelude.seq`
      Prelude.rnf userAccessLoggingSettingsArn

instance
  Data.ToHeaders
    AssociateUserAccessLoggingSettings
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
  Data.ToJSON
    AssociateUserAccessLoggingSettings
  where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance
  Data.ToPath
    AssociateUserAccessLoggingSettings
  where
  toPath AssociateUserAccessLoggingSettings' {..} =
    Prelude.mconcat
      [ "/portals/",
        Data.toBS portalArn,
        "/userAccessLoggingSettings"
      ]

instance
  Data.ToQuery
    AssociateUserAccessLoggingSettings
  where
  toQuery AssociateUserAccessLoggingSettings' {..} =
    Prelude.mconcat
      [ "userAccessLoggingSettingsArn"
          Data.=: userAccessLoggingSettingsArn
      ]

-- | /See:/ 'newAssociateUserAccessLoggingSettingsResponse' smart constructor.
data AssociateUserAccessLoggingSettingsResponse = AssociateUserAccessLoggingSettingsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The ARN of the web portal.
    portalArn :: Prelude.Text,
    -- | The ARN of the user access logging settings.
    userAccessLoggingSettingsArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateUserAccessLoggingSettingsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'associateUserAccessLoggingSettingsResponse_httpStatus' - The response's http status code.
--
-- 'portalArn', 'associateUserAccessLoggingSettingsResponse_portalArn' - The ARN of the web portal.
--
-- 'userAccessLoggingSettingsArn', 'associateUserAccessLoggingSettingsResponse_userAccessLoggingSettingsArn' - The ARN of the user access logging settings.
newAssociateUserAccessLoggingSettingsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'portalArn'
  Prelude.Text ->
  -- | 'userAccessLoggingSettingsArn'
  Prelude.Text ->
  AssociateUserAccessLoggingSettingsResponse
newAssociateUserAccessLoggingSettingsResponse
  pHttpStatus_
  pPortalArn_
  pUserAccessLoggingSettingsArn_ =
    AssociateUserAccessLoggingSettingsResponse'
      { httpStatus =
          pHttpStatus_,
        portalArn = pPortalArn_,
        userAccessLoggingSettingsArn =
          pUserAccessLoggingSettingsArn_
      }

-- | The response's http status code.
associateUserAccessLoggingSettingsResponse_httpStatus :: Lens.Lens' AssociateUserAccessLoggingSettingsResponse Prelude.Int
associateUserAccessLoggingSettingsResponse_httpStatus = Lens.lens (\AssociateUserAccessLoggingSettingsResponse' {httpStatus} -> httpStatus) (\s@AssociateUserAccessLoggingSettingsResponse' {} a -> s {httpStatus = a} :: AssociateUserAccessLoggingSettingsResponse)

-- | The ARN of the web portal.
associateUserAccessLoggingSettingsResponse_portalArn :: Lens.Lens' AssociateUserAccessLoggingSettingsResponse Prelude.Text
associateUserAccessLoggingSettingsResponse_portalArn = Lens.lens (\AssociateUserAccessLoggingSettingsResponse' {portalArn} -> portalArn) (\s@AssociateUserAccessLoggingSettingsResponse' {} a -> s {portalArn = a} :: AssociateUserAccessLoggingSettingsResponse)

-- | The ARN of the user access logging settings.
associateUserAccessLoggingSettingsResponse_userAccessLoggingSettingsArn :: Lens.Lens' AssociateUserAccessLoggingSettingsResponse Prelude.Text
associateUserAccessLoggingSettingsResponse_userAccessLoggingSettingsArn = Lens.lens (\AssociateUserAccessLoggingSettingsResponse' {userAccessLoggingSettingsArn} -> userAccessLoggingSettingsArn) (\s@AssociateUserAccessLoggingSettingsResponse' {} a -> s {userAccessLoggingSettingsArn = a} :: AssociateUserAccessLoggingSettingsResponse)

instance
  Prelude.NFData
    AssociateUserAccessLoggingSettingsResponse
  where
  rnf AssociateUserAccessLoggingSettingsResponse' {..} =
    Prelude.rnf httpStatus `Prelude.seq`
      Prelude.rnf portalArn `Prelude.seq`
        Prelude.rnf userAccessLoggingSettingsArn
