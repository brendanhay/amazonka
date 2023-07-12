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
-- Module      : Amazonka.WorkSpacesWeb.AssociateUserSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a user settings resource with a web portal.
module Amazonka.WorkSpacesWeb.AssociateUserSettings
  ( -- * Creating a Request
    AssociateUserSettings (..),
    newAssociateUserSettings,

    -- * Request Lenses
    associateUserSettings_portalArn,
    associateUserSettings_userSettingsArn,

    -- * Destructuring the Response
    AssociateUserSettingsResponse (..),
    newAssociateUserSettingsResponse,

    -- * Response Lenses
    associateUserSettingsResponse_httpStatus,
    associateUserSettingsResponse_portalArn,
    associateUserSettingsResponse_userSettingsArn,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkSpacesWeb.Types

-- | /See:/ 'newAssociateUserSettings' smart constructor.
data AssociateUserSettings = AssociateUserSettings'
  { -- | The ARN of the web portal.
    portalArn :: Prelude.Text,
    -- | The ARN of the user settings.
    userSettingsArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateUserSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'portalArn', 'associateUserSettings_portalArn' - The ARN of the web portal.
--
-- 'userSettingsArn', 'associateUserSettings_userSettingsArn' - The ARN of the user settings.
newAssociateUserSettings ::
  -- | 'portalArn'
  Prelude.Text ->
  -- | 'userSettingsArn'
  Prelude.Text ->
  AssociateUserSettings
newAssociateUserSettings
  pPortalArn_
  pUserSettingsArn_ =
    AssociateUserSettings'
      { portalArn = pPortalArn_,
        userSettingsArn = pUserSettingsArn_
      }

-- | The ARN of the web portal.
associateUserSettings_portalArn :: Lens.Lens' AssociateUserSettings Prelude.Text
associateUserSettings_portalArn = Lens.lens (\AssociateUserSettings' {portalArn} -> portalArn) (\s@AssociateUserSettings' {} a -> s {portalArn = a} :: AssociateUserSettings)

-- | The ARN of the user settings.
associateUserSettings_userSettingsArn :: Lens.Lens' AssociateUserSettings Prelude.Text
associateUserSettings_userSettingsArn = Lens.lens (\AssociateUserSettings' {userSettingsArn} -> userSettingsArn) (\s@AssociateUserSettings' {} a -> s {userSettingsArn = a} :: AssociateUserSettings)

instance Core.AWSRequest AssociateUserSettings where
  type
    AWSResponse AssociateUserSettings =
      AssociateUserSettingsResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          AssociateUserSettingsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "portalArn")
            Prelude.<*> (x Data..:> "userSettingsArn")
      )

instance Prelude.Hashable AssociateUserSettings where
  hashWithSalt _salt AssociateUserSettings' {..} =
    _salt
      `Prelude.hashWithSalt` portalArn
      `Prelude.hashWithSalt` userSettingsArn

instance Prelude.NFData AssociateUserSettings where
  rnf AssociateUserSettings' {..} =
    Prelude.rnf portalArn
      `Prelude.seq` Prelude.rnf userSettingsArn

instance Data.ToHeaders AssociateUserSettings where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON AssociateUserSettings where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath AssociateUserSettings where
  toPath AssociateUserSettings' {..} =
    Prelude.mconcat
      ["/portals/", Data.toBS portalArn, "/userSettings"]

instance Data.ToQuery AssociateUserSettings where
  toQuery AssociateUserSettings' {..} =
    Prelude.mconcat
      ["userSettingsArn" Data.=: userSettingsArn]

-- | /See:/ 'newAssociateUserSettingsResponse' smart constructor.
data AssociateUserSettingsResponse = AssociateUserSettingsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The ARN of the web portal.
    portalArn :: Prelude.Text,
    -- | The ARN of the user settings.
    userSettingsArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateUserSettingsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'associateUserSettingsResponse_httpStatus' - The response's http status code.
--
-- 'portalArn', 'associateUserSettingsResponse_portalArn' - The ARN of the web portal.
--
-- 'userSettingsArn', 'associateUserSettingsResponse_userSettingsArn' - The ARN of the user settings.
newAssociateUserSettingsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'portalArn'
  Prelude.Text ->
  -- | 'userSettingsArn'
  Prelude.Text ->
  AssociateUserSettingsResponse
newAssociateUserSettingsResponse
  pHttpStatus_
  pPortalArn_
  pUserSettingsArn_ =
    AssociateUserSettingsResponse'
      { httpStatus =
          pHttpStatus_,
        portalArn = pPortalArn_,
        userSettingsArn = pUserSettingsArn_
      }

-- | The response's http status code.
associateUserSettingsResponse_httpStatus :: Lens.Lens' AssociateUserSettingsResponse Prelude.Int
associateUserSettingsResponse_httpStatus = Lens.lens (\AssociateUserSettingsResponse' {httpStatus} -> httpStatus) (\s@AssociateUserSettingsResponse' {} a -> s {httpStatus = a} :: AssociateUserSettingsResponse)

-- | The ARN of the web portal.
associateUserSettingsResponse_portalArn :: Lens.Lens' AssociateUserSettingsResponse Prelude.Text
associateUserSettingsResponse_portalArn = Lens.lens (\AssociateUserSettingsResponse' {portalArn} -> portalArn) (\s@AssociateUserSettingsResponse' {} a -> s {portalArn = a} :: AssociateUserSettingsResponse)

-- | The ARN of the user settings.
associateUserSettingsResponse_userSettingsArn :: Lens.Lens' AssociateUserSettingsResponse Prelude.Text
associateUserSettingsResponse_userSettingsArn = Lens.lens (\AssociateUserSettingsResponse' {userSettingsArn} -> userSettingsArn) (\s@AssociateUserSettingsResponse' {} a -> s {userSettingsArn = a} :: AssociateUserSettingsResponse)

instance Prelude.NFData AssociateUserSettingsResponse where
  rnf AssociateUserSettingsResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf portalArn
      `Prelude.seq` Prelude.rnf userSettingsArn
