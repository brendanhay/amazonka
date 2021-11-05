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
-- Module      : Network.AWS.ChimeSDKIdentity.CreateAppInstanceAdmin
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Promotes an @AppInstanceUser@ to an @AppInstanceAdmin@. The promoted
-- user can perform the following actions.
--
-- -   @ChannelModerator@ actions across all channels in the @AppInstance@.
--
-- -   @DeleteChannelMessage@ actions.
--
-- Only an @AppInstanceUser@ can be promoted to an @AppInstanceAdmin@ role.
module Network.AWS.ChimeSDKIdentity.CreateAppInstanceAdmin
  ( -- * Creating a Request
    CreateAppInstanceAdmin (..),
    newCreateAppInstanceAdmin,

    -- * Request Lenses
    createAppInstanceAdmin_appInstanceAdminArn,
    createAppInstanceAdmin_appInstanceArn,

    -- * Destructuring the Response
    CreateAppInstanceAdminResponse (..),
    newCreateAppInstanceAdminResponse,

    -- * Response Lenses
    createAppInstanceAdminResponse_appInstanceAdmin,
    createAppInstanceAdminResponse_appInstanceArn,
    createAppInstanceAdminResponse_httpStatus,
  )
where

import Network.AWS.ChimeSDKIdentity.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateAppInstanceAdmin' smart constructor.
data CreateAppInstanceAdmin = CreateAppInstanceAdmin'
  { -- | The ARN of the administrator of the current @AppInstance@.
    appInstanceAdminArn :: Prelude.Text,
    -- | The ARN of the @AppInstance@.
    appInstanceArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateAppInstanceAdmin' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appInstanceAdminArn', 'createAppInstanceAdmin_appInstanceAdminArn' - The ARN of the administrator of the current @AppInstance@.
--
-- 'appInstanceArn', 'createAppInstanceAdmin_appInstanceArn' - The ARN of the @AppInstance@.
newCreateAppInstanceAdmin ::
  -- | 'appInstanceAdminArn'
  Prelude.Text ->
  -- | 'appInstanceArn'
  Prelude.Text ->
  CreateAppInstanceAdmin
newCreateAppInstanceAdmin
  pAppInstanceAdminArn_
  pAppInstanceArn_ =
    CreateAppInstanceAdmin'
      { appInstanceAdminArn =
          pAppInstanceAdminArn_,
        appInstanceArn = pAppInstanceArn_
      }

-- | The ARN of the administrator of the current @AppInstance@.
createAppInstanceAdmin_appInstanceAdminArn :: Lens.Lens' CreateAppInstanceAdmin Prelude.Text
createAppInstanceAdmin_appInstanceAdminArn = Lens.lens (\CreateAppInstanceAdmin' {appInstanceAdminArn} -> appInstanceAdminArn) (\s@CreateAppInstanceAdmin' {} a -> s {appInstanceAdminArn = a} :: CreateAppInstanceAdmin)

-- | The ARN of the @AppInstance@.
createAppInstanceAdmin_appInstanceArn :: Lens.Lens' CreateAppInstanceAdmin Prelude.Text
createAppInstanceAdmin_appInstanceArn = Lens.lens (\CreateAppInstanceAdmin' {appInstanceArn} -> appInstanceArn) (\s@CreateAppInstanceAdmin' {} a -> s {appInstanceArn = a} :: CreateAppInstanceAdmin)

instance Core.AWSRequest CreateAppInstanceAdmin where
  type
    AWSResponse CreateAppInstanceAdmin =
      CreateAppInstanceAdminResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateAppInstanceAdminResponse'
            Prelude.<$> (x Core..?> "AppInstanceAdmin")
            Prelude.<*> (x Core..?> "AppInstanceArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateAppInstanceAdmin

instance Prelude.NFData CreateAppInstanceAdmin

instance Core.ToHeaders CreateAppInstanceAdmin where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToJSON CreateAppInstanceAdmin where
  toJSON CreateAppInstanceAdmin' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("AppInstanceAdminArn" Core..= appInstanceAdminArn)
          ]
      )

instance Core.ToPath CreateAppInstanceAdmin where
  toPath CreateAppInstanceAdmin' {..} =
    Prelude.mconcat
      [ "/app-instances/",
        Core.toBS appInstanceArn,
        "/admins"
      ]

instance Core.ToQuery CreateAppInstanceAdmin where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateAppInstanceAdminResponse' smart constructor.
data CreateAppInstanceAdminResponse = CreateAppInstanceAdminResponse'
  { -- | The name and ARN of the admin for the @AppInstance@.
    appInstanceAdmin :: Prelude.Maybe Identity,
    -- | The ARN of the of the admin for the @AppInstance@.
    appInstanceArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateAppInstanceAdminResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appInstanceAdmin', 'createAppInstanceAdminResponse_appInstanceAdmin' - The name and ARN of the admin for the @AppInstance@.
--
-- 'appInstanceArn', 'createAppInstanceAdminResponse_appInstanceArn' - The ARN of the of the admin for the @AppInstance@.
--
-- 'httpStatus', 'createAppInstanceAdminResponse_httpStatus' - The response's http status code.
newCreateAppInstanceAdminResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateAppInstanceAdminResponse
newCreateAppInstanceAdminResponse pHttpStatus_ =
  CreateAppInstanceAdminResponse'
    { appInstanceAdmin =
        Prelude.Nothing,
      appInstanceArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The name and ARN of the admin for the @AppInstance@.
createAppInstanceAdminResponse_appInstanceAdmin :: Lens.Lens' CreateAppInstanceAdminResponse (Prelude.Maybe Identity)
createAppInstanceAdminResponse_appInstanceAdmin = Lens.lens (\CreateAppInstanceAdminResponse' {appInstanceAdmin} -> appInstanceAdmin) (\s@CreateAppInstanceAdminResponse' {} a -> s {appInstanceAdmin = a} :: CreateAppInstanceAdminResponse)

-- | The ARN of the of the admin for the @AppInstance@.
createAppInstanceAdminResponse_appInstanceArn :: Lens.Lens' CreateAppInstanceAdminResponse (Prelude.Maybe Prelude.Text)
createAppInstanceAdminResponse_appInstanceArn = Lens.lens (\CreateAppInstanceAdminResponse' {appInstanceArn} -> appInstanceArn) (\s@CreateAppInstanceAdminResponse' {} a -> s {appInstanceArn = a} :: CreateAppInstanceAdminResponse)

-- | The response's http status code.
createAppInstanceAdminResponse_httpStatus :: Lens.Lens' CreateAppInstanceAdminResponse Prelude.Int
createAppInstanceAdminResponse_httpStatus = Lens.lens (\CreateAppInstanceAdminResponse' {httpStatus} -> httpStatus) (\s@CreateAppInstanceAdminResponse' {} a -> s {httpStatus = a} :: CreateAppInstanceAdminResponse)

instance
  Prelude.NFData
    CreateAppInstanceAdminResponse
