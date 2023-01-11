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
-- Module      : Amazonka.SageMaker.UpdateDomain
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the default settings for new user profiles in the domain.
module Amazonka.SageMaker.UpdateDomain
  ( -- * Creating a Request
    UpdateDomain (..),
    newUpdateDomain,

    -- * Request Lenses
    updateDomain_appSecurityGroupManagement,
    updateDomain_defaultSpaceSettings,
    updateDomain_defaultUserSettings,
    updateDomain_domainSettingsForUpdate,
    updateDomain_domainId,

    -- * Destructuring the Response
    UpdateDomainResponse (..),
    newUpdateDomainResponse,

    -- * Response Lenses
    updateDomainResponse_domainArn,
    updateDomainResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newUpdateDomain' smart constructor.
data UpdateDomain = UpdateDomain'
  { -- | The entity that creates and manages the required security groups for
    -- inter-app communication in @VPCOnly@ mode. Required when
    -- @CreateDomain.AppNetworkAccessType@ is @VPCOnly@ and
    -- @DomainSettings.RStudioServerProDomainSettings.DomainExecutionRoleArn@
    -- is provided.
    appSecurityGroupManagement :: Prelude.Maybe AppSecurityGroupManagement,
    -- | The default settings used to create a space within the Domain.
    defaultSpaceSettings :: Prelude.Maybe DefaultSpaceSettings,
    -- | A collection of settings.
    defaultUserSettings :: Prelude.Maybe UserSettings,
    -- | A collection of @DomainSettings@ configuration values to update.
    domainSettingsForUpdate :: Prelude.Maybe DomainSettingsForUpdate,
    -- | The ID of the domain to be updated.
    domainId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateDomain' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appSecurityGroupManagement', 'updateDomain_appSecurityGroupManagement' - The entity that creates and manages the required security groups for
-- inter-app communication in @VPCOnly@ mode. Required when
-- @CreateDomain.AppNetworkAccessType@ is @VPCOnly@ and
-- @DomainSettings.RStudioServerProDomainSettings.DomainExecutionRoleArn@
-- is provided.
--
-- 'defaultSpaceSettings', 'updateDomain_defaultSpaceSettings' - The default settings used to create a space within the Domain.
--
-- 'defaultUserSettings', 'updateDomain_defaultUserSettings' - A collection of settings.
--
-- 'domainSettingsForUpdate', 'updateDomain_domainSettingsForUpdate' - A collection of @DomainSettings@ configuration values to update.
--
-- 'domainId', 'updateDomain_domainId' - The ID of the domain to be updated.
newUpdateDomain ::
  -- | 'domainId'
  Prelude.Text ->
  UpdateDomain
newUpdateDomain pDomainId_ =
  UpdateDomain'
    { appSecurityGroupManagement =
        Prelude.Nothing,
      defaultSpaceSettings = Prelude.Nothing,
      defaultUserSettings = Prelude.Nothing,
      domainSettingsForUpdate = Prelude.Nothing,
      domainId = pDomainId_
    }

-- | The entity that creates and manages the required security groups for
-- inter-app communication in @VPCOnly@ mode. Required when
-- @CreateDomain.AppNetworkAccessType@ is @VPCOnly@ and
-- @DomainSettings.RStudioServerProDomainSettings.DomainExecutionRoleArn@
-- is provided.
updateDomain_appSecurityGroupManagement :: Lens.Lens' UpdateDomain (Prelude.Maybe AppSecurityGroupManagement)
updateDomain_appSecurityGroupManagement = Lens.lens (\UpdateDomain' {appSecurityGroupManagement} -> appSecurityGroupManagement) (\s@UpdateDomain' {} a -> s {appSecurityGroupManagement = a} :: UpdateDomain)

-- | The default settings used to create a space within the Domain.
updateDomain_defaultSpaceSettings :: Lens.Lens' UpdateDomain (Prelude.Maybe DefaultSpaceSettings)
updateDomain_defaultSpaceSettings = Lens.lens (\UpdateDomain' {defaultSpaceSettings} -> defaultSpaceSettings) (\s@UpdateDomain' {} a -> s {defaultSpaceSettings = a} :: UpdateDomain)

-- | A collection of settings.
updateDomain_defaultUserSettings :: Lens.Lens' UpdateDomain (Prelude.Maybe UserSettings)
updateDomain_defaultUserSettings = Lens.lens (\UpdateDomain' {defaultUserSettings} -> defaultUserSettings) (\s@UpdateDomain' {} a -> s {defaultUserSettings = a} :: UpdateDomain)

-- | A collection of @DomainSettings@ configuration values to update.
updateDomain_domainSettingsForUpdate :: Lens.Lens' UpdateDomain (Prelude.Maybe DomainSettingsForUpdate)
updateDomain_domainSettingsForUpdate = Lens.lens (\UpdateDomain' {domainSettingsForUpdate} -> domainSettingsForUpdate) (\s@UpdateDomain' {} a -> s {domainSettingsForUpdate = a} :: UpdateDomain)

-- | The ID of the domain to be updated.
updateDomain_domainId :: Lens.Lens' UpdateDomain Prelude.Text
updateDomain_domainId = Lens.lens (\UpdateDomain' {domainId} -> domainId) (\s@UpdateDomain' {} a -> s {domainId = a} :: UpdateDomain)

instance Core.AWSRequest UpdateDomain where
  type AWSResponse UpdateDomain = UpdateDomainResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateDomainResponse'
            Prelude.<$> (x Data..?> "DomainArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateDomain where
  hashWithSalt _salt UpdateDomain' {..} =
    _salt
      `Prelude.hashWithSalt` appSecurityGroupManagement
      `Prelude.hashWithSalt` defaultSpaceSettings
      `Prelude.hashWithSalt` defaultUserSettings
      `Prelude.hashWithSalt` domainSettingsForUpdate
      `Prelude.hashWithSalt` domainId

instance Prelude.NFData UpdateDomain where
  rnf UpdateDomain' {..} =
    Prelude.rnf appSecurityGroupManagement
      `Prelude.seq` Prelude.rnf defaultSpaceSettings
      `Prelude.seq` Prelude.rnf defaultUserSettings
      `Prelude.seq` Prelude.rnf domainSettingsForUpdate
      `Prelude.seq` Prelude.rnf domainId

instance Data.ToHeaders UpdateDomain where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("SageMaker.UpdateDomain" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateDomain where
  toJSON UpdateDomain' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AppSecurityGroupManagement" Data..=)
              Prelude.<$> appSecurityGroupManagement,
            ("DefaultSpaceSettings" Data..=)
              Prelude.<$> defaultSpaceSettings,
            ("DefaultUserSettings" Data..=)
              Prelude.<$> defaultUserSettings,
            ("DomainSettingsForUpdate" Data..=)
              Prelude.<$> domainSettingsForUpdate,
            Prelude.Just ("DomainId" Data..= domainId)
          ]
      )

instance Data.ToPath UpdateDomain where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateDomain where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateDomainResponse' smart constructor.
data UpdateDomainResponse = UpdateDomainResponse'
  { -- | The Amazon Resource Name (ARN) of the domain.
    domainArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateDomainResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainArn', 'updateDomainResponse_domainArn' - The Amazon Resource Name (ARN) of the domain.
--
-- 'httpStatus', 'updateDomainResponse_httpStatus' - The response's http status code.
newUpdateDomainResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateDomainResponse
newUpdateDomainResponse pHttpStatus_ =
  UpdateDomainResponse'
    { domainArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the domain.
updateDomainResponse_domainArn :: Lens.Lens' UpdateDomainResponse (Prelude.Maybe Prelude.Text)
updateDomainResponse_domainArn = Lens.lens (\UpdateDomainResponse' {domainArn} -> domainArn) (\s@UpdateDomainResponse' {} a -> s {domainArn = a} :: UpdateDomainResponse)

-- | The response's http status code.
updateDomainResponse_httpStatus :: Lens.Lens' UpdateDomainResponse Prelude.Int
updateDomainResponse_httpStatus = Lens.lens (\UpdateDomainResponse' {httpStatus} -> httpStatus) (\s@UpdateDomainResponse' {} a -> s {httpStatus = a} :: UpdateDomainResponse)

instance Prelude.NFData UpdateDomainResponse where
  rnf UpdateDomainResponse' {..} =
    Prelude.rnf domainArn
      `Prelude.seq` Prelude.rnf httpStatus
