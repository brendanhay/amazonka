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
-- Copyright   : (c) 2013-2022 Brendan Hay
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
    updateDomain_domainSettingsForUpdate,
    updateDomain_defaultUserSettings,
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
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newUpdateDomain' smart constructor.
data UpdateDomain = UpdateDomain'
  { -- | A collection of @DomainSettings@ configuration values to update.
    domainSettingsForUpdate :: Prelude.Maybe DomainSettingsForUpdate,
    -- | A collection of settings.
    defaultUserSettings :: Prelude.Maybe UserSettings,
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
-- 'domainSettingsForUpdate', 'updateDomain_domainSettingsForUpdate' - A collection of @DomainSettings@ configuration values to update.
--
-- 'defaultUserSettings', 'updateDomain_defaultUserSettings' - A collection of settings.
--
-- 'domainId', 'updateDomain_domainId' - The ID of the domain to be updated.
newUpdateDomain ::
  -- | 'domainId'
  Prelude.Text ->
  UpdateDomain
newUpdateDomain pDomainId_ =
  UpdateDomain'
    { domainSettingsForUpdate =
        Prelude.Nothing,
      defaultUserSettings = Prelude.Nothing,
      domainId = pDomainId_
    }

-- | A collection of @DomainSettings@ configuration values to update.
updateDomain_domainSettingsForUpdate :: Lens.Lens' UpdateDomain (Prelude.Maybe DomainSettingsForUpdate)
updateDomain_domainSettingsForUpdate = Lens.lens (\UpdateDomain' {domainSettingsForUpdate} -> domainSettingsForUpdate) (\s@UpdateDomain' {} a -> s {domainSettingsForUpdate = a} :: UpdateDomain)

-- | A collection of settings.
updateDomain_defaultUserSettings :: Lens.Lens' UpdateDomain (Prelude.Maybe UserSettings)
updateDomain_defaultUserSettings = Lens.lens (\UpdateDomain' {defaultUserSettings} -> defaultUserSettings) (\s@UpdateDomain' {} a -> s {defaultUserSettings = a} :: UpdateDomain)

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
            Prelude.<$> (x Core..?> "DomainArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateDomain where
  hashWithSalt _salt UpdateDomain' {..} =
    _salt
      `Prelude.hashWithSalt` domainSettingsForUpdate
      `Prelude.hashWithSalt` defaultUserSettings
      `Prelude.hashWithSalt` domainId

instance Prelude.NFData UpdateDomain where
  rnf UpdateDomain' {..} =
    Prelude.rnf domainSettingsForUpdate
      `Prelude.seq` Prelude.rnf defaultUserSettings
      `Prelude.seq` Prelude.rnf domainId

instance Core.ToHeaders UpdateDomain where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("SageMaker.UpdateDomain" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateDomain where
  toJSON UpdateDomain' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("DomainSettingsForUpdate" Core..=)
              Prelude.<$> domainSettingsForUpdate,
            ("DefaultUserSettings" Core..=)
              Prelude.<$> defaultUserSettings,
            Prelude.Just ("DomainId" Core..= domainId)
          ]
      )

instance Core.ToPath UpdateDomain where
  toPath = Prelude.const "/"

instance Core.ToQuery UpdateDomain where
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
