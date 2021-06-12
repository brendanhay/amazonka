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
-- Module      : Network.AWS.SageMaker.UpdateDomain
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the default settings for new user profiles in the domain.
module Network.AWS.SageMaker.UpdateDomain
  ( -- * Creating a Request
    UpdateDomain (..),
    newUpdateDomain,

    -- * Request Lenses
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newUpdateDomain' smart constructor.
data UpdateDomain = UpdateDomain'
  { -- | A collection of settings.
    defaultUserSettings :: Core.Maybe UserSettings,
    -- | The ID of the domain to be updated.
    domainId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateDomain' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'defaultUserSettings', 'updateDomain_defaultUserSettings' - A collection of settings.
--
-- 'domainId', 'updateDomain_domainId' - The ID of the domain to be updated.
newUpdateDomain ::
  -- | 'domainId'
  Core.Text ->
  UpdateDomain
newUpdateDomain pDomainId_ =
  UpdateDomain'
    { defaultUserSettings = Core.Nothing,
      domainId = pDomainId_
    }

-- | A collection of settings.
updateDomain_defaultUserSettings :: Lens.Lens' UpdateDomain (Core.Maybe UserSettings)
updateDomain_defaultUserSettings = Lens.lens (\UpdateDomain' {defaultUserSettings} -> defaultUserSettings) (\s@UpdateDomain' {} a -> s {defaultUserSettings = a} :: UpdateDomain)

-- | The ID of the domain to be updated.
updateDomain_domainId :: Lens.Lens' UpdateDomain Core.Text
updateDomain_domainId = Lens.lens (\UpdateDomain' {domainId} -> domainId) (\s@UpdateDomain' {} a -> s {domainId = a} :: UpdateDomain)

instance Core.AWSRequest UpdateDomain where
  type AWSResponse UpdateDomain = UpdateDomainResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateDomainResponse'
            Core.<$> (x Core..?> "DomainArn")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateDomain

instance Core.NFData UpdateDomain

instance Core.ToHeaders UpdateDomain where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("SageMaker.UpdateDomain" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateDomain where
  toJSON UpdateDomain' {..} =
    Core.object
      ( Core.catMaybes
          [ ("DefaultUserSettings" Core..=)
              Core.<$> defaultUserSettings,
            Core.Just ("DomainId" Core..= domainId)
          ]
      )

instance Core.ToPath UpdateDomain where
  toPath = Core.const "/"

instance Core.ToQuery UpdateDomain where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateDomainResponse' smart constructor.
data UpdateDomainResponse = UpdateDomainResponse'
  { -- | The Amazon Resource Name (ARN) of the domain.
    domainArn :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  UpdateDomainResponse
newUpdateDomainResponse pHttpStatus_ =
  UpdateDomainResponse'
    { domainArn = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the domain.
updateDomainResponse_domainArn :: Lens.Lens' UpdateDomainResponse (Core.Maybe Core.Text)
updateDomainResponse_domainArn = Lens.lens (\UpdateDomainResponse' {domainArn} -> domainArn) (\s@UpdateDomainResponse' {} a -> s {domainArn = a} :: UpdateDomainResponse)

-- | The response's http status code.
updateDomainResponse_httpStatus :: Lens.Lens' UpdateDomainResponse Core.Int
updateDomainResponse_httpStatus = Lens.lens (\UpdateDomainResponse' {httpStatus} -> httpStatus) (\s@UpdateDomainResponse' {} a -> s {httpStatus = a} :: UpdateDomainResponse)

instance Core.NFData UpdateDomainResponse
