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
-- Module      : Network.AWS.Discovery.UpdateApplication
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates metadata about an application.
module Network.AWS.Discovery.UpdateApplication
  ( -- * Creating a Request
    UpdateApplication (..),
    newUpdateApplication,

    -- * Request Lenses
    updateApplication_name,
    updateApplication_description,
    updateApplication_configurationId,

    -- * Destructuring the Response
    UpdateApplicationResponse (..),
    newUpdateApplicationResponse,

    -- * Response Lenses
    updateApplicationResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Discovery.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateApplication' smart constructor.
data UpdateApplication = UpdateApplication'
  { -- | New name of the application to be updated.
    name :: Core.Maybe Core.Text,
    -- | New description of the application to be updated.
    description :: Core.Maybe Core.Text,
    -- | Configuration ID of the application to be updated.
    configurationId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateApplication' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'updateApplication_name' - New name of the application to be updated.
--
-- 'description', 'updateApplication_description' - New description of the application to be updated.
--
-- 'configurationId', 'updateApplication_configurationId' - Configuration ID of the application to be updated.
newUpdateApplication ::
  -- | 'configurationId'
  Core.Text ->
  UpdateApplication
newUpdateApplication pConfigurationId_ =
  UpdateApplication'
    { name = Core.Nothing,
      description = Core.Nothing,
      configurationId = pConfigurationId_
    }

-- | New name of the application to be updated.
updateApplication_name :: Lens.Lens' UpdateApplication (Core.Maybe Core.Text)
updateApplication_name = Lens.lens (\UpdateApplication' {name} -> name) (\s@UpdateApplication' {} a -> s {name = a} :: UpdateApplication)

-- | New description of the application to be updated.
updateApplication_description :: Lens.Lens' UpdateApplication (Core.Maybe Core.Text)
updateApplication_description = Lens.lens (\UpdateApplication' {description} -> description) (\s@UpdateApplication' {} a -> s {description = a} :: UpdateApplication)

-- | Configuration ID of the application to be updated.
updateApplication_configurationId :: Lens.Lens' UpdateApplication Core.Text
updateApplication_configurationId = Lens.lens (\UpdateApplication' {configurationId} -> configurationId) (\s@UpdateApplication' {} a -> s {configurationId = a} :: UpdateApplication)

instance Core.AWSRequest UpdateApplication where
  type
    AWSResponse UpdateApplication =
      UpdateApplicationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateApplicationResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateApplication

instance Core.NFData UpdateApplication

instance Core.ToHeaders UpdateApplication where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSPoseidonService_V2015_11_01.UpdateApplication" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateApplication where
  toJSON UpdateApplication' {..} =
    Core.object
      ( Core.catMaybes
          [ ("name" Core..=) Core.<$> name,
            ("description" Core..=) Core.<$> description,
            Core.Just
              ("configurationId" Core..= configurationId)
          ]
      )

instance Core.ToPath UpdateApplication where
  toPath = Core.const "/"

instance Core.ToQuery UpdateApplication where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateApplicationResponse' smart constructor.
data UpdateApplicationResponse = UpdateApplicationResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateApplicationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateApplicationResponse_httpStatus' - The response's http status code.
newUpdateApplicationResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdateApplicationResponse
newUpdateApplicationResponse pHttpStatus_ =
  UpdateApplicationResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateApplicationResponse_httpStatus :: Lens.Lens' UpdateApplicationResponse Core.Int
updateApplicationResponse_httpStatus = Lens.lens (\UpdateApplicationResponse' {httpStatus} -> httpStatus) (\s@UpdateApplicationResponse' {} a -> s {httpStatus = a} :: UpdateApplicationResponse)

instance Core.NFData UpdateApplicationResponse
