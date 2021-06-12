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
-- Module      : Network.AWS.ElasticBeanstalk.UpdateApplicationResourceLifecycle
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies lifecycle settings for an application.
module Network.AWS.ElasticBeanstalk.UpdateApplicationResourceLifecycle
  ( -- * Creating a Request
    UpdateApplicationResourceLifecycle (..),
    newUpdateApplicationResourceLifecycle,

    -- * Request Lenses
    updateApplicationResourceLifecycle_applicationName,
    updateApplicationResourceLifecycle_resourceLifecycleConfig,

    -- * Destructuring the Response
    UpdateApplicationResourceLifecycleResponse (..),
    newUpdateApplicationResourceLifecycleResponse,

    -- * Response Lenses
    updateApplicationResourceLifecycleResponse_resourceLifecycleConfig,
    updateApplicationResourceLifecycleResponse_applicationName,
    updateApplicationResourceLifecycleResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ElasticBeanstalk.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateApplicationResourceLifecycle' smart constructor.
data UpdateApplicationResourceLifecycle = UpdateApplicationResourceLifecycle'
  { -- | The name of the application.
    applicationName :: Core.Text,
    -- | The lifecycle configuration.
    resourceLifecycleConfig :: ApplicationResourceLifecycleConfig
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateApplicationResourceLifecycle' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationName', 'updateApplicationResourceLifecycle_applicationName' - The name of the application.
--
-- 'resourceLifecycleConfig', 'updateApplicationResourceLifecycle_resourceLifecycleConfig' - The lifecycle configuration.
newUpdateApplicationResourceLifecycle ::
  -- | 'applicationName'
  Core.Text ->
  -- | 'resourceLifecycleConfig'
  ApplicationResourceLifecycleConfig ->
  UpdateApplicationResourceLifecycle
newUpdateApplicationResourceLifecycle
  pApplicationName_
  pResourceLifecycleConfig_ =
    UpdateApplicationResourceLifecycle'
      { applicationName =
          pApplicationName_,
        resourceLifecycleConfig =
          pResourceLifecycleConfig_
      }

-- | The name of the application.
updateApplicationResourceLifecycle_applicationName :: Lens.Lens' UpdateApplicationResourceLifecycle Core.Text
updateApplicationResourceLifecycle_applicationName = Lens.lens (\UpdateApplicationResourceLifecycle' {applicationName} -> applicationName) (\s@UpdateApplicationResourceLifecycle' {} a -> s {applicationName = a} :: UpdateApplicationResourceLifecycle)

-- | The lifecycle configuration.
updateApplicationResourceLifecycle_resourceLifecycleConfig :: Lens.Lens' UpdateApplicationResourceLifecycle ApplicationResourceLifecycleConfig
updateApplicationResourceLifecycle_resourceLifecycleConfig = Lens.lens (\UpdateApplicationResourceLifecycle' {resourceLifecycleConfig} -> resourceLifecycleConfig) (\s@UpdateApplicationResourceLifecycle' {} a -> s {resourceLifecycleConfig = a} :: UpdateApplicationResourceLifecycle)

instance
  Core.AWSRequest
    UpdateApplicationResourceLifecycle
  where
  type
    AWSResponse UpdateApplicationResourceLifecycle =
      UpdateApplicationResourceLifecycleResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "UpdateApplicationResourceLifecycleResult"
      ( \s h x ->
          UpdateApplicationResourceLifecycleResponse'
            Core.<$> (x Core..@? "ResourceLifecycleConfig")
            Core.<*> (x Core..@? "ApplicationName")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    UpdateApplicationResourceLifecycle

instance
  Core.NFData
    UpdateApplicationResourceLifecycle

instance
  Core.ToHeaders
    UpdateApplicationResourceLifecycle
  where
  toHeaders = Core.const Core.mempty

instance
  Core.ToPath
    UpdateApplicationResourceLifecycle
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    UpdateApplicationResourceLifecycle
  where
  toQuery UpdateApplicationResourceLifecycle' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ( "UpdateApplicationResourceLifecycle" ::
                      Core.ByteString
                  ),
        "Version" Core.=: ("2010-12-01" :: Core.ByteString),
        "ApplicationName" Core.=: applicationName,
        "ResourceLifecycleConfig"
          Core.=: resourceLifecycleConfig
      ]

-- | /See:/ 'newUpdateApplicationResourceLifecycleResponse' smart constructor.
data UpdateApplicationResourceLifecycleResponse = UpdateApplicationResourceLifecycleResponse'
  { -- | The lifecycle configuration.
    resourceLifecycleConfig :: Core.Maybe ApplicationResourceLifecycleConfig,
    -- | The name of the application.
    applicationName :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateApplicationResourceLifecycleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceLifecycleConfig', 'updateApplicationResourceLifecycleResponse_resourceLifecycleConfig' - The lifecycle configuration.
--
-- 'applicationName', 'updateApplicationResourceLifecycleResponse_applicationName' - The name of the application.
--
-- 'httpStatus', 'updateApplicationResourceLifecycleResponse_httpStatus' - The response's http status code.
newUpdateApplicationResourceLifecycleResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdateApplicationResourceLifecycleResponse
newUpdateApplicationResourceLifecycleResponse
  pHttpStatus_ =
    UpdateApplicationResourceLifecycleResponse'
      { resourceLifecycleConfig =
          Core.Nothing,
        applicationName = Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The lifecycle configuration.
updateApplicationResourceLifecycleResponse_resourceLifecycleConfig :: Lens.Lens' UpdateApplicationResourceLifecycleResponse (Core.Maybe ApplicationResourceLifecycleConfig)
updateApplicationResourceLifecycleResponse_resourceLifecycleConfig = Lens.lens (\UpdateApplicationResourceLifecycleResponse' {resourceLifecycleConfig} -> resourceLifecycleConfig) (\s@UpdateApplicationResourceLifecycleResponse' {} a -> s {resourceLifecycleConfig = a} :: UpdateApplicationResourceLifecycleResponse)

-- | The name of the application.
updateApplicationResourceLifecycleResponse_applicationName :: Lens.Lens' UpdateApplicationResourceLifecycleResponse (Core.Maybe Core.Text)
updateApplicationResourceLifecycleResponse_applicationName = Lens.lens (\UpdateApplicationResourceLifecycleResponse' {applicationName} -> applicationName) (\s@UpdateApplicationResourceLifecycleResponse' {} a -> s {applicationName = a} :: UpdateApplicationResourceLifecycleResponse)

-- | The response's http status code.
updateApplicationResourceLifecycleResponse_httpStatus :: Lens.Lens' UpdateApplicationResourceLifecycleResponse Core.Int
updateApplicationResourceLifecycleResponse_httpStatus = Lens.lens (\UpdateApplicationResourceLifecycleResponse' {httpStatus} -> httpStatus) (\s@UpdateApplicationResourceLifecycleResponse' {} a -> s {httpStatus = a} :: UpdateApplicationResourceLifecycleResponse)

instance
  Core.NFData
    UpdateApplicationResourceLifecycleResponse
