{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.ElasticBeanstalk.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateApplicationResourceLifecycle' smart constructor.
data UpdateApplicationResourceLifecycle = UpdateApplicationResourceLifecycle'
  { -- | The name of the application.
    applicationName :: Prelude.Text,
    -- | The lifecycle configuration.
    resourceLifecycleConfig :: ApplicationResourceLifecycleConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
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
updateApplicationResourceLifecycle_applicationName :: Lens.Lens' UpdateApplicationResourceLifecycle Prelude.Text
updateApplicationResourceLifecycle_applicationName = Lens.lens (\UpdateApplicationResourceLifecycle' {applicationName} -> applicationName) (\s@UpdateApplicationResourceLifecycle' {} a -> s {applicationName = a} :: UpdateApplicationResourceLifecycle)

-- | The lifecycle configuration.
updateApplicationResourceLifecycle_resourceLifecycleConfig :: Lens.Lens' UpdateApplicationResourceLifecycle ApplicationResourceLifecycleConfig
updateApplicationResourceLifecycle_resourceLifecycleConfig = Lens.lens (\UpdateApplicationResourceLifecycle' {resourceLifecycleConfig} -> resourceLifecycleConfig) (\s@UpdateApplicationResourceLifecycle' {} a -> s {resourceLifecycleConfig = a} :: UpdateApplicationResourceLifecycle)

instance
  Prelude.AWSRequest
    UpdateApplicationResourceLifecycle
  where
  type
    Rs UpdateApplicationResourceLifecycle =
      UpdateApplicationResourceLifecycleResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "UpdateApplicationResourceLifecycleResult"
      ( \s h x ->
          UpdateApplicationResourceLifecycleResponse'
            Prelude.<$> (x Prelude..@? "ResourceLifecycleConfig")
              Prelude.<*> (x Prelude..@? "ApplicationName")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    UpdateApplicationResourceLifecycle

instance
  Prelude.NFData
    UpdateApplicationResourceLifecycle

instance
  Prelude.ToHeaders
    UpdateApplicationResourceLifecycle
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Prelude.ToPath
    UpdateApplicationResourceLifecycle
  where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
    UpdateApplicationResourceLifecycle
  where
  toQuery UpdateApplicationResourceLifecycle' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ( "UpdateApplicationResourceLifecycle" ::
                         Prelude.ByteString
                     ),
        "Version"
          Prelude.=: ("2010-12-01" :: Prelude.ByteString),
        "ApplicationName" Prelude.=: applicationName,
        "ResourceLifecycleConfig"
          Prelude.=: resourceLifecycleConfig
      ]

-- | /See:/ 'newUpdateApplicationResourceLifecycleResponse' smart constructor.
data UpdateApplicationResourceLifecycleResponse = UpdateApplicationResourceLifecycleResponse'
  { -- | The lifecycle configuration.
    resourceLifecycleConfig :: Prelude.Maybe ApplicationResourceLifecycleConfig,
    -- | The name of the application.
    applicationName :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  UpdateApplicationResourceLifecycleResponse
newUpdateApplicationResourceLifecycleResponse
  pHttpStatus_ =
    UpdateApplicationResourceLifecycleResponse'
      { resourceLifecycleConfig =
          Prelude.Nothing,
        applicationName =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The lifecycle configuration.
updateApplicationResourceLifecycleResponse_resourceLifecycleConfig :: Lens.Lens' UpdateApplicationResourceLifecycleResponse (Prelude.Maybe ApplicationResourceLifecycleConfig)
updateApplicationResourceLifecycleResponse_resourceLifecycleConfig = Lens.lens (\UpdateApplicationResourceLifecycleResponse' {resourceLifecycleConfig} -> resourceLifecycleConfig) (\s@UpdateApplicationResourceLifecycleResponse' {} a -> s {resourceLifecycleConfig = a} :: UpdateApplicationResourceLifecycleResponse)

-- | The name of the application.
updateApplicationResourceLifecycleResponse_applicationName :: Lens.Lens' UpdateApplicationResourceLifecycleResponse (Prelude.Maybe Prelude.Text)
updateApplicationResourceLifecycleResponse_applicationName = Lens.lens (\UpdateApplicationResourceLifecycleResponse' {applicationName} -> applicationName) (\s@UpdateApplicationResourceLifecycleResponse' {} a -> s {applicationName = a} :: UpdateApplicationResourceLifecycleResponse)

-- | The response's http status code.
updateApplicationResourceLifecycleResponse_httpStatus :: Lens.Lens' UpdateApplicationResourceLifecycleResponse Prelude.Int
updateApplicationResourceLifecycleResponse_httpStatus = Lens.lens (\UpdateApplicationResourceLifecycleResponse' {httpStatus} -> httpStatus) (\s@UpdateApplicationResourceLifecycleResponse' {} a -> s {httpStatus = a} :: UpdateApplicationResourceLifecycleResponse)

instance
  Prelude.NFData
    UpdateApplicationResourceLifecycleResponse
