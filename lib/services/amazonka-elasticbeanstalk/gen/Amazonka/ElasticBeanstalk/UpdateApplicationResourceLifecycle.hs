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
-- Module      : Amazonka.ElasticBeanstalk.UpdateApplicationResourceLifecycle
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies lifecycle settings for an application.
module Amazonka.ElasticBeanstalk.UpdateApplicationResourceLifecycle
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
    updateApplicationResourceLifecycleResponse_applicationName,
    updateApplicationResourceLifecycleResponse_resourceLifecycleConfig,
    updateApplicationResourceLifecycleResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElasticBeanstalk.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateApplicationResourceLifecycle' smart constructor.
data UpdateApplicationResourceLifecycle = UpdateApplicationResourceLifecycle'
  { -- | The name of the application.
    applicationName :: Prelude.Text,
    -- | The lifecycle configuration.
    resourceLifecycleConfig :: ApplicationResourceLifecycleConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Core.AWSRequest
    UpdateApplicationResourceLifecycle
  where
  type
    AWSResponse UpdateApplicationResourceLifecycle =
      UpdateApplicationResourceLifecycleResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "UpdateApplicationResourceLifecycleResult"
      ( \s h x ->
          UpdateApplicationResourceLifecycleResponse'
            Prelude.<$> (x Data..@? "ApplicationName")
              Prelude.<*> (x Data..@? "ResourceLifecycleConfig")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    UpdateApplicationResourceLifecycle
  where
  hashWithSalt
    _salt
    UpdateApplicationResourceLifecycle' {..} =
      _salt `Prelude.hashWithSalt` applicationName
        `Prelude.hashWithSalt` resourceLifecycleConfig

instance
  Prelude.NFData
    UpdateApplicationResourceLifecycle
  where
  rnf UpdateApplicationResourceLifecycle' {..} =
    Prelude.rnf applicationName
      `Prelude.seq` Prelude.rnf resourceLifecycleConfig

instance
  Data.ToHeaders
    UpdateApplicationResourceLifecycle
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    UpdateApplicationResourceLifecycle
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    UpdateApplicationResourceLifecycle
  where
  toQuery UpdateApplicationResourceLifecycle' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "UpdateApplicationResourceLifecycle" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2010-12-01" :: Prelude.ByteString),
        "ApplicationName" Data.=: applicationName,
        "ResourceLifecycleConfig"
          Data.=: resourceLifecycleConfig
      ]

-- | /See:/ 'newUpdateApplicationResourceLifecycleResponse' smart constructor.
data UpdateApplicationResourceLifecycleResponse = UpdateApplicationResourceLifecycleResponse'
  { -- | The name of the application.
    applicationName :: Prelude.Maybe Prelude.Text,
    -- | The lifecycle configuration.
    resourceLifecycleConfig :: Prelude.Maybe ApplicationResourceLifecycleConfig,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateApplicationResourceLifecycleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationName', 'updateApplicationResourceLifecycleResponse_applicationName' - The name of the application.
--
-- 'resourceLifecycleConfig', 'updateApplicationResourceLifecycleResponse_resourceLifecycleConfig' - The lifecycle configuration.
--
-- 'httpStatus', 'updateApplicationResourceLifecycleResponse_httpStatus' - The response's http status code.
newUpdateApplicationResourceLifecycleResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateApplicationResourceLifecycleResponse
newUpdateApplicationResourceLifecycleResponse
  pHttpStatus_ =
    UpdateApplicationResourceLifecycleResponse'
      { applicationName =
          Prelude.Nothing,
        resourceLifecycleConfig =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The name of the application.
updateApplicationResourceLifecycleResponse_applicationName :: Lens.Lens' UpdateApplicationResourceLifecycleResponse (Prelude.Maybe Prelude.Text)
updateApplicationResourceLifecycleResponse_applicationName = Lens.lens (\UpdateApplicationResourceLifecycleResponse' {applicationName} -> applicationName) (\s@UpdateApplicationResourceLifecycleResponse' {} a -> s {applicationName = a} :: UpdateApplicationResourceLifecycleResponse)

-- | The lifecycle configuration.
updateApplicationResourceLifecycleResponse_resourceLifecycleConfig :: Lens.Lens' UpdateApplicationResourceLifecycleResponse (Prelude.Maybe ApplicationResourceLifecycleConfig)
updateApplicationResourceLifecycleResponse_resourceLifecycleConfig = Lens.lens (\UpdateApplicationResourceLifecycleResponse' {resourceLifecycleConfig} -> resourceLifecycleConfig) (\s@UpdateApplicationResourceLifecycleResponse' {} a -> s {resourceLifecycleConfig = a} :: UpdateApplicationResourceLifecycleResponse)

-- | The response's http status code.
updateApplicationResourceLifecycleResponse_httpStatus :: Lens.Lens' UpdateApplicationResourceLifecycleResponse Prelude.Int
updateApplicationResourceLifecycleResponse_httpStatus = Lens.lens (\UpdateApplicationResourceLifecycleResponse' {httpStatus} -> httpStatus) (\s@UpdateApplicationResourceLifecycleResponse' {} a -> s {httpStatus = a} :: UpdateApplicationResourceLifecycleResponse)

instance
  Prelude.NFData
    UpdateApplicationResourceLifecycleResponse
  where
  rnf UpdateApplicationResourceLifecycleResponse' {..} =
    Prelude.rnf applicationName
      `Prelude.seq` Prelude.rnf resourceLifecycleConfig
      `Prelude.seq` Prelude.rnf httpStatus
