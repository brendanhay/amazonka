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
-- Module      : Amazonka.Proton.GetTemplateSyncConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get detail data for a template sync configuration.
module Amazonka.Proton.GetTemplateSyncConfig
  ( -- * Creating a Request
    GetTemplateSyncConfig (..),
    newGetTemplateSyncConfig,

    -- * Request Lenses
    getTemplateSyncConfig_templateName,
    getTemplateSyncConfig_templateType,

    -- * Destructuring the Response
    GetTemplateSyncConfigResponse (..),
    newGetTemplateSyncConfigResponse,

    -- * Response Lenses
    getTemplateSyncConfigResponse_templateSyncConfig,
    getTemplateSyncConfigResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Proton.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetTemplateSyncConfig' smart constructor.
data GetTemplateSyncConfig = GetTemplateSyncConfig'
  { -- | The template name.
    templateName :: Prelude.Text,
    -- | The template type.
    templateType :: TemplateType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetTemplateSyncConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'templateName', 'getTemplateSyncConfig_templateName' - The template name.
--
-- 'templateType', 'getTemplateSyncConfig_templateType' - The template type.
newGetTemplateSyncConfig ::
  -- | 'templateName'
  Prelude.Text ->
  -- | 'templateType'
  TemplateType ->
  GetTemplateSyncConfig
newGetTemplateSyncConfig
  pTemplateName_
  pTemplateType_ =
    GetTemplateSyncConfig'
      { templateName =
          pTemplateName_,
        templateType = pTemplateType_
      }

-- | The template name.
getTemplateSyncConfig_templateName :: Lens.Lens' GetTemplateSyncConfig Prelude.Text
getTemplateSyncConfig_templateName = Lens.lens (\GetTemplateSyncConfig' {templateName} -> templateName) (\s@GetTemplateSyncConfig' {} a -> s {templateName = a} :: GetTemplateSyncConfig)

-- | The template type.
getTemplateSyncConfig_templateType :: Lens.Lens' GetTemplateSyncConfig TemplateType
getTemplateSyncConfig_templateType = Lens.lens (\GetTemplateSyncConfig' {templateType} -> templateType) (\s@GetTemplateSyncConfig' {} a -> s {templateType = a} :: GetTemplateSyncConfig)

instance Core.AWSRequest GetTemplateSyncConfig where
  type
    AWSResponse GetTemplateSyncConfig =
      GetTemplateSyncConfigResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetTemplateSyncConfigResponse'
            Prelude.<$> (x Data..?> "templateSyncConfig")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetTemplateSyncConfig where
  hashWithSalt _salt GetTemplateSyncConfig' {..} =
    _salt
      `Prelude.hashWithSalt` templateName
      `Prelude.hashWithSalt` templateType

instance Prelude.NFData GetTemplateSyncConfig where
  rnf GetTemplateSyncConfig' {..} =
    Prelude.rnf templateName
      `Prelude.seq` Prelude.rnf templateType

instance Data.ToHeaders GetTemplateSyncConfig where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AwsProton20200720.GetTemplateSyncConfig" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetTemplateSyncConfig where
  toJSON GetTemplateSyncConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("templateName" Data..= templateName),
            Prelude.Just ("templateType" Data..= templateType)
          ]
      )

instance Data.ToPath GetTemplateSyncConfig where
  toPath = Prelude.const "/"

instance Data.ToQuery GetTemplateSyncConfig where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetTemplateSyncConfigResponse' smart constructor.
data GetTemplateSyncConfigResponse = GetTemplateSyncConfigResponse'
  { -- | The template sync configuration detail data that\'s returned by Proton.
    templateSyncConfig :: Prelude.Maybe TemplateSyncConfig,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetTemplateSyncConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'templateSyncConfig', 'getTemplateSyncConfigResponse_templateSyncConfig' - The template sync configuration detail data that\'s returned by Proton.
--
-- 'httpStatus', 'getTemplateSyncConfigResponse_httpStatus' - The response's http status code.
newGetTemplateSyncConfigResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetTemplateSyncConfigResponse
newGetTemplateSyncConfigResponse pHttpStatus_ =
  GetTemplateSyncConfigResponse'
    { templateSyncConfig =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The template sync configuration detail data that\'s returned by Proton.
getTemplateSyncConfigResponse_templateSyncConfig :: Lens.Lens' GetTemplateSyncConfigResponse (Prelude.Maybe TemplateSyncConfig)
getTemplateSyncConfigResponse_templateSyncConfig = Lens.lens (\GetTemplateSyncConfigResponse' {templateSyncConfig} -> templateSyncConfig) (\s@GetTemplateSyncConfigResponse' {} a -> s {templateSyncConfig = a} :: GetTemplateSyncConfigResponse)

-- | The response's http status code.
getTemplateSyncConfigResponse_httpStatus :: Lens.Lens' GetTemplateSyncConfigResponse Prelude.Int
getTemplateSyncConfigResponse_httpStatus = Lens.lens (\GetTemplateSyncConfigResponse' {httpStatus} -> httpStatus) (\s@GetTemplateSyncConfigResponse' {} a -> s {httpStatus = a} :: GetTemplateSyncConfigResponse)

instance Prelude.NFData GetTemplateSyncConfigResponse where
  rnf GetTemplateSyncConfigResponse' {..} =
    Prelude.rnf templateSyncConfig
      `Prelude.seq` Prelude.rnf httpStatus
