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
-- Module      : Amazonka.Proton.DeleteTemplateSyncConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete a template sync configuration.
module Amazonka.Proton.DeleteTemplateSyncConfig
  ( -- * Creating a Request
    DeleteTemplateSyncConfig (..),
    newDeleteTemplateSyncConfig,

    -- * Request Lenses
    deleteTemplateSyncConfig_templateName,
    deleteTemplateSyncConfig_templateType,

    -- * Destructuring the Response
    DeleteTemplateSyncConfigResponse (..),
    newDeleteTemplateSyncConfigResponse,

    -- * Response Lenses
    deleteTemplateSyncConfigResponse_templateSyncConfig,
    deleteTemplateSyncConfigResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Proton.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteTemplateSyncConfig' smart constructor.
data DeleteTemplateSyncConfig = DeleteTemplateSyncConfig'
  { -- | The template name.
    templateName :: Prelude.Text,
    -- | The template type.
    templateType :: TemplateType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteTemplateSyncConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'templateName', 'deleteTemplateSyncConfig_templateName' - The template name.
--
-- 'templateType', 'deleteTemplateSyncConfig_templateType' - The template type.
newDeleteTemplateSyncConfig ::
  -- | 'templateName'
  Prelude.Text ->
  -- | 'templateType'
  TemplateType ->
  DeleteTemplateSyncConfig
newDeleteTemplateSyncConfig
  pTemplateName_
  pTemplateType_ =
    DeleteTemplateSyncConfig'
      { templateName =
          pTemplateName_,
        templateType = pTemplateType_
      }

-- | The template name.
deleteTemplateSyncConfig_templateName :: Lens.Lens' DeleteTemplateSyncConfig Prelude.Text
deleteTemplateSyncConfig_templateName = Lens.lens (\DeleteTemplateSyncConfig' {templateName} -> templateName) (\s@DeleteTemplateSyncConfig' {} a -> s {templateName = a} :: DeleteTemplateSyncConfig)

-- | The template type.
deleteTemplateSyncConfig_templateType :: Lens.Lens' DeleteTemplateSyncConfig TemplateType
deleteTemplateSyncConfig_templateType = Lens.lens (\DeleteTemplateSyncConfig' {templateType} -> templateType) (\s@DeleteTemplateSyncConfig' {} a -> s {templateType = a} :: DeleteTemplateSyncConfig)

instance Core.AWSRequest DeleteTemplateSyncConfig where
  type
    AWSResponse DeleteTemplateSyncConfig =
      DeleteTemplateSyncConfigResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteTemplateSyncConfigResponse'
            Prelude.<$> (x Data..?> "templateSyncConfig")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteTemplateSyncConfig where
  hashWithSalt _salt DeleteTemplateSyncConfig' {..} =
    _salt
      `Prelude.hashWithSalt` templateName
      `Prelude.hashWithSalt` templateType

instance Prelude.NFData DeleteTemplateSyncConfig where
  rnf DeleteTemplateSyncConfig' {..} =
    Prelude.rnf templateName `Prelude.seq`
      Prelude.rnf templateType

instance Data.ToHeaders DeleteTemplateSyncConfig where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AwsProton20200720.DeleteTemplateSyncConfig" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteTemplateSyncConfig where
  toJSON DeleteTemplateSyncConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("templateName" Data..= templateName),
            Prelude.Just ("templateType" Data..= templateType)
          ]
      )

instance Data.ToPath DeleteTemplateSyncConfig where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteTemplateSyncConfig where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteTemplateSyncConfigResponse' smart constructor.
data DeleteTemplateSyncConfigResponse = DeleteTemplateSyncConfigResponse'
  { -- | The template sync configuration detail data that\'s returned by Proton.
    templateSyncConfig :: Prelude.Maybe TemplateSyncConfig,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteTemplateSyncConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'templateSyncConfig', 'deleteTemplateSyncConfigResponse_templateSyncConfig' - The template sync configuration detail data that\'s returned by Proton.
--
-- 'httpStatus', 'deleteTemplateSyncConfigResponse_httpStatus' - The response's http status code.
newDeleteTemplateSyncConfigResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteTemplateSyncConfigResponse
newDeleteTemplateSyncConfigResponse pHttpStatus_ =
  DeleteTemplateSyncConfigResponse'
    { templateSyncConfig =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The template sync configuration detail data that\'s returned by Proton.
deleteTemplateSyncConfigResponse_templateSyncConfig :: Lens.Lens' DeleteTemplateSyncConfigResponse (Prelude.Maybe TemplateSyncConfig)
deleteTemplateSyncConfigResponse_templateSyncConfig = Lens.lens (\DeleteTemplateSyncConfigResponse' {templateSyncConfig} -> templateSyncConfig) (\s@DeleteTemplateSyncConfigResponse' {} a -> s {templateSyncConfig = a} :: DeleteTemplateSyncConfigResponse)

-- | The response's http status code.
deleteTemplateSyncConfigResponse_httpStatus :: Lens.Lens' DeleteTemplateSyncConfigResponse Prelude.Int
deleteTemplateSyncConfigResponse_httpStatus = Lens.lens (\DeleteTemplateSyncConfigResponse' {httpStatus} -> httpStatus) (\s@DeleteTemplateSyncConfigResponse' {} a -> s {httpStatus = a} :: DeleteTemplateSyncConfigResponse)

instance
  Prelude.NFData
    DeleteTemplateSyncConfigResponse
  where
  rnf DeleteTemplateSyncConfigResponse' {..} =
    Prelude.rnf templateSyncConfig `Prelude.seq`
      Prelude.rnf httpStatus
