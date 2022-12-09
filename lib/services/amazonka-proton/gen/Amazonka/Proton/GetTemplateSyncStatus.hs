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
-- Module      : Amazonka.Proton.GetTemplateSyncStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get the status of a template sync.
module Amazonka.Proton.GetTemplateSyncStatus
  ( -- * Creating a Request
    GetTemplateSyncStatus (..),
    newGetTemplateSyncStatus,

    -- * Request Lenses
    getTemplateSyncStatus_templateName,
    getTemplateSyncStatus_templateType,
    getTemplateSyncStatus_templateVersion,

    -- * Destructuring the Response
    GetTemplateSyncStatusResponse (..),
    newGetTemplateSyncStatusResponse,

    -- * Response Lenses
    getTemplateSyncStatusResponse_desiredState,
    getTemplateSyncStatusResponse_latestSuccessfulSync,
    getTemplateSyncStatusResponse_latestSync,
    getTemplateSyncStatusResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Proton.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetTemplateSyncStatus' smart constructor.
data GetTemplateSyncStatus = GetTemplateSyncStatus'
  { -- | The template name.
    templateName :: Prelude.Text,
    -- | The template type.
    templateType :: TemplateType,
    -- | The template major version.
    templateVersion :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetTemplateSyncStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'templateName', 'getTemplateSyncStatus_templateName' - The template name.
--
-- 'templateType', 'getTemplateSyncStatus_templateType' - The template type.
--
-- 'templateVersion', 'getTemplateSyncStatus_templateVersion' - The template major version.
newGetTemplateSyncStatus ::
  -- | 'templateName'
  Prelude.Text ->
  -- | 'templateType'
  TemplateType ->
  -- | 'templateVersion'
  Prelude.Text ->
  GetTemplateSyncStatus
newGetTemplateSyncStatus
  pTemplateName_
  pTemplateType_
  pTemplateVersion_ =
    GetTemplateSyncStatus'
      { templateName =
          pTemplateName_,
        templateType = pTemplateType_,
        templateVersion = pTemplateVersion_
      }

-- | The template name.
getTemplateSyncStatus_templateName :: Lens.Lens' GetTemplateSyncStatus Prelude.Text
getTemplateSyncStatus_templateName = Lens.lens (\GetTemplateSyncStatus' {templateName} -> templateName) (\s@GetTemplateSyncStatus' {} a -> s {templateName = a} :: GetTemplateSyncStatus)

-- | The template type.
getTemplateSyncStatus_templateType :: Lens.Lens' GetTemplateSyncStatus TemplateType
getTemplateSyncStatus_templateType = Lens.lens (\GetTemplateSyncStatus' {templateType} -> templateType) (\s@GetTemplateSyncStatus' {} a -> s {templateType = a} :: GetTemplateSyncStatus)

-- | The template major version.
getTemplateSyncStatus_templateVersion :: Lens.Lens' GetTemplateSyncStatus Prelude.Text
getTemplateSyncStatus_templateVersion = Lens.lens (\GetTemplateSyncStatus' {templateVersion} -> templateVersion) (\s@GetTemplateSyncStatus' {} a -> s {templateVersion = a} :: GetTemplateSyncStatus)

instance Core.AWSRequest GetTemplateSyncStatus where
  type
    AWSResponse GetTemplateSyncStatus =
      GetTemplateSyncStatusResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetTemplateSyncStatusResponse'
            Prelude.<$> (x Data..?> "desiredState")
            Prelude.<*> (x Data..?> "latestSuccessfulSync")
            Prelude.<*> (x Data..?> "latestSync")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetTemplateSyncStatus where
  hashWithSalt _salt GetTemplateSyncStatus' {..} =
    _salt `Prelude.hashWithSalt` templateName
      `Prelude.hashWithSalt` templateType
      `Prelude.hashWithSalt` templateVersion

instance Prelude.NFData GetTemplateSyncStatus where
  rnf GetTemplateSyncStatus' {..} =
    Prelude.rnf templateName
      `Prelude.seq` Prelude.rnf templateType
      `Prelude.seq` Prelude.rnf templateVersion

instance Data.ToHeaders GetTemplateSyncStatus where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AwsProton20200720.GetTemplateSyncStatus" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetTemplateSyncStatus where
  toJSON GetTemplateSyncStatus' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("templateName" Data..= templateName),
            Prelude.Just ("templateType" Data..= templateType),
            Prelude.Just
              ("templateVersion" Data..= templateVersion)
          ]
      )

instance Data.ToPath GetTemplateSyncStatus where
  toPath = Prelude.const "/"

instance Data.ToQuery GetTemplateSyncStatus where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetTemplateSyncStatusResponse' smart constructor.
data GetTemplateSyncStatusResponse = GetTemplateSyncStatusResponse'
  { -- | The template sync desired state that\'s returned by Proton.
    desiredState :: Prelude.Maybe Revision,
    -- | The details of the last successful sync that\'s returned by Proton.
    latestSuccessfulSync :: Prelude.Maybe ResourceSyncAttempt,
    -- | The details of the last sync that\'s returned by Proton.
    latestSync :: Prelude.Maybe ResourceSyncAttempt,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetTemplateSyncStatusResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'desiredState', 'getTemplateSyncStatusResponse_desiredState' - The template sync desired state that\'s returned by Proton.
--
-- 'latestSuccessfulSync', 'getTemplateSyncStatusResponse_latestSuccessfulSync' - The details of the last successful sync that\'s returned by Proton.
--
-- 'latestSync', 'getTemplateSyncStatusResponse_latestSync' - The details of the last sync that\'s returned by Proton.
--
-- 'httpStatus', 'getTemplateSyncStatusResponse_httpStatus' - The response's http status code.
newGetTemplateSyncStatusResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetTemplateSyncStatusResponse
newGetTemplateSyncStatusResponse pHttpStatus_ =
  GetTemplateSyncStatusResponse'
    { desiredState =
        Prelude.Nothing,
      latestSuccessfulSync = Prelude.Nothing,
      latestSync = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The template sync desired state that\'s returned by Proton.
getTemplateSyncStatusResponse_desiredState :: Lens.Lens' GetTemplateSyncStatusResponse (Prelude.Maybe Revision)
getTemplateSyncStatusResponse_desiredState = Lens.lens (\GetTemplateSyncStatusResponse' {desiredState} -> desiredState) (\s@GetTemplateSyncStatusResponse' {} a -> s {desiredState = a} :: GetTemplateSyncStatusResponse)

-- | The details of the last successful sync that\'s returned by Proton.
getTemplateSyncStatusResponse_latestSuccessfulSync :: Lens.Lens' GetTemplateSyncStatusResponse (Prelude.Maybe ResourceSyncAttempt)
getTemplateSyncStatusResponse_latestSuccessfulSync = Lens.lens (\GetTemplateSyncStatusResponse' {latestSuccessfulSync} -> latestSuccessfulSync) (\s@GetTemplateSyncStatusResponse' {} a -> s {latestSuccessfulSync = a} :: GetTemplateSyncStatusResponse)

-- | The details of the last sync that\'s returned by Proton.
getTemplateSyncStatusResponse_latestSync :: Lens.Lens' GetTemplateSyncStatusResponse (Prelude.Maybe ResourceSyncAttempt)
getTemplateSyncStatusResponse_latestSync = Lens.lens (\GetTemplateSyncStatusResponse' {latestSync} -> latestSync) (\s@GetTemplateSyncStatusResponse' {} a -> s {latestSync = a} :: GetTemplateSyncStatusResponse)

-- | The response's http status code.
getTemplateSyncStatusResponse_httpStatus :: Lens.Lens' GetTemplateSyncStatusResponse Prelude.Int
getTemplateSyncStatusResponse_httpStatus = Lens.lens (\GetTemplateSyncStatusResponse' {httpStatus} -> httpStatus) (\s@GetTemplateSyncStatusResponse' {} a -> s {httpStatus = a} :: GetTemplateSyncStatusResponse)

instance Prelude.NFData GetTemplateSyncStatusResponse where
  rnf GetTemplateSyncStatusResponse' {..} =
    Prelude.rnf desiredState
      `Prelude.seq` Prelude.rnf latestSuccessfulSync
      `Prelude.seq` Prelude.rnf latestSync
      `Prelude.seq` Prelude.rnf httpStatus
