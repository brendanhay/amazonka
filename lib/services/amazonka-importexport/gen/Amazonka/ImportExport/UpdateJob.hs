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
-- Module      : Amazonka.ImportExport.UpdateJob
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- You use this operation to change the parameters specified in the
-- original manifest file by supplying a new manifest file. The manifest
-- file attached to this request replaces the original manifest file. You
-- can only use the operation after a CreateJob request but before the data
-- transfer starts and you can only use it on jobs you own.
module Amazonka.ImportExport.UpdateJob
  ( -- * Creating a Request
    UpdateJob (..),
    newUpdateJob,

    -- * Request Lenses
    updateJob_aPIVersion,
    updateJob_jobId,
    updateJob_manifest,
    updateJob_jobType,
    updateJob_validateOnly,

    -- * Destructuring the Response
    UpdateJobResponse (..),
    newUpdateJobResponse,

    -- * Response Lenses
    updateJobResponse_warningMessage,
    updateJobResponse_success,
    updateJobResponse_artifactList,
    updateJobResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ImportExport.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Input structure for the UpateJob operation.
--
-- /See:/ 'newUpdateJob' smart constructor.
data UpdateJob = UpdateJob'
  { aPIVersion :: Prelude.Maybe Prelude.Text,
    jobId :: Prelude.Text,
    manifest :: Prelude.Text,
    jobType :: JobType,
    validateOnly :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'aPIVersion', 'updateJob_aPIVersion' - Undocumented member.
--
-- 'jobId', 'updateJob_jobId' - Undocumented member.
--
-- 'manifest', 'updateJob_manifest' - Undocumented member.
--
-- 'jobType', 'updateJob_jobType' - Undocumented member.
--
-- 'validateOnly', 'updateJob_validateOnly' - Undocumented member.
newUpdateJob ::
  -- | 'jobId'
  Prelude.Text ->
  -- | 'manifest'
  Prelude.Text ->
  -- | 'jobType'
  JobType ->
  -- | 'validateOnly'
  Prelude.Bool ->
  UpdateJob
newUpdateJob
  pJobId_
  pManifest_
  pJobType_
  pValidateOnly_ =
    UpdateJob'
      { aPIVersion = Prelude.Nothing,
        jobId = pJobId_,
        manifest = pManifest_,
        jobType = pJobType_,
        validateOnly = pValidateOnly_
      }

-- | Undocumented member.
updateJob_aPIVersion :: Lens.Lens' UpdateJob (Prelude.Maybe Prelude.Text)
updateJob_aPIVersion = Lens.lens (\UpdateJob' {aPIVersion} -> aPIVersion) (\s@UpdateJob' {} a -> s {aPIVersion = a} :: UpdateJob)

-- | Undocumented member.
updateJob_jobId :: Lens.Lens' UpdateJob Prelude.Text
updateJob_jobId = Lens.lens (\UpdateJob' {jobId} -> jobId) (\s@UpdateJob' {} a -> s {jobId = a} :: UpdateJob)

-- | Undocumented member.
updateJob_manifest :: Lens.Lens' UpdateJob Prelude.Text
updateJob_manifest = Lens.lens (\UpdateJob' {manifest} -> manifest) (\s@UpdateJob' {} a -> s {manifest = a} :: UpdateJob)

-- | Undocumented member.
updateJob_jobType :: Lens.Lens' UpdateJob JobType
updateJob_jobType = Lens.lens (\UpdateJob' {jobType} -> jobType) (\s@UpdateJob' {} a -> s {jobType = a} :: UpdateJob)

-- | Undocumented member.
updateJob_validateOnly :: Lens.Lens' UpdateJob Prelude.Bool
updateJob_validateOnly = Lens.lens (\UpdateJob' {validateOnly} -> validateOnly) (\s@UpdateJob' {} a -> s {validateOnly = a} :: UpdateJob)

instance Core.AWSRequest UpdateJob where
  type AWSResponse UpdateJob = UpdateJobResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "UpdateJobResult"
      ( \s h x ->
          UpdateJobResponse'
            Prelude.<$> (x Data..@? "WarningMessage")
            Prelude.<*> (x Data..@? "Success")
            Prelude.<*> ( x Data..@? "ArtifactList" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "member")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateJob where
  hashWithSalt _salt UpdateJob' {..} =
    _salt `Prelude.hashWithSalt` aPIVersion
      `Prelude.hashWithSalt` jobId
      `Prelude.hashWithSalt` manifest
      `Prelude.hashWithSalt` jobType
      `Prelude.hashWithSalt` validateOnly

instance Prelude.NFData UpdateJob where
  rnf UpdateJob' {..} =
    Prelude.rnf aPIVersion
      `Prelude.seq` Prelude.rnf jobId
      `Prelude.seq` Prelude.rnf manifest
      `Prelude.seq` Prelude.rnf jobType
      `Prelude.seq` Prelude.rnf validateOnly

instance Data.ToHeaders UpdateJob where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath UpdateJob where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateJob where
  toQuery UpdateJob' {..} =
    Prelude.mconcat
      [ "Operation=UpdateJob",
        "Action" Data.=: ("UpdateJob" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-06-01" :: Prelude.ByteString),
        "APIVersion" Data.=: aPIVersion,
        "JobId" Data.=: jobId,
        "Manifest" Data.=: manifest,
        "JobType" Data.=: jobType,
        "ValidateOnly" Data.=: validateOnly
      ]

-- | Output structure for the UpateJob operation.
--
-- /See:/ 'newUpdateJobResponse' smart constructor.
data UpdateJobResponse = UpdateJobResponse'
  { warningMessage :: Prelude.Maybe Prelude.Text,
    success :: Prelude.Maybe Prelude.Bool,
    artifactList :: Prelude.Maybe [Artifact],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'warningMessage', 'updateJobResponse_warningMessage' - Undocumented member.
--
-- 'success', 'updateJobResponse_success' - Undocumented member.
--
-- 'artifactList', 'updateJobResponse_artifactList' - Undocumented member.
--
-- 'httpStatus', 'updateJobResponse_httpStatus' - The response's http status code.
newUpdateJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateJobResponse
newUpdateJobResponse pHttpStatus_ =
  UpdateJobResponse'
    { warningMessage =
        Prelude.Nothing,
      success = Prelude.Nothing,
      artifactList = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
updateJobResponse_warningMessage :: Lens.Lens' UpdateJobResponse (Prelude.Maybe Prelude.Text)
updateJobResponse_warningMessage = Lens.lens (\UpdateJobResponse' {warningMessage} -> warningMessage) (\s@UpdateJobResponse' {} a -> s {warningMessage = a} :: UpdateJobResponse)

-- | Undocumented member.
updateJobResponse_success :: Lens.Lens' UpdateJobResponse (Prelude.Maybe Prelude.Bool)
updateJobResponse_success = Lens.lens (\UpdateJobResponse' {success} -> success) (\s@UpdateJobResponse' {} a -> s {success = a} :: UpdateJobResponse)

-- | Undocumented member.
updateJobResponse_artifactList :: Lens.Lens' UpdateJobResponse (Prelude.Maybe [Artifact])
updateJobResponse_artifactList = Lens.lens (\UpdateJobResponse' {artifactList} -> artifactList) (\s@UpdateJobResponse' {} a -> s {artifactList = a} :: UpdateJobResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
updateJobResponse_httpStatus :: Lens.Lens' UpdateJobResponse Prelude.Int
updateJobResponse_httpStatus = Lens.lens (\UpdateJobResponse' {httpStatus} -> httpStatus) (\s@UpdateJobResponse' {} a -> s {httpStatus = a} :: UpdateJobResponse)

instance Prelude.NFData UpdateJobResponse where
  rnf UpdateJobResponse' {..} =
    Prelude.rnf warningMessage
      `Prelude.seq` Prelude.rnf success
      `Prelude.seq` Prelude.rnf artifactList
      `Prelude.seq` Prelude.rnf httpStatus
