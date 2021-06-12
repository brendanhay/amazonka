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
-- Module      : Network.AWS.ImportExport.UpdateJob
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.ImportExport.UpdateJob
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
    updateJobResponse_artifactList,
    updateJobResponse_success,
    updateJobResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ImportExport.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Input structure for the UpateJob operation.
--
-- /See:/ 'newUpdateJob' smart constructor.
data UpdateJob = UpdateJob'
  { aPIVersion :: Core.Maybe Core.Text,
    jobId :: Core.Text,
    manifest :: Core.Text,
    jobType :: JobType,
    validateOnly :: Core.Bool
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'manifest'
  Core.Text ->
  -- | 'jobType'
  JobType ->
  -- | 'validateOnly'
  Core.Bool ->
  UpdateJob
newUpdateJob
  pJobId_
  pManifest_
  pJobType_
  pValidateOnly_ =
    UpdateJob'
      { aPIVersion = Core.Nothing,
        jobId = pJobId_,
        manifest = pManifest_,
        jobType = pJobType_,
        validateOnly = pValidateOnly_
      }

-- | Undocumented member.
updateJob_aPIVersion :: Lens.Lens' UpdateJob (Core.Maybe Core.Text)
updateJob_aPIVersion = Lens.lens (\UpdateJob' {aPIVersion} -> aPIVersion) (\s@UpdateJob' {} a -> s {aPIVersion = a} :: UpdateJob)

-- | Undocumented member.
updateJob_jobId :: Lens.Lens' UpdateJob Core.Text
updateJob_jobId = Lens.lens (\UpdateJob' {jobId} -> jobId) (\s@UpdateJob' {} a -> s {jobId = a} :: UpdateJob)

-- | Undocumented member.
updateJob_manifest :: Lens.Lens' UpdateJob Core.Text
updateJob_manifest = Lens.lens (\UpdateJob' {manifest} -> manifest) (\s@UpdateJob' {} a -> s {manifest = a} :: UpdateJob)

-- | Undocumented member.
updateJob_jobType :: Lens.Lens' UpdateJob JobType
updateJob_jobType = Lens.lens (\UpdateJob' {jobType} -> jobType) (\s@UpdateJob' {} a -> s {jobType = a} :: UpdateJob)

-- | Undocumented member.
updateJob_validateOnly :: Lens.Lens' UpdateJob Core.Bool
updateJob_validateOnly = Lens.lens (\UpdateJob' {validateOnly} -> validateOnly) (\s@UpdateJob' {} a -> s {validateOnly = a} :: UpdateJob)

instance Core.AWSRequest UpdateJob where
  type AWSResponse UpdateJob = UpdateJobResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "UpdateJobResult"
      ( \s h x ->
          UpdateJobResponse'
            Core.<$> (x Core..@? "WarningMessage")
            Core.<*> ( x Core..@? "ArtifactList" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "member")
                     )
            Core.<*> (x Core..@? "Success")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateJob

instance Core.NFData UpdateJob

instance Core.ToHeaders UpdateJob where
  toHeaders = Core.const Core.mempty

instance Core.ToPath UpdateJob where
  toPath = Core.const "/"

instance Core.ToQuery UpdateJob where
  toQuery UpdateJob' {..} =
    Core.mconcat
      [ "Operation=UpdateJob",
        "Action" Core.=: ("UpdateJob" :: Core.ByteString),
        "Version" Core.=: ("2010-06-01" :: Core.ByteString),
        "APIVersion" Core.=: aPIVersion,
        "JobId" Core.=: jobId,
        "Manifest" Core.=: manifest,
        "JobType" Core.=: jobType,
        "ValidateOnly" Core.=: validateOnly
      ]

-- | Output structure for the UpateJob operation.
--
-- /See:/ 'newUpdateJobResponse' smart constructor.
data UpdateJobResponse = UpdateJobResponse'
  { warningMessage :: Core.Maybe Core.Text,
    artifactList :: Core.Maybe [Artifact],
    success :: Core.Maybe Core.Bool,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- 'artifactList', 'updateJobResponse_artifactList' - Undocumented member.
--
-- 'success', 'updateJobResponse_success' - Undocumented member.
--
-- 'httpStatus', 'updateJobResponse_httpStatus' - The response's http status code.
newUpdateJobResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdateJobResponse
newUpdateJobResponse pHttpStatus_ =
  UpdateJobResponse'
    { warningMessage = Core.Nothing,
      artifactList = Core.Nothing,
      success = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
updateJobResponse_warningMessage :: Lens.Lens' UpdateJobResponse (Core.Maybe Core.Text)
updateJobResponse_warningMessage = Lens.lens (\UpdateJobResponse' {warningMessage} -> warningMessage) (\s@UpdateJobResponse' {} a -> s {warningMessage = a} :: UpdateJobResponse)

-- | Undocumented member.
updateJobResponse_artifactList :: Lens.Lens' UpdateJobResponse (Core.Maybe [Artifact])
updateJobResponse_artifactList = Lens.lens (\UpdateJobResponse' {artifactList} -> artifactList) (\s@UpdateJobResponse' {} a -> s {artifactList = a} :: UpdateJobResponse) Core.. Lens.mapping Lens._Coerce

-- | Undocumented member.
updateJobResponse_success :: Lens.Lens' UpdateJobResponse (Core.Maybe Core.Bool)
updateJobResponse_success = Lens.lens (\UpdateJobResponse' {success} -> success) (\s@UpdateJobResponse' {} a -> s {success = a} :: UpdateJobResponse)

-- | The response's http status code.
updateJobResponse_httpStatus :: Lens.Lens' UpdateJobResponse Core.Int
updateJobResponse_httpStatus = Lens.lens (\UpdateJobResponse' {httpStatus} -> httpStatus) (\s@UpdateJobResponse' {} a -> s {httpStatus = a} :: UpdateJobResponse)

instance Core.NFData UpdateJobResponse
