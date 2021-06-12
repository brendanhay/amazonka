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
-- Module      : Network.AWS.ImportExport.CreateJob
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation initiates the process of scheduling an upload or download
-- of your data. You include in the request a manifest that describes the
-- data transfer specifics. The response to the request includes a job ID,
-- which you can use in other operations, a signature that you use to
-- identify your storage device, and the address where you should ship your
-- storage device.
module Network.AWS.ImportExport.CreateJob
  ( -- * Creating a Request
    CreateJob (..),
    newCreateJob,

    -- * Request Lenses
    createJob_aPIVersion,
    createJob_manifestAddendum,
    createJob_jobType,
    createJob_manifest,
    createJob_validateOnly,

    -- * Destructuring the Response
    CreateJobResponse (..),
    newCreateJobResponse,

    -- * Response Lenses
    createJobResponse_warningMessage,
    createJobResponse_jobType,
    createJobResponse_artifactList,
    createJobResponse_signature,
    createJobResponse_signatureFileContents,
    createJobResponse_jobId,
    createJobResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ImportExport.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Input structure for the CreateJob operation.
--
-- /See:/ 'newCreateJob' smart constructor.
data CreateJob = CreateJob'
  { aPIVersion :: Core.Maybe Core.Text,
    manifestAddendum :: Core.Maybe Core.Text,
    jobType :: JobType,
    manifest :: Core.Text,
    validateOnly :: Core.Bool
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'aPIVersion', 'createJob_aPIVersion' - Undocumented member.
--
-- 'manifestAddendum', 'createJob_manifestAddendum' - Undocumented member.
--
-- 'jobType', 'createJob_jobType' - Undocumented member.
--
-- 'manifest', 'createJob_manifest' - Undocumented member.
--
-- 'validateOnly', 'createJob_validateOnly' - Undocumented member.
newCreateJob ::
  -- | 'jobType'
  JobType ->
  -- | 'manifest'
  Core.Text ->
  -- | 'validateOnly'
  Core.Bool ->
  CreateJob
newCreateJob pJobType_ pManifest_ pValidateOnly_ =
  CreateJob'
    { aPIVersion = Core.Nothing,
      manifestAddendum = Core.Nothing,
      jobType = pJobType_,
      manifest = pManifest_,
      validateOnly = pValidateOnly_
    }

-- | Undocumented member.
createJob_aPIVersion :: Lens.Lens' CreateJob (Core.Maybe Core.Text)
createJob_aPIVersion = Lens.lens (\CreateJob' {aPIVersion} -> aPIVersion) (\s@CreateJob' {} a -> s {aPIVersion = a} :: CreateJob)

-- | Undocumented member.
createJob_manifestAddendum :: Lens.Lens' CreateJob (Core.Maybe Core.Text)
createJob_manifestAddendum = Lens.lens (\CreateJob' {manifestAddendum} -> manifestAddendum) (\s@CreateJob' {} a -> s {manifestAddendum = a} :: CreateJob)

-- | Undocumented member.
createJob_jobType :: Lens.Lens' CreateJob JobType
createJob_jobType = Lens.lens (\CreateJob' {jobType} -> jobType) (\s@CreateJob' {} a -> s {jobType = a} :: CreateJob)

-- | Undocumented member.
createJob_manifest :: Lens.Lens' CreateJob Core.Text
createJob_manifest = Lens.lens (\CreateJob' {manifest} -> manifest) (\s@CreateJob' {} a -> s {manifest = a} :: CreateJob)

-- | Undocumented member.
createJob_validateOnly :: Lens.Lens' CreateJob Core.Bool
createJob_validateOnly = Lens.lens (\CreateJob' {validateOnly} -> validateOnly) (\s@CreateJob' {} a -> s {validateOnly = a} :: CreateJob)

instance Core.AWSRequest CreateJob where
  type AWSResponse CreateJob = CreateJobResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "CreateJobResult"
      ( \s h x ->
          CreateJobResponse'
            Core.<$> (x Core..@? "WarningMessage")
            Core.<*> (x Core..@? "JobType")
            Core.<*> ( x Core..@? "ArtifactList" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "member")
                     )
            Core.<*> (x Core..@? "Signature")
            Core.<*> (x Core..@? "SignatureFileContents")
            Core.<*> (x Core..@? "JobId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateJob

instance Core.NFData CreateJob

instance Core.ToHeaders CreateJob where
  toHeaders = Core.const Core.mempty

instance Core.ToPath CreateJob where
  toPath = Core.const "/"

instance Core.ToQuery CreateJob where
  toQuery CreateJob' {..} =
    Core.mconcat
      [ "Operation=CreateJob",
        "Action" Core.=: ("CreateJob" :: Core.ByteString),
        "Version" Core.=: ("2010-06-01" :: Core.ByteString),
        "APIVersion" Core.=: aPIVersion,
        "ManifestAddendum" Core.=: manifestAddendum,
        "JobType" Core.=: jobType,
        "Manifest" Core.=: manifest,
        "ValidateOnly" Core.=: validateOnly
      ]

-- | Output structure for the CreateJob operation.
--
-- /See:/ 'newCreateJobResponse' smart constructor.
data CreateJobResponse = CreateJobResponse'
  { warningMessage :: Core.Maybe Core.Text,
    jobType :: Core.Maybe JobType,
    artifactList :: Core.Maybe [Artifact],
    signature :: Core.Maybe Core.Text,
    signatureFileContents :: Core.Maybe Core.Text,
    jobId :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'warningMessage', 'createJobResponse_warningMessage' - Undocumented member.
--
-- 'jobType', 'createJobResponse_jobType' - Undocumented member.
--
-- 'artifactList', 'createJobResponse_artifactList' - Undocumented member.
--
-- 'signature', 'createJobResponse_signature' - Undocumented member.
--
-- 'signatureFileContents', 'createJobResponse_signatureFileContents' - Undocumented member.
--
-- 'jobId', 'createJobResponse_jobId' - Undocumented member.
--
-- 'httpStatus', 'createJobResponse_httpStatus' - The response's http status code.
newCreateJobResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateJobResponse
newCreateJobResponse pHttpStatus_ =
  CreateJobResponse'
    { warningMessage = Core.Nothing,
      jobType = Core.Nothing,
      artifactList = Core.Nothing,
      signature = Core.Nothing,
      signatureFileContents = Core.Nothing,
      jobId = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
createJobResponse_warningMessage :: Lens.Lens' CreateJobResponse (Core.Maybe Core.Text)
createJobResponse_warningMessage = Lens.lens (\CreateJobResponse' {warningMessage} -> warningMessage) (\s@CreateJobResponse' {} a -> s {warningMessage = a} :: CreateJobResponse)

-- | Undocumented member.
createJobResponse_jobType :: Lens.Lens' CreateJobResponse (Core.Maybe JobType)
createJobResponse_jobType = Lens.lens (\CreateJobResponse' {jobType} -> jobType) (\s@CreateJobResponse' {} a -> s {jobType = a} :: CreateJobResponse)

-- | Undocumented member.
createJobResponse_artifactList :: Lens.Lens' CreateJobResponse (Core.Maybe [Artifact])
createJobResponse_artifactList = Lens.lens (\CreateJobResponse' {artifactList} -> artifactList) (\s@CreateJobResponse' {} a -> s {artifactList = a} :: CreateJobResponse) Core.. Lens.mapping Lens._Coerce

-- | Undocumented member.
createJobResponse_signature :: Lens.Lens' CreateJobResponse (Core.Maybe Core.Text)
createJobResponse_signature = Lens.lens (\CreateJobResponse' {signature} -> signature) (\s@CreateJobResponse' {} a -> s {signature = a} :: CreateJobResponse)

-- | Undocumented member.
createJobResponse_signatureFileContents :: Lens.Lens' CreateJobResponse (Core.Maybe Core.Text)
createJobResponse_signatureFileContents = Lens.lens (\CreateJobResponse' {signatureFileContents} -> signatureFileContents) (\s@CreateJobResponse' {} a -> s {signatureFileContents = a} :: CreateJobResponse)

-- | Undocumented member.
createJobResponse_jobId :: Lens.Lens' CreateJobResponse (Core.Maybe Core.Text)
createJobResponse_jobId = Lens.lens (\CreateJobResponse' {jobId} -> jobId) (\s@CreateJobResponse' {} a -> s {jobId = a} :: CreateJobResponse)

-- | The response's http status code.
createJobResponse_httpStatus :: Lens.Lens' CreateJobResponse Core.Int
createJobResponse_httpStatus = Lens.lens (\CreateJobResponse' {httpStatus} -> httpStatus) (\s@CreateJobResponse' {} a -> s {httpStatus = a} :: CreateJobResponse)

instance Core.NFData CreateJobResponse
