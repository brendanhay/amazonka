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
-- Module      : Amazonka.Panorama.CreateNodeFromTemplateJob
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a camera stream node.
module Amazonka.Panorama.CreateNodeFromTemplateJob
  ( -- * Creating a Request
    CreateNodeFromTemplateJob (..),
    newCreateNodeFromTemplateJob,

    -- * Request Lenses
    createNodeFromTemplateJob_jobTags,
    createNodeFromTemplateJob_nodeDescription,
    createNodeFromTemplateJob_nodeName,
    createNodeFromTemplateJob_outputPackageName,
    createNodeFromTemplateJob_outputPackageVersion,
    createNodeFromTemplateJob_templateParameters,
    createNodeFromTemplateJob_templateType,

    -- * Destructuring the Response
    CreateNodeFromTemplateJobResponse (..),
    newCreateNodeFromTemplateJobResponse,

    -- * Response Lenses
    createNodeFromTemplateJobResponse_httpStatus,
    createNodeFromTemplateJobResponse_jobId,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Panorama.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateNodeFromTemplateJob' smart constructor.
data CreateNodeFromTemplateJob = CreateNodeFromTemplateJob'
  { -- | Tags for the job.
    jobTags :: Prelude.Maybe [JobResourceTags],
    -- | A description for the node.
    nodeDescription :: Prelude.Maybe Prelude.Text,
    -- | A name for the node.
    nodeName :: Prelude.Text,
    -- | An output package name for the node.
    outputPackageName :: Prelude.Text,
    -- | An output package version for the node.
    outputPackageVersion :: Prelude.Text,
    -- | Template parameters for the node.
    templateParameters :: Prelude.HashMap Prelude.Text (Data.Sensitive Prelude.Text),
    -- | The type of node.
    templateType :: TemplateType
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateNodeFromTemplateJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobTags', 'createNodeFromTemplateJob_jobTags' - Tags for the job.
--
-- 'nodeDescription', 'createNodeFromTemplateJob_nodeDescription' - A description for the node.
--
-- 'nodeName', 'createNodeFromTemplateJob_nodeName' - A name for the node.
--
-- 'outputPackageName', 'createNodeFromTemplateJob_outputPackageName' - An output package name for the node.
--
-- 'outputPackageVersion', 'createNodeFromTemplateJob_outputPackageVersion' - An output package version for the node.
--
-- 'templateParameters', 'createNodeFromTemplateJob_templateParameters' - Template parameters for the node.
--
-- 'templateType', 'createNodeFromTemplateJob_templateType' - The type of node.
newCreateNodeFromTemplateJob ::
  -- | 'nodeName'
  Prelude.Text ->
  -- | 'outputPackageName'
  Prelude.Text ->
  -- | 'outputPackageVersion'
  Prelude.Text ->
  -- | 'templateType'
  TemplateType ->
  CreateNodeFromTemplateJob
newCreateNodeFromTemplateJob
  pNodeName_
  pOutputPackageName_
  pOutputPackageVersion_
  pTemplateType_ =
    CreateNodeFromTemplateJob'
      { jobTags =
          Prelude.Nothing,
        nodeDescription = Prelude.Nothing,
        nodeName = pNodeName_,
        outputPackageName = pOutputPackageName_,
        outputPackageVersion = pOutputPackageVersion_,
        templateParameters = Prelude.mempty,
        templateType = pTemplateType_
      }

-- | Tags for the job.
createNodeFromTemplateJob_jobTags :: Lens.Lens' CreateNodeFromTemplateJob (Prelude.Maybe [JobResourceTags])
createNodeFromTemplateJob_jobTags = Lens.lens (\CreateNodeFromTemplateJob' {jobTags} -> jobTags) (\s@CreateNodeFromTemplateJob' {} a -> s {jobTags = a} :: CreateNodeFromTemplateJob) Prelude.. Lens.mapping Lens.coerced

-- | A description for the node.
createNodeFromTemplateJob_nodeDescription :: Lens.Lens' CreateNodeFromTemplateJob (Prelude.Maybe Prelude.Text)
createNodeFromTemplateJob_nodeDescription = Lens.lens (\CreateNodeFromTemplateJob' {nodeDescription} -> nodeDescription) (\s@CreateNodeFromTemplateJob' {} a -> s {nodeDescription = a} :: CreateNodeFromTemplateJob)

-- | A name for the node.
createNodeFromTemplateJob_nodeName :: Lens.Lens' CreateNodeFromTemplateJob Prelude.Text
createNodeFromTemplateJob_nodeName = Lens.lens (\CreateNodeFromTemplateJob' {nodeName} -> nodeName) (\s@CreateNodeFromTemplateJob' {} a -> s {nodeName = a} :: CreateNodeFromTemplateJob)

-- | An output package name for the node.
createNodeFromTemplateJob_outputPackageName :: Lens.Lens' CreateNodeFromTemplateJob Prelude.Text
createNodeFromTemplateJob_outputPackageName = Lens.lens (\CreateNodeFromTemplateJob' {outputPackageName} -> outputPackageName) (\s@CreateNodeFromTemplateJob' {} a -> s {outputPackageName = a} :: CreateNodeFromTemplateJob)

-- | An output package version for the node.
createNodeFromTemplateJob_outputPackageVersion :: Lens.Lens' CreateNodeFromTemplateJob Prelude.Text
createNodeFromTemplateJob_outputPackageVersion = Lens.lens (\CreateNodeFromTemplateJob' {outputPackageVersion} -> outputPackageVersion) (\s@CreateNodeFromTemplateJob' {} a -> s {outputPackageVersion = a} :: CreateNodeFromTemplateJob)

-- | Template parameters for the node.
createNodeFromTemplateJob_templateParameters :: Lens.Lens' CreateNodeFromTemplateJob (Prelude.HashMap Prelude.Text Prelude.Text)
createNodeFromTemplateJob_templateParameters = Lens.lens (\CreateNodeFromTemplateJob' {templateParameters} -> templateParameters) (\s@CreateNodeFromTemplateJob' {} a -> s {templateParameters = a} :: CreateNodeFromTemplateJob) Prelude.. Lens.coerced

-- | The type of node.
createNodeFromTemplateJob_templateType :: Lens.Lens' CreateNodeFromTemplateJob TemplateType
createNodeFromTemplateJob_templateType = Lens.lens (\CreateNodeFromTemplateJob' {templateType} -> templateType) (\s@CreateNodeFromTemplateJob' {} a -> s {templateType = a} :: CreateNodeFromTemplateJob)

instance Core.AWSRequest CreateNodeFromTemplateJob where
  type
    AWSResponse CreateNodeFromTemplateJob =
      CreateNodeFromTemplateJobResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateNodeFromTemplateJobResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "JobId")
      )

instance Prelude.Hashable CreateNodeFromTemplateJob where
  hashWithSalt _salt CreateNodeFromTemplateJob' {..} =
    _salt
      `Prelude.hashWithSalt` jobTags
      `Prelude.hashWithSalt` nodeDescription
      `Prelude.hashWithSalt` nodeName
      `Prelude.hashWithSalt` outputPackageName
      `Prelude.hashWithSalt` outputPackageVersion
      `Prelude.hashWithSalt` templateParameters
      `Prelude.hashWithSalt` templateType

instance Prelude.NFData CreateNodeFromTemplateJob where
  rnf CreateNodeFromTemplateJob' {..} =
    Prelude.rnf jobTags `Prelude.seq`
      Prelude.rnf nodeDescription `Prelude.seq`
        Prelude.rnf nodeName `Prelude.seq`
          Prelude.rnf outputPackageName `Prelude.seq`
            Prelude.rnf outputPackageVersion `Prelude.seq`
              Prelude.rnf templateParameters `Prelude.seq`
                Prelude.rnf templateType

instance Data.ToHeaders CreateNodeFromTemplateJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateNodeFromTemplateJob where
  toJSON CreateNodeFromTemplateJob' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("JobTags" Data..=) Prelude.<$> jobTags,
            ("NodeDescription" Data..=)
              Prelude.<$> nodeDescription,
            Prelude.Just ("NodeName" Data..= nodeName),
            Prelude.Just
              ("OutputPackageName" Data..= outputPackageName),
            Prelude.Just
              ( "OutputPackageVersion"
                  Data..= outputPackageVersion
              ),
            Prelude.Just
              ("TemplateParameters" Data..= templateParameters),
            Prelude.Just ("TemplateType" Data..= templateType)
          ]
      )

instance Data.ToPath CreateNodeFromTemplateJob where
  toPath = Prelude.const "/packages/template-job"

instance Data.ToQuery CreateNodeFromTemplateJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateNodeFromTemplateJobResponse' smart constructor.
data CreateNodeFromTemplateJobResponse = CreateNodeFromTemplateJobResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The job\'s ID.
    jobId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateNodeFromTemplateJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createNodeFromTemplateJobResponse_httpStatus' - The response's http status code.
--
-- 'jobId', 'createNodeFromTemplateJobResponse_jobId' - The job\'s ID.
newCreateNodeFromTemplateJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'jobId'
  Prelude.Text ->
  CreateNodeFromTemplateJobResponse
newCreateNodeFromTemplateJobResponse
  pHttpStatus_
  pJobId_ =
    CreateNodeFromTemplateJobResponse'
      { httpStatus =
          pHttpStatus_,
        jobId = pJobId_
      }

-- | The response's http status code.
createNodeFromTemplateJobResponse_httpStatus :: Lens.Lens' CreateNodeFromTemplateJobResponse Prelude.Int
createNodeFromTemplateJobResponse_httpStatus = Lens.lens (\CreateNodeFromTemplateJobResponse' {httpStatus} -> httpStatus) (\s@CreateNodeFromTemplateJobResponse' {} a -> s {httpStatus = a} :: CreateNodeFromTemplateJobResponse)

-- | The job\'s ID.
createNodeFromTemplateJobResponse_jobId :: Lens.Lens' CreateNodeFromTemplateJobResponse Prelude.Text
createNodeFromTemplateJobResponse_jobId = Lens.lens (\CreateNodeFromTemplateJobResponse' {jobId} -> jobId) (\s@CreateNodeFromTemplateJobResponse' {} a -> s {jobId = a} :: CreateNodeFromTemplateJobResponse)

instance
  Prelude.NFData
    CreateNodeFromTemplateJobResponse
  where
  rnf CreateNodeFromTemplateJobResponse' {..} =
    Prelude.rnf httpStatus `Prelude.seq`
      Prelude.rnf jobId
