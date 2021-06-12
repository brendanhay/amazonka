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
-- Module      : Network.AWS.MediaConvert.GetJobTemplate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieve the JSON for a specific job template.
module Network.AWS.MediaConvert.GetJobTemplate
  ( -- * Creating a Request
    GetJobTemplate (..),
    newGetJobTemplate,

    -- * Request Lenses
    getJobTemplate_name,

    -- * Destructuring the Response
    GetJobTemplateResponse (..),
    newGetJobTemplateResponse,

    -- * Response Lenses
    getJobTemplateResponse_jobTemplate,
    getJobTemplateResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetJobTemplate' smart constructor.
data GetJobTemplate = GetJobTemplate'
  { -- | The name of the job template.
    name :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetJobTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'getJobTemplate_name' - The name of the job template.
newGetJobTemplate ::
  -- | 'name'
  Core.Text ->
  GetJobTemplate
newGetJobTemplate pName_ =
  GetJobTemplate' {name = pName_}

-- | The name of the job template.
getJobTemplate_name :: Lens.Lens' GetJobTemplate Core.Text
getJobTemplate_name = Lens.lens (\GetJobTemplate' {name} -> name) (\s@GetJobTemplate' {} a -> s {name = a} :: GetJobTemplate)

instance Core.AWSRequest GetJobTemplate where
  type
    AWSResponse GetJobTemplate =
      GetJobTemplateResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetJobTemplateResponse'
            Core.<$> (x Core..?> "jobTemplate")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetJobTemplate

instance Core.NFData GetJobTemplate

instance Core.ToHeaders GetJobTemplate where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath GetJobTemplate where
  toPath GetJobTemplate' {..} =
    Core.mconcat
      ["/2017-08-29/jobTemplates/", Core.toBS name]

instance Core.ToQuery GetJobTemplate where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetJobTemplateResponse' smart constructor.
data GetJobTemplateResponse = GetJobTemplateResponse'
  { -- | A job template is a pre-made set of encoding instructions that you can
    -- use to quickly create a job.
    jobTemplate :: Core.Maybe JobTemplate,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetJobTemplateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobTemplate', 'getJobTemplateResponse_jobTemplate' - A job template is a pre-made set of encoding instructions that you can
-- use to quickly create a job.
--
-- 'httpStatus', 'getJobTemplateResponse_httpStatus' - The response's http status code.
newGetJobTemplateResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetJobTemplateResponse
newGetJobTemplateResponse pHttpStatus_ =
  GetJobTemplateResponse'
    { jobTemplate = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A job template is a pre-made set of encoding instructions that you can
-- use to quickly create a job.
getJobTemplateResponse_jobTemplate :: Lens.Lens' GetJobTemplateResponse (Core.Maybe JobTemplate)
getJobTemplateResponse_jobTemplate = Lens.lens (\GetJobTemplateResponse' {jobTemplate} -> jobTemplate) (\s@GetJobTemplateResponse' {} a -> s {jobTemplate = a} :: GetJobTemplateResponse)

-- | The response's http status code.
getJobTemplateResponse_httpStatus :: Lens.Lens' GetJobTemplateResponse Core.Int
getJobTemplateResponse_httpStatus = Lens.lens (\GetJobTemplateResponse' {httpStatus} -> httpStatus) (\s@GetJobTemplateResponse' {} a -> s {httpStatus = a} :: GetJobTemplateResponse)

instance Core.NFData GetJobTemplateResponse
