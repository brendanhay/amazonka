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
-- Module      : Amazonka.MediaConvert.GetJobTemplate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieve the JSON for a specific job template.
module Amazonka.MediaConvert.GetJobTemplate
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConvert.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetJobTemplate' smart constructor.
data GetJobTemplate = GetJobTemplate'
  { -- | The name of the job template.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  GetJobTemplate
newGetJobTemplate pName_ =
  GetJobTemplate' {name = pName_}

-- | The name of the job template.
getJobTemplate_name :: Lens.Lens' GetJobTemplate Prelude.Text
getJobTemplate_name = Lens.lens (\GetJobTemplate' {name} -> name) (\s@GetJobTemplate' {} a -> s {name = a} :: GetJobTemplate)

instance Core.AWSRequest GetJobTemplate where
  type
    AWSResponse GetJobTemplate =
      GetJobTemplateResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetJobTemplateResponse'
            Prelude.<$> (x Data..?> "jobTemplate")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetJobTemplate where
  hashWithSalt _salt GetJobTemplate' {..} =
    _salt `Prelude.hashWithSalt` name

instance Prelude.NFData GetJobTemplate where
  rnf GetJobTemplate' {..} = Prelude.rnf name

instance Data.ToHeaders GetJobTemplate where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetJobTemplate where
  toPath GetJobTemplate' {..} =
    Prelude.mconcat
      ["/2017-08-29/jobTemplates/", Data.toBS name]

instance Data.ToQuery GetJobTemplate where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetJobTemplateResponse' smart constructor.
data GetJobTemplateResponse = GetJobTemplateResponse'
  { -- | A job template is a pre-made set of encoding instructions that you can
    -- use to quickly create a job.
    jobTemplate :: Prelude.Maybe JobTemplate,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  GetJobTemplateResponse
newGetJobTemplateResponse pHttpStatus_ =
  GetJobTemplateResponse'
    { jobTemplate =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A job template is a pre-made set of encoding instructions that you can
-- use to quickly create a job.
getJobTemplateResponse_jobTemplate :: Lens.Lens' GetJobTemplateResponse (Prelude.Maybe JobTemplate)
getJobTemplateResponse_jobTemplate = Lens.lens (\GetJobTemplateResponse' {jobTemplate} -> jobTemplate) (\s@GetJobTemplateResponse' {} a -> s {jobTemplate = a} :: GetJobTemplateResponse)

-- | The response's http status code.
getJobTemplateResponse_httpStatus :: Lens.Lens' GetJobTemplateResponse Prelude.Int
getJobTemplateResponse_httpStatus = Lens.lens (\GetJobTemplateResponse' {httpStatus} -> httpStatus) (\s@GetJobTemplateResponse' {} a -> s {httpStatus = a} :: GetJobTemplateResponse)

instance Prelude.NFData GetJobTemplateResponse where
  rnf GetJobTemplateResponse' {..} =
    Prelude.rnf jobTemplate
      `Prelude.seq` Prelude.rnf httpStatus
