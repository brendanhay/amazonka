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
-- Module      : Amazonka.EMRContainers.DescribeJobTemplate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Displays detailed information about a specified job template. Job
-- template stores values of StartJobRun API request in a template and can
-- be used to start a job run. Job template allows two use cases: avoid
-- repeating recurring StartJobRun API request values, enforcing certain
-- values in StartJobRun API request.
module Amazonka.EMRContainers.DescribeJobTemplate
  ( -- * Creating a Request
    DescribeJobTemplate (..),
    newDescribeJobTemplate,

    -- * Request Lenses
    describeJobTemplate_id,

    -- * Destructuring the Response
    DescribeJobTemplateResponse (..),
    newDescribeJobTemplateResponse,

    -- * Response Lenses
    describeJobTemplateResponse_jobTemplate,
    describeJobTemplateResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EMRContainers.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeJobTemplate' smart constructor.
data DescribeJobTemplate = DescribeJobTemplate'
  { -- | The ID of the job template that will be described.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeJobTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'describeJobTemplate_id' - The ID of the job template that will be described.
newDescribeJobTemplate ::
  -- | 'id'
  Prelude.Text ->
  DescribeJobTemplate
newDescribeJobTemplate pId_ =
  DescribeJobTemplate' {id = pId_}

-- | The ID of the job template that will be described.
describeJobTemplate_id :: Lens.Lens' DescribeJobTemplate Prelude.Text
describeJobTemplate_id = Lens.lens (\DescribeJobTemplate' {id} -> id) (\s@DescribeJobTemplate' {} a -> s {id = a} :: DescribeJobTemplate)

instance Core.AWSRequest DescribeJobTemplate where
  type
    AWSResponse DescribeJobTemplate =
      DescribeJobTemplateResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeJobTemplateResponse'
            Prelude.<$> (x Data..?> "jobTemplate")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeJobTemplate where
  hashWithSalt _salt DescribeJobTemplate' {..} =
    _salt `Prelude.hashWithSalt` id

instance Prelude.NFData DescribeJobTemplate where
  rnf DescribeJobTemplate' {..} = Prelude.rnf id

instance Data.ToHeaders DescribeJobTemplate where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribeJobTemplate where
  toPath DescribeJobTemplate' {..} =
    Prelude.mconcat ["/jobtemplates/", Data.toBS id]

instance Data.ToQuery DescribeJobTemplate where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeJobTemplateResponse' smart constructor.
data DescribeJobTemplateResponse = DescribeJobTemplateResponse'
  { -- | This output displays information about the specified job template.
    jobTemplate :: Prelude.Maybe JobTemplate,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeJobTemplateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobTemplate', 'describeJobTemplateResponse_jobTemplate' - This output displays information about the specified job template.
--
-- 'httpStatus', 'describeJobTemplateResponse_httpStatus' - The response's http status code.
newDescribeJobTemplateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeJobTemplateResponse
newDescribeJobTemplateResponse pHttpStatus_ =
  DescribeJobTemplateResponse'
    { jobTemplate =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | This output displays information about the specified job template.
describeJobTemplateResponse_jobTemplate :: Lens.Lens' DescribeJobTemplateResponse (Prelude.Maybe JobTemplate)
describeJobTemplateResponse_jobTemplate = Lens.lens (\DescribeJobTemplateResponse' {jobTemplate} -> jobTemplate) (\s@DescribeJobTemplateResponse' {} a -> s {jobTemplate = a} :: DescribeJobTemplateResponse)

-- | The response's http status code.
describeJobTemplateResponse_httpStatus :: Lens.Lens' DescribeJobTemplateResponse Prelude.Int
describeJobTemplateResponse_httpStatus = Lens.lens (\DescribeJobTemplateResponse' {httpStatus} -> httpStatus) (\s@DescribeJobTemplateResponse' {} a -> s {httpStatus = a} :: DescribeJobTemplateResponse)

instance Prelude.NFData DescribeJobTemplateResponse where
  rnf DescribeJobTemplateResponse' {..} =
    Prelude.rnf jobTemplate `Prelude.seq`
      Prelude.rnf httpStatus
