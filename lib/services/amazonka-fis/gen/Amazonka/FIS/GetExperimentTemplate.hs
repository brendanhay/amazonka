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
-- Module      : Amazonka.FIS.GetExperimentTemplate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the specified experiment template.
module Amazonka.FIS.GetExperimentTemplate
  ( -- * Creating a Request
    GetExperimentTemplate (..),
    newGetExperimentTemplate,

    -- * Request Lenses
    getExperimentTemplate_id,

    -- * Destructuring the Response
    GetExperimentTemplateResponse (..),
    newGetExperimentTemplateResponse,

    -- * Response Lenses
    getExperimentTemplateResponse_experimentTemplate,
    getExperimentTemplateResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FIS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetExperimentTemplate' smart constructor.
data GetExperimentTemplate = GetExperimentTemplate'
  { -- | The ID of the experiment template.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetExperimentTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'getExperimentTemplate_id' - The ID of the experiment template.
newGetExperimentTemplate ::
  -- | 'id'
  Prelude.Text ->
  GetExperimentTemplate
newGetExperimentTemplate pId_ =
  GetExperimentTemplate' {id = pId_}

-- | The ID of the experiment template.
getExperimentTemplate_id :: Lens.Lens' GetExperimentTemplate Prelude.Text
getExperimentTemplate_id = Lens.lens (\GetExperimentTemplate' {id} -> id) (\s@GetExperimentTemplate' {} a -> s {id = a} :: GetExperimentTemplate)

instance Core.AWSRequest GetExperimentTemplate where
  type
    AWSResponse GetExperimentTemplate =
      GetExperimentTemplateResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetExperimentTemplateResponse'
            Prelude.<$> (x Data..?> "experimentTemplate")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetExperimentTemplate where
  hashWithSalt _salt GetExperimentTemplate' {..} =
    _salt `Prelude.hashWithSalt` id

instance Prelude.NFData GetExperimentTemplate where
  rnf GetExperimentTemplate' {..} = Prelude.rnf id

instance Data.ToHeaders GetExperimentTemplate where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetExperimentTemplate where
  toPath GetExperimentTemplate' {..} =
    Prelude.mconcat
      ["/experimentTemplates/", Data.toBS id]

instance Data.ToQuery GetExperimentTemplate where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetExperimentTemplateResponse' smart constructor.
data GetExperimentTemplateResponse = GetExperimentTemplateResponse'
  { -- | Information about the experiment template.
    experimentTemplate :: Prelude.Maybe ExperimentTemplate,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetExperimentTemplateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'experimentTemplate', 'getExperimentTemplateResponse_experimentTemplate' - Information about the experiment template.
--
-- 'httpStatus', 'getExperimentTemplateResponse_httpStatus' - The response's http status code.
newGetExperimentTemplateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetExperimentTemplateResponse
newGetExperimentTemplateResponse pHttpStatus_ =
  GetExperimentTemplateResponse'
    { experimentTemplate =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the experiment template.
getExperimentTemplateResponse_experimentTemplate :: Lens.Lens' GetExperimentTemplateResponse (Prelude.Maybe ExperimentTemplate)
getExperimentTemplateResponse_experimentTemplate = Lens.lens (\GetExperimentTemplateResponse' {experimentTemplate} -> experimentTemplate) (\s@GetExperimentTemplateResponse' {} a -> s {experimentTemplate = a} :: GetExperimentTemplateResponse)

-- | The response's http status code.
getExperimentTemplateResponse_httpStatus :: Lens.Lens' GetExperimentTemplateResponse Prelude.Int
getExperimentTemplateResponse_httpStatus = Lens.lens (\GetExperimentTemplateResponse' {httpStatus} -> httpStatus) (\s@GetExperimentTemplateResponse' {} a -> s {httpStatus = a} :: GetExperimentTemplateResponse)

instance Prelude.NFData GetExperimentTemplateResponse where
  rnf GetExperimentTemplateResponse' {..} =
    Prelude.rnf experimentTemplate
      `Prelude.seq` Prelude.rnf httpStatus
