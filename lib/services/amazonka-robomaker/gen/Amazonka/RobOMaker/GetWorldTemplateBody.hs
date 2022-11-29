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
-- Module      : Amazonka.RobOMaker.GetWorldTemplateBody
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the world template body.
module Amazonka.RobOMaker.GetWorldTemplateBody
  ( -- * Creating a Request
    GetWorldTemplateBody (..),
    newGetWorldTemplateBody,

    -- * Request Lenses
    getWorldTemplateBody_generationJob,
    getWorldTemplateBody_template,

    -- * Destructuring the Response
    GetWorldTemplateBodyResponse (..),
    newGetWorldTemplateBodyResponse,

    -- * Response Lenses
    getWorldTemplateBodyResponse_templateBody,
    getWorldTemplateBodyResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.RobOMaker.Types

-- | /See:/ 'newGetWorldTemplateBody' smart constructor.
data GetWorldTemplateBody = GetWorldTemplateBody'
  { -- | The Amazon Resource Name (arn) of the world generator job.
    generationJob :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (arn) of the world template.
    template :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetWorldTemplateBody' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'generationJob', 'getWorldTemplateBody_generationJob' - The Amazon Resource Name (arn) of the world generator job.
--
-- 'template', 'getWorldTemplateBody_template' - The Amazon Resource Name (arn) of the world template.
newGetWorldTemplateBody ::
  GetWorldTemplateBody
newGetWorldTemplateBody =
  GetWorldTemplateBody'
    { generationJob =
        Prelude.Nothing,
      template = Prelude.Nothing
    }

-- | The Amazon Resource Name (arn) of the world generator job.
getWorldTemplateBody_generationJob :: Lens.Lens' GetWorldTemplateBody (Prelude.Maybe Prelude.Text)
getWorldTemplateBody_generationJob = Lens.lens (\GetWorldTemplateBody' {generationJob} -> generationJob) (\s@GetWorldTemplateBody' {} a -> s {generationJob = a} :: GetWorldTemplateBody)

-- | The Amazon Resource Name (arn) of the world template.
getWorldTemplateBody_template :: Lens.Lens' GetWorldTemplateBody (Prelude.Maybe Prelude.Text)
getWorldTemplateBody_template = Lens.lens (\GetWorldTemplateBody' {template} -> template) (\s@GetWorldTemplateBody' {} a -> s {template = a} :: GetWorldTemplateBody)

instance Core.AWSRequest GetWorldTemplateBody where
  type
    AWSResponse GetWorldTemplateBody =
      GetWorldTemplateBodyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetWorldTemplateBodyResponse'
            Prelude.<$> (x Core..?> "templateBody")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetWorldTemplateBody where
  hashWithSalt _salt GetWorldTemplateBody' {..} =
    _salt `Prelude.hashWithSalt` generationJob
      `Prelude.hashWithSalt` template

instance Prelude.NFData GetWorldTemplateBody where
  rnf GetWorldTemplateBody' {..} =
    Prelude.rnf generationJob
      `Prelude.seq` Prelude.rnf template

instance Core.ToHeaders GetWorldTemplateBody where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetWorldTemplateBody where
  toJSON GetWorldTemplateBody' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("generationJob" Core..=) Prelude.<$> generationJob,
            ("template" Core..=) Prelude.<$> template
          ]
      )

instance Core.ToPath GetWorldTemplateBody where
  toPath = Prelude.const "/getWorldTemplateBody"

instance Core.ToQuery GetWorldTemplateBody where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetWorldTemplateBodyResponse' smart constructor.
data GetWorldTemplateBodyResponse = GetWorldTemplateBodyResponse'
  { -- | The world template body.
    templateBody :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetWorldTemplateBodyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'templateBody', 'getWorldTemplateBodyResponse_templateBody' - The world template body.
--
-- 'httpStatus', 'getWorldTemplateBodyResponse_httpStatus' - The response's http status code.
newGetWorldTemplateBodyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetWorldTemplateBodyResponse
newGetWorldTemplateBodyResponse pHttpStatus_ =
  GetWorldTemplateBodyResponse'
    { templateBody =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The world template body.
getWorldTemplateBodyResponse_templateBody :: Lens.Lens' GetWorldTemplateBodyResponse (Prelude.Maybe Prelude.Text)
getWorldTemplateBodyResponse_templateBody = Lens.lens (\GetWorldTemplateBodyResponse' {templateBody} -> templateBody) (\s@GetWorldTemplateBodyResponse' {} a -> s {templateBody = a} :: GetWorldTemplateBodyResponse)

-- | The response's http status code.
getWorldTemplateBodyResponse_httpStatus :: Lens.Lens' GetWorldTemplateBodyResponse Prelude.Int
getWorldTemplateBodyResponse_httpStatus = Lens.lens (\GetWorldTemplateBodyResponse' {httpStatus} -> httpStatus) (\s@GetWorldTemplateBodyResponse' {} a -> s {httpStatus = a} :: GetWorldTemplateBodyResponse)

instance Prelude.NFData GetWorldTemplateBodyResponse where
  rnf GetWorldTemplateBodyResponse' {..} =
    Prelude.rnf templateBody
      `Prelude.seq` Prelude.rnf httpStatus
