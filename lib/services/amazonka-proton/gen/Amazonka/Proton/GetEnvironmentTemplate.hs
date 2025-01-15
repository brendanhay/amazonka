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
-- Module      : Amazonka.Proton.GetEnvironmentTemplate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get detailed data for an environment template.
module Amazonka.Proton.GetEnvironmentTemplate
  ( -- * Creating a Request
    GetEnvironmentTemplate (..),
    newGetEnvironmentTemplate,

    -- * Request Lenses
    getEnvironmentTemplate_name,

    -- * Destructuring the Response
    GetEnvironmentTemplateResponse (..),
    newGetEnvironmentTemplateResponse,

    -- * Response Lenses
    getEnvironmentTemplateResponse_httpStatus,
    getEnvironmentTemplateResponse_environmentTemplate,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Proton.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetEnvironmentTemplate' smart constructor.
data GetEnvironmentTemplate = GetEnvironmentTemplate'
  { -- | The name of the environment template that you want to get the detailed
    -- data for.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetEnvironmentTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'getEnvironmentTemplate_name' - The name of the environment template that you want to get the detailed
-- data for.
newGetEnvironmentTemplate ::
  -- | 'name'
  Prelude.Text ->
  GetEnvironmentTemplate
newGetEnvironmentTemplate pName_ =
  GetEnvironmentTemplate' {name = pName_}

-- | The name of the environment template that you want to get the detailed
-- data for.
getEnvironmentTemplate_name :: Lens.Lens' GetEnvironmentTemplate Prelude.Text
getEnvironmentTemplate_name = Lens.lens (\GetEnvironmentTemplate' {name} -> name) (\s@GetEnvironmentTemplate' {} a -> s {name = a} :: GetEnvironmentTemplate)

instance Core.AWSRequest GetEnvironmentTemplate where
  type
    AWSResponse GetEnvironmentTemplate =
      GetEnvironmentTemplateResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetEnvironmentTemplateResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "environmentTemplate")
      )

instance Prelude.Hashable GetEnvironmentTemplate where
  hashWithSalt _salt GetEnvironmentTemplate' {..} =
    _salt `Prelude.hashWithSalt` name

instance Prelude.NFData GetEnvironmentTemplate where
  rnf GetEnvironmentTemplate' {..} = Prelude.rnf name

instance Data.ToHeaders GetEnvironmentTemplate where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AwsProton20200720.GetEnvironmentTemplate" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetEnvironmentTemplate where
  toJSON GetEnvironmentTemplate' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("name" Data..= name)]
      )

instance Data.ToPath GetEnvironmentTemplate where
  toPath = Prelude.const "/"

instance Data.ToQuery GetEnvironmentTemplate where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetEnvironmentTemplateResponse' smart constructor.
data GetEnvironmentTemplateResponse = GetEnvironmentTemplateResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The detailed data of the requested environment template.
    environmentTemplate :: EnvironmentTemplate
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetEnvironmentTemplateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getEnvironmentTemplateResponse_httpStatus' - The response's http status code.
--
-- 'environmentTemplate', 'getEnvironmentTemplateResponse_environmentTemplate' - The detailed data of the requested environment template.
newGetEnvironmentTemplateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'environmentTemplate'
  EnvironmentTemplate ->
  GetEnvironmentTemplateResponse
newGetEnvironmentTemplateResponse
  pHttpStatus_
  pEnvironmentTemplate_ =
    GetEnvironmentTemplateResponse'
      { httpStatus =
          pHttpStatus_,
        environmentTemplate = pEnvironmentTemplate_
      }

-- | The response's http status code.
getEnvironmentTemplateResponse_httpStatus :: Lens.Lens' GetEnvironmentTemplateResponse Prelude.Int
getEnvironmentTemplateResponse_httpStatus = Lens.lens (\GetEnvironmentTemplateResponse' {httpStatus} -> httpStatus) (\s@GetEnvironmentTemplateResponse' {} a -> s {httpStatus = a} :: GetEnvironmentTemplateResponse)

-- | The detailed data of the requested environment template.
getEnvironmentTemplateResponse_environmentTemplate :: Lens.Lens' GetEnvironmentTemplateResponse EnvironmentTemplate
getEnvironmentTemplateResponse_environmentTemplate = Lens.lens (\GetEnvironmentTemplateResponse' {environmentTemplate} -> environmentTemplate) (\s@GetEnvironmentTemplateResponse' {} a -> s {environmentTemplate = a} :: GetEnvironmentTemplateResponse)

instance
  Prelude.NFData
    GetEnvironmentTemplateResponse
  where
  rnf GetEnvironmentTemplateResponse' {..} =
    Prelude.rnf httpStatus `Prelude.seq`
      Prelude.rnf environmentTemplate
