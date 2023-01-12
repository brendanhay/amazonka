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
-- Module      : Amazonka.Proton.UpdateEnvironmentTemplate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update an environment template.
module Amazonka.Proton.UpdateEnvironmentTemplate
  ( -- * Creating a Request
    UpdateEnvironmentTemplate (..),
    newUpdateEnvironmentTemplate,

    -- * Request Lenses
    updateEnvironmentTemplate_description,
    updateEnvironmentTemplate_displayName,
    updateEnvironmentTemplate_name,

    -- * Destructuring the Response
    UpdateEnvironmentTemplateResponse (..),
    newUpdateEnvironmentTemplateResponse,

    -- * Response Lenses
    updateEnvironmentTemplateResponse_httpStatus,
    updateEnvironmentTemplateResponse_environmentTemplate,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Proton.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateEnvironmentTemplate' smart constructor.
data UpdateEnvironmentTemplate = UpdateEnvironmentTemplate'
  { -- | A description of the environment template update.
    description :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The name of the environment template to update as displayed in the
    -- developer interface.
    displayName :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The name of the environment template to update.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateEnvironmentTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'updateEnvironmentTemplate_description' - A description of the environment template update.
--
-- 'displayName', 'updateEnvironmentTemplate_displayName' - The name of the environment template to update as displayed in the
-- developer interface.
--
-- 'name', 'updateEnvironmentTemplate_name' - The name of the environment template to update.
newUpdateEnvironmentTemplate ::
  -- | 'name'
  Prelude.Text ->
  UpdateEnvironmentTemplate
newUpdateEnvironmentTemplate pName_ =
  UpdateEnvironmentTemplate'
    { description =
        Prelude.Nothing,
      displayName = Prelude.Nothing,
      name = pName_
    }

-- | A description of the environment template update.
updateEnvironmentTemplate_description :: Lens.Lens' UpdateEnvironmentTemplate (Prelude.Maybe Prelude.Text)
updateEnvironmentTemplate_description = Lens.lens (\UpdateEnvironmentTemplate' {description} -> description) (\s@UpdateEnvironmentTemplate' {} a -> s {description = a} :: UpdateEnvironmentTemplate) Prelude.. Lens.mapping Data._Sensitive

-- | The name of the environment template to update as displayed in the
-- developer interface.
updateEnvironmentTemplate_displayName :: Lens.Lens' UpdateEnvironmentTemplate (Prelude.Maybe Prelude.Text)
updateEnvironmentTemplate_displayName = Lens.lens (\UpdateEnvironmentTemplate' {displayName} -> displayName) (\s@UpdateEnvironmentTemplate' {} a -> s {displayName = a} :: UpdateEnvironmentTemplate) Prelude.. Lens.mapping Data._Sensitive

-- | The name of the environment template to update.
updateEnvironmentTemplate_name :: Lens.Lens' UpdateEnvironmentTemplate Prelude.Text
updateEnvironmentTemplate_name = Lens.lens (\UpdateEnvironmentTemplate' {name} -> name) (\s@UpdateEnvironmentTemplate' {} a -> s {name = a} :: UpdateEnvironmentTemplate)

instance Core.AWSRequest UpdateEnvironmentTemplate where
  type
    AWSResponse UpdateEnvironmentTemplate =
      UpdateEnvironmentTemplateResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateEnvironmentTemplateResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "environmentTemplate")
      )

instance Prelude.Hashable UpdateEnvironmentTemplate where
  hashWithSalt _salt UpdateEnvironmentTemplate' {..} =
    _salt `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` displayName
      `Prelude.hashWithSalt` name

instance Prelude.NFData UpdateEnvironmentTemplate where
  rnf UpdateEnvironmentTemplate' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf displayName
      `Prelude.seq` Prelude.rnf name

instance Data.ToHeaders UpdateEnvironmentTemplate where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AwsProton20200720.UpdateEnvironmentTemplate" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateEnvironmentTemplate where
  toJSON UpdateEnvironmentTemplate' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("description" Data..=) Prelude.<$> description,
            ("displayName" Data..=) Prelude.<$> displayName,
            Prelude.Just ("name" Data..= name)
          ]
      )

instance Data.ToPath UpdateEnvironmentTemplate where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateEnvironmentTemplate where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateEnvironmentTemplateResponse' smart constructor.
data UpdateEnvironmentTemplateResponse = UpdateEnvironmentTemplateResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The environment template detail data that\'s returned by Proton.
    environmentTemplate :: EnvironmentTemplate
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateEnvironmentTemplateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateEnvironmentTemplateResponse_httpStatus' - The response's http status code.
--
-- 'environmentTemplate', 'updateEnvironmentTemplateResponse_environmentTemplate' - The environment template detail data that\'s returned by Proton.
newUpdateEnvironmentTemplateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'environmentTemplate'
  EnvironmentTemplate ->
  UpdateEnvironmentTemplateResponse
newUpdateEnvironmentTemplateResponse
  pHttpStatus_
  pEnvironmentTemplate_ =
    UpdateEnvironmentTemplateResponse'
      { httpStatus =
          pHttpStatus_,
        environmentTemplate =
          pEnvironmentTemplate_
      }

-- | The response's http status code.
updateEnvironmentTemplateResponse_httpStatus :: Lens.Lens' UpdateEnvironmentTemplateResponse Prelude.Int
updateEnvironmentTemplateResponse_httpStatus = Lens.lens (\UpdateEnvironmentTemplateResponse' {httpStatus} -> httpStatus) (\s@UpdateEnvironmentTemplateResponse' {} a -> s {httpStatus = a} :: UpdateEnvironmentTemplateResponse)

-- | The environment template detail data that\'s returned by Proton.
updateEnvironmentTemplateResponse_environmentTemplate :: Lens.Lens' UpdateEnvironmentTemplateResponse EnvironmentTemplate
updateEnvironmentTemplateResponse_environmentTemplate = Lens.lens (\UpdateEnvironmentTemplateResponse' {environmentTemplate} -> environmentTemplate) (\s@UpdateEnvironmentTemplateResponse' {} a -> s {environmentTemplate = a} :: UpdateEnvironmentTemplateResponse)

instance
  Prelude.NFData
    UpdateEnvironmentTemplateResponse
  where
  rnf UpdateEnvironmentTemplateResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf environmentTemplate
