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
-- Module      : Amazonka.RobOMaker.UpdateWorldTemplate
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a world template.
module Amazonka.RobOMaker.UpdateWorldTemplate
  ( -- * Creating a Request
    UpdateWorldTemplate (..),
    newUpdateWorldTemplate,

    -- * Request Lenses
    updateWorldTemplate_name,
    updateWorldTemplate_templateBody,
    updateWorldTemplate_templateLocation,
    updateWorldTemplate_template,

    -- * Destructuring the Response
    UpdateWorldTemplateResponse (..),
    newUpdateWorldTemplateResponse,

    -- * Response Lenses
    updateWorldTemplateResponse_name,
    updateWorldTemplateResponse_lastUpdatedAt,
    updateWorldTemplateResponse_arn,
    updateWorldTemplateResponse_createdAt,
    updateWorldTemplateResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.RobOMaker.Types

-- | /See:/ 'newUpdateWorldTemplate' smart constructor.
data UpdateWorldTemplate = UpdateWorldTemplate'
  { -- | The name of the template.
    name :: Prelude.Maybe Prelude.Text,
    -- | The world template body.
    templateBody :: Prelude.Maybe Prelude.Text,
    -- | The location of the world template.
    templateLocation :: Prelude.Maybe TemplateLocation,
    -- | The Amazon Resource Name (arn) of the world template to update.
    template :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateWorldTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'updateWorldTemplate_name' - The name of the template.
--
-- 'templateBody', 'updateWorldTemplate_templateBody' - The world template body.
--
-- 'templateLocation', 'updateWorldTemplate_templateLocation' - The location of the world template.
--
-- 'template', 'updateWorldTemplate_template' - The Amazon Resource Name (arn) of the world template to update.
newUpdateWorldTemplate ::
  -- | 'template'
  Prelude.Text ->
  UpdateWorldTemplate
newUpdateWorldTemplate pTemplate_ =
  UpdateWorldTemplate'
    { name = Prelude.Nothing,
      templateBody = Prelude.Nothing,
      templateLocation = Prelude.Nothing,
      template = pTemplate_
    }

-- | The name of the template.
updateWorldTemplate_name :: Lens.Lens' UpdateWorldTemplate (Prelude.Maybe Prelude.Text)
updateWorldTemplate_name = Lens.lens (\UpdateWorldTemplate' {name} -> name) (\s@UpdateWorldTemplate' {} a -> s {name = a} :: UpdateWorldTemplate)

-- | The world template body.
updateWorldTemplate_templateBody :: Lens.Lens' UpdateWorldTemplate (Prelude.Maybe Prelude.Text)
updateWorldTemplate_templateBody = Lens.lens (\UpdateWorldTemplate' {templateBody} -> templateBody) (\s@UpdateWorldTemplate' {} a -> s {templateBody = a} :: UpdateWorldTemplate)

-- | The location of the world template.
updateWorldTemplate_templateLocation :: Lens.Lens' UpdateWorldTemplate (Prelude.Maybe TemplateLocation)
updateWorldTemplate_templateLocation = Lens.lens (\UpdateWorldTemplate' {templateLocation} -> templateLocation) (\s@UpdateWorldTemplate' {} a -> s {templateLocation = a} :: UpdateWorldTemplate)

-- | The Amazon Resource Name (arn) of the world template to update.
updateWorldTemplate_template :: Lens.Lens' UpdateWorldTemplate Prelude.Text
updateWorldTemplate_template = Lens.lens (\UpdateWorldTemplate' {template} -> template) (\s@UpdateWorldTemplate' {} a -> s {template = a} :: UpdateWorldTemplate)

instance Core.AWSRequest UpdateWorldTemplate where
  type
    AWSResponse UpdateWorldTemplate =
      UpdateWorldTemplateResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateWorldTemplateResponse'
            Prelude.<$> (x Core..?> "name")
            Prelude.<*> (x Core..?> "lastUpdatedAt")
            Prelude.<*> (x Core..?> "arn")
            Prelude.<*> (x Core..?> "createdAt")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateWorldTemplate where
  hashWithSalt _salt UpdateWorldTemplate' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` templateBody
      `Prelude.hashWithSalt` templateLocation
      `Prelude.hashWithSalt` template

instance Prelude.NFData UpdateWorldTemplate where
  rnf UpdateWorldTemplate' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf templateBody
      `Prelude.seq` Prelude.rnf templateLocation
      `Prelude.seq` Prelude.rnf template

instance Core.ToHeaders UpdateWorldTemplate where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateWorldTemplate where
  toJSON UpdateWorldTemplate' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("name" Core..=) Prelude.<$> name,
            ("templateBody" Core..=) Prelude.<$> templateBody,
            ("templateLocation" Core..=)
              Prelude.<$> templateLocation,
            Prelude.Just ("template" Core..= template)
          ]
      )

instance Core.ToPath UpdateWorldTemplate where
  toPath = Prelude.const "/updateWorldTemplate"

instance Core.ToQuery UpdateWorldTemplate where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateWorldTemplateResponse' smart constructor.
data UpdateWorldTemplateResponse = UpdateWorldTemplateResponse'
  { -- | The name of the world template.
    name :: Prelude.Maybe Prelude.Text,
    -- | The time, in milliseconds since the epoch, when the world template was
    -- last updated.
    lastUpdatedAt :: Prelude.Maybe Core.POSIX,
    -- | The Amazon Resource Name (arn) of the world template.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The time, in milliseconds since the epoch, when the world template was
    -- created.
    createdAt :: Prelude.Maybe Core.POSIX,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateWorldTemplateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'updateWorldTemplateResponse_name' - The name of the world template.
--
-- 'lastUpdatedAt', 'updateWorldTemplateResponse_lastUpdatedAt' - The time, in milliseconds since the epoch, when the world template was
-- last updated.
--
-- 'arn', 'updateWorldTemplateResponse_arn' - The Amazon Resource Name (arn) of the world template.
--
-- 'createdAt', 'updateWorldTemplateResponse_createdAt' - The time, in milliseconds since the epoch, when the world template was
-- created.
--
-- 'httpStatus', 'updateWorldTemplateResponse_httpStatus' - The response's http status code.
newUpdateWorldTemplateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateWorldTemplateResponse
newUpdateWorldTemplateResponse pHttpStatus_ =
  UpdateWorldTemplateResponse'
    { name =
        Prelude.Nothing,
      lastUpdatedAt = Prelude.Nothing,
      arn = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The name of the world template.
updateWorldTemplateResponse_name :: Lens.Lens' UpdateWorldTemplateResponse (Prelude.Maybe Prelude.Text)
updateWorldTemplateResponse_name = Lens.lens (\UpdateWorldTemplateResponse' {name} -> name) (\s@UpdateWorldTemplateResponse' {} a -> s {name = a} :: UpdateWorldTemplateResponse)

-- | The time, in milliseconds since the epoch, when the world template was
-- last updated.
updateWorldTemplateResponse_lastUpdatedAt :: Lens.Lens' UpdateWorldTemplateResponse (Prelude.Maybe Prelude.UTCTime)
updateWorldTemplateResponse_lastUpdatedAt = Lens.lens (\UpdateWorldTemplateResponse' {lastUpdatedAt} -> lastUpdatedAt) (\s@UpdateWorldTemplateResponse' {} a -> s {lastUpdatedAt = a} :: UpdateWorldTemplateResponse) Prelude.. Lens.mapping Core._Time

-- | The Amazon Resource Name (arn) of the world template.
updateWorldTemplateResponse_arn :: Lens.Lens' UpdateWorldTemplateResponse (Prelude.Maybe Prelude.Text)
updateWorldTemplateResponse_arn = Lens.lens (\UpdateWorldTemplateResponse' {arn} -> arn) (\s@UpdateWorldTemplateResponse' {} a -> s {arn = a} :: UpdateWorldTemplateResponse)

-- | The time, in milliseconds since the epoch, when the world template was
-- created.
updateWorldTemplateResponse_createdAt :: Lens.Lens' UpdateWorldTemplateResponse (Prelude.Maybe Prelude.UTCTime)
updateWorldTemplateResponse_createdAt = Lens.lens (\UpdateWorldTemplateResponse' {createdAt} -> createdAt) (\s@UpdateWorldTemplateResponse' {} a -> s {createdAt = a} :: UpdateWorldTemplateResponse) Prelude.. Lens.mapping Core._Time

-- | The response's http status code.
updateWorldTemplateResponse_httpStatus :: Lens.Lens' UpdateWorldTemplateResponse Prelude.Int
updateWorldTemplateResponse_httpStatus = Lens.lens (\UpdateWorldTemplateResponse' {httpStatus} -> httpStatus) (\s@UpdateWorldTemplateResponse' {} a -> s {httpStatus = a} :: UpdateWorldTemplateResponse)

instance Prelude.NFData UpdateWorldTemplateResponse where
  rnf UpdateWorldTemplateResponse' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf lastUpdatedAt
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf httpStatus
