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
-- Module      : Amazonka.AmplifyUiBuilder.ExportForms
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Exports form configurations to code that is ready to integrate into an
-- Amplify app.
--
-- This operation returns paginated results.
module Amazonka.AmplifyUiBuilder.ExportForms
  ( -- * Creating a Request
    ExportForms (..),
    newExportForms,

    -- * Request Lenses
    exportForms_nextToken,
    exportForms_appId,
    exportForms_environmentName,

    -- * Destructuring the Response
    ExportFormsResponse (..),
    newExportFormsResponse,

    -- * Response Lenses
    exportFormsResponse_nextToken,
    exportFormsResponse_httpStatus,
    exportFormsResponse_entities,
  )
where

import Amazonka.AmplifyUiBuilder.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newExportForms' smart constructor.
data ExportForms = ExportForms'
  { -- | The token to request the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The unique ID of the Amplify app to export forms to.
    appId :: Prelude.Text,
    -- | The name of the backend environment that is a part of the Amplify app.
    environmentName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExportForms' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'exportForms_nextToken' - The token to request the next page of results.
--
-- 'appId', 'exportForms_appId' - The unique ID of the Amplify app to export forms to.
--
-- 'environmentName', 'exportForms_environmentName' - The name of the backend environment that is a part of the Amplify app.
newExportForms ::
  -- | 'appId'
  Prelude.Text ->
  -- | 'environmentName'
  Prelude.Text ->
  ExportForms
newExportForms pAppId_ pEnvironmentName_ =
  ExportForms'
    { nextToken = Prelude.Nothing,
      appId = pAppId_,
      environmentName = pEnvironmentName_
    }

-- | The token to request the next page of results.
exportForms_nextToken :: Lens.Lens' ExportForms (Prelude.Maybe Prelude.Text)
exportForms_nextToken = Lens.lens (\ExportForms' {nextToken} -> nextToken) (\s@ExportForms' {} a -> s {nextToken = a} :: ExportForms)

-- | The unique ID of the Amplify app to export forms to.
exportForms_appId :: Lens.Lens' ExportForms Prelude.Text
exportForms_appId = Lens.lens (\ExportForms' {appId} -> appId) (\s@ExportForms' {} a -> s {appId = a} :: ExportForms)

-- | The name of the backend environment that is a part of the Amplify app.
exportForms_environmentName :: Lens.Lens' ExportForms Prelude.Text
exportForms_environmentName = Lens.lens (\ExportForms' {environmentName} -> environmentName) (\s@ExportForms' {} a -> s {environmentName = a} :: ExportForms)

instance Core.AWSPager ExportForms where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? exportFormsResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop (rs Lens.^. exportFormsResponse_entities) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& exportForms_nextToken
          Lens..~ rs
          Lens.^? exportFormsResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ExportForms where
  type AWSResponse ExportForms = ExportFormsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ExportFormsResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..?> "entities" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable ExportForms where
  hashWithSalt _salt ExportForms' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` appId
      `Prelude.hashWithSalt` environmentName

instance Prelude.NFData ExportForms where
  rnf ExportForms' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf appId
      `Prelude.seq` Prelude.rnf environmentName

instance Core.ToHeaders ExportForms where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath ExportForms where
  toPath ExportForms' {..} =
    Prelude.mconcat
      [ "/export/app/",
        Core.toBS appId,
        "/environment/",
        Core.toBS environmentName,
        "/forms"
      ]

instance Core.ToQuery ExportForms where
  toQuery ExportForms' {..} =
    Prelude.mconcat ["nextToken" Core.=: nextToken]

-- | /See:/ 'newExportFormsResponse' smart constructor.
data ExportFormsResponse = ExportFormsResponse'
  { -- | The pagination token that\'s included if more results are available.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Represents the configuration of the exported forms.
    entities :: [Form]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExportFormsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'exportFormsResponse_nextToken' - The pagination token that\'s included if more results are available.
--
-- 'httpStatus', 'exportFormsResponse_httpStatus' - The response's http status code.
--
-- 'entities', 'exportFormsResponse_entities' - Represents the configuration of the exported forms.
newExportFormsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ExportFormsResponse
newExportFormsResponse pHttpStatus_ =
  ExportFormsResponse'
    { nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      entities = Prelude.mempty
    }

-- | The pagination token that\'s included if more results are available.
exportFormsResponse_nextToken :: Lens.Lens' ExportFormsResponse (Prelude.Maybe Prelude.Text)
exportFormsResponse_nextToken = Lens.lens (\ExportFormsResponse' {nextToken} -> nextToken) (\s@ExportFormsResponse' {} a -> s {nextToken = a} :: ExportFormsResponse)

-- | The response's http status code.
exportFormsResponse_httpStatus :: Lens.Lens' ExportFormsResponse Prelude.Int
exportFormsResponse_httpStatus = Lens.lens (\ExportFormsResponse' {httpStatus} -> httpStatus) (\s@ExportFormsResponse' {} a -> s {httpStatus = a} :: ExportFormsResponse)

-- | Represents the configuration of the exported forms.
exportFormsResponse_entities :: Lens.Lens' ExportFormsResponse [Form]
exportFormsResponse_entities = Lens.lens (\ExportFormsResponse' {entities} -> entities) (\s@ExportFormsResponse' {} a -> s {entities = a} :: ExportFormsResponse) Prelude.. Lens.coerced

instance Prelude.NFData ExportFormsResponse where
  rnf ExportFormsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf entities
