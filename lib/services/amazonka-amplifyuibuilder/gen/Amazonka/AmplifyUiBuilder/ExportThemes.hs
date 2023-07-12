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
-- Module      : Amazonka.AmplifyUiBuilder.ExportThemes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Exports theme configurations to code that is ready to integrate into an
-- Amplify app.
--
-- This operation returns paginated results.
module Amazonka.AmplifyUiBuilder.ExportThemes
  ( -- * Creating a Request
    ExportThemes (..),
    newExportThemes,

    -- * Request Lenses
    exportThemes_nextToken,
    exportThemes_appId,
    exportThemes_environmentName,

    -- * Destructuring the Response
    ExportThemesResponse (..),
    newExportThemesResponse,

    -- * Response Lenses
    exportThemesResponse_nextToken,
    exportThemesResponse_httpStatus,
    exportThemesResponse_entities,
  )
where

import Amazonka.AmplifyUiBuilder.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newExportThemes' smart constructor.
data ExportThemes = ExportThemes'
  { -- | The token to request the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The unique ID of the Amplify app to export the themes to.
    appId :: Prelude.Text,
    -- | The name of the backend environment that is part of the Amplify app.
    environmentName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExportThemes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'exportThemes_nextToken' - The token to request the next page of results.
--
-- 'appId', 'exportThemes_appId' - The unique ID of the Amplify app to export the themes to.
--
-- 'environmentName', 'exportThemes_environmentName' - The name of the backend environment that is part of the Amplify app.
newExportThemes ::
  -- | 'appId'
  Prelude.Text ->
  -- | 'environmentName'
  Prelude.Text ->
  ExportThemes
newExportThemes pAppId_ pEnvironmentName_ =
  ExportThemes'
    { nextToken = Prelude.Nothing,
      appId = pAppId_,
      environmentName = pEnvironmentName_
    }

-- | The token to request the next page of results.
exportThemes_nextToken :: Lens.Lens' ExportThemes (Prelude.Maybe Prelude.Text)
exportThemes_nextToken = Lens.lens (\ExportThemes' {nextToken} -> nextToken) (\s@ExportThemes' {} a -> s {nextToken = a} :: ExportThemes)

-- | The unique ID of the Amplify app to export the themes to.
exportThemes_appId :: Lens.Lens' ExportThemes Prelude.Text
exportThemes_appId = Lens.lens (\ExportThemes' {appId} -> appId) (\s@ExportThemes' {} a -> s {appId = a} :: ExportThemes)

-- | The name of the backend environment that is part of the Amplify app.
exportThemes_environmentName :: Lens.Lens' ExportThemes Prelude.Text
exportThemes_environmentName = Lens.lens (\ExportThemes' {environmentName} -> environmentName) (\s@ExportThemes' {} a -> s {environmentName = a} :: ExportThemes)

instance Core.AWSPager ExportThemes where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? exportThemesResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        (rs Lens.^. exportThemesResponse_entities) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& exportThemes_nextToken
          Lens..~ rs
          Lens.^? exportThemesResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ExportThemes where
  type AWSResponse ExportThemes = ExportThemesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ExportThemesResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "entities" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable ExportThemes where
  hashWithSalt _salt ExportThemes' {..} =
    _salt
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` appId
      `Prelude.hashWithSalt` environmentName

instance Prelude.NFData ExportThemes where
  rnf ExportThemes' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf appId
      `Prelude.seq` Prelude.rnf environmentName

instance Data.ToHeaders ExportThemes where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ExportThemes where
  toPath ExportThemes' {..} =
    Prelude.mconcat
      [ "/export/app/",
        Data.toBS appId,
        "/environment/",
        Data.toBS environmentName,
        "/themes"
      ]

instance Data.ToQuery ExportThemes where
  toQuery ExportThemes' {..} =
    Prelude.mconcat ["nextToken" Data.=: nextToken]

-- | /See:/ 'newExportThemesResponse' smart constructor.
data ExportThemesResponse = ExportThemesResponse'
  { -- | The pagination token that\'s included if more results are available.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Represents the configuration of the exported themes.
    entities :: [Theme]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExportThemesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'exportThemesResponse_nextToken' - The pagination token that\'s included if more results are available.
--
-- 'httpStatus', 'exportThemesResponse_httpStatus' - The response's http status code.
--
-- 'entities', 'exportThemesResponse_entities' - Represents the configuration of the exported themes.
newExportThemesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ExportThemesResponse
newExportThemesResponse pHttpStatus_ =
  ExportThemesResponse'
    { nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      entities = Prelude.mempty
    }

-- | The pagination token that\'s included if more results are available.
exportThemesResponse_nextToken :: Lens.Lens' ExportThemesResponse (Prelude.Maybe Prelude.Text)
exportThemesResponse_nextToken = Lens.lens (\ExportThemesResponse' {nextToken} -> nextToken) (\s@ExportThemesResponse' {} a -> s {nextToken = a} :: ExportThemesResponse)

-- | The response's http status code.
exportThemesResponse_httpStatus :: Lens.Lens' ExportThemesResponse Prelude.Int
exportThemesResponse_httpStatus = Lens.lens (\ExportThemesResponse' {httpStatus} -> httpStatus) (\s@ExportThemesResponse' {} a -> s {httpStatus = a} :: ExportThemesResponse)

-- | Represents the configuration of the exported themes.
exportThemesResponse_entities :: Lens.Lens' ExportThemesResponse [Theme]
exportThemesResponse_entities = Lens.lens (\ExportThemesResponse' {entities} -> entities) (\s@ExportThemesResponse' {} a -> s {entities = a} :: ExportThemesResponse) Prelude.. Lens.coerced

instance Prelude.NFData ExportThemesResponse where
  rnf ExportThemesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf entities
