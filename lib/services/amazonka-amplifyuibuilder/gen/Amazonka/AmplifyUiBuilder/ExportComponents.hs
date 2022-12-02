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
-- Module      : Amazonka.AmplifyUiBuilder.ExportComponents
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Exports component configurations to code that is ready to integrate into
-- an Amplify app.
--
-- This operation returns paginated results.
module Amazonka.AmplifyUiBuilder.ExportComponents
  ( -- * Creating a Request
    ExportComponents (..),
    newExportComponents,

    -- * Request Lenses
    exportComponents_nextToken,
    exportComponents_appId,
    exportComponents_environmentName,

    -- * Destructuring the Response
    ExportComponentsResponse (..),
    newExportComponentsResponse,

    -- * Response Lenses
    exportComponentsResponse_nextToken,
    exportComponentsResponse_httpStatus,
    exportComponentsResponse_entities,
  )
where

import Amazonka.AmplifyUiBuilder.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newExportComponents' smart constructor.
data ExportComponents = ExportComponents'
  { -- | The token to request the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The unique ID of the Amplify app to export components to.
    appId :: Prelude.Text,
    -- | The name of the backend environment that is a part of the Amplify app.
    environmentName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExportComponents' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'exportComponents_nextToken' - The token to request the next page of results.
--
-- 'appId', 'exportComponents_appId' - The unique ID of the Amplify app to export components to.
--
-- 'environmentName', 'exportComponents_environmentName' - The name of the backend environment that is a part of the Amplify app.
newExportComponents ::
  -- | 'appId'
  Prelude.Text ->
  -- | 'environmentName'
  Prelude.Text ->
  ExportComponents
newExportComponents pAppId_ pEnvironmentName_ =
  ExportComponents'
    { nextToken = Prelude.Nothing,
      appId = pAppId_,
      environmentName = pEnvironmentName_
    }

-- | The token to request the next page of results.
exportComponents_nextToken :: Lens.Lens' ExportComponents (Prelude.Maybe Prelude.Text)
exportComponents_nextToken = Lens.lens (\ExportComponents' {nextToken} -> nextToken) (\s@ExportComponents' {} a -> s {nextToken = a} :: ExportComponents)

-- | The unique ID of the Amplify app to export components to.
exportComponents_appId :: Lens.Lens' ExportComponents Prelude.Text
exportComponents_appId = Lens.lens (\ExportComponents' {appId} -> appId) (\s@ExportComponents' {} a -> s {appId = a} :: ExportComponents)

-- | The name of the backend environment that is a part of the Amplify app.
exportComponents_environmentName :: Lens.Lens' ExportComponents Prelude.Text
exportComponents_environmentName = Lens.lens (\ExportComponents' {environmentName} -> environmentName) (\s@ExportComponents' {} a -> s {environmentName = a} :: ExportComponents)

instance Core.AWSPager ExportComponents where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? exportComponentsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        (rs Lens.^. exportComponentsResponse_entities) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& exportComponents_nextToken
          Lens..~ rs
          Lens.^? exportComponentsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ExportComponents where
  type
    AWSResponse ExportComponents =
      ExportComponentsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ExportComponentsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "entities" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable ExportComponents where
  hashWithSalt _salt ExportComponents' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` appId
      `Prelude.hashWithSalt` environmentName

instance Prelude.NFData ExportComponents where
  rnf ExportComponents' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf appId
      `Prelude.seq` Prelude.rnf environmentName

instance Data.ToHeaders ExportComponents where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ExportComponents where
  toPath ExportComponents' {..} =
    Prelude.mconcat
      [ "/export/app/",
        Data.toBS appId,
        "/environment/",
        Data.toBS environmentName,
        "/components"
      ]

instance Data.ToQuery ExportComponents where
  toQuery ExportComponents' {..} =
    Prelude.mconcat ["nextToken" Data.=: nextToken]

-- | /See:/ 'newExportComponentsResponse' smart constructor.
data ExportComponentsResponse = ExportComponentsResponse'
  { -- | The pagination token that\'s included if more results are available.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Represents the configuration of the exported components.
    entities :: [Component]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExportComponentsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'exportComponentsResponse_nextToken' - The pagination token that\'s included if more results are available.
--
-- 'httpStatus', 'exportComponentsResponse_httpStatus' - The response's http status code.
--
-- 'entities', 'exportComponentsResponse_entities' - Represents the configuration of the exported components.
newExportComponentsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ExportComponentsResponse
newExportComponentsResponse pHttpStatus_ =
  ExportComponentsResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      entities = Prelude.mempty
    }

-- | The pagination token that\'s included if more results are available.
exportComponentsResponse_nextToken :: Lens.Lens' ExportComponentsResponse (Prelude.Maybe Prelude.Text)
exportComponentsResponse_nextToken = Lens.lens (\ExportComponentsResponse' {nextToken} -> nextToken) (\s@ExportComponentsResponse' {} a -> s {nextToken = a} :: ExportComponentsResponse)

-- | The response's http status code.
exportComponentsResponse_httpStatus :: Lens.Lens' ExportComponentsResponse Prelude.Int
exportComponentsResponse_httpStatus = Lens.lens (\ExportComponentsResponse' {httpStatus} -> httpStatus) (\s@ExportComponentsResponse' {} a -> s {httpStatus = a} :: ExportComponentsResponse)

-- | Represents the configuration of the exported components.
exportComponentsResponse_entities :: Lens.Lens' ExportComponentsResponse [Component]
exportComponentsResponse_entities = Lens.lens (\ExportComponentsResponse' {entities} -> entities) (\s@ExportComponentsResponse' {} a -> s {entities = a} :: ExportComponentsResponse) Prelude.. Lens.coerced

instance Prelude.NFData ExportComponentsResponse where
  rnf ExportComponentsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf entities
