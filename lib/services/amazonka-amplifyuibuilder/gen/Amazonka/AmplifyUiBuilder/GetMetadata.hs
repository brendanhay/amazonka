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
-- Module      : Amazonka.AmplifyUiBuilder.GetMetadata
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns existing metadata for an Amplify app.
module Amazonka.AmplifyUiBuilder.GetMetadata
  ( -- * Creating a Request
    GetMetadata (..),
    newGetMetadata,

    -- * Request Lenses
    getMetadata_appId,
    getMetadata_environmentName,

    -- * Destructuring the Response
    GetMetadataResponse (..),
    newGetMetadataResponse,

    -- * Response Lenses
    getMetadataResponse_httpStatus,
    getMetadataResponse_features,
  )
where

import Amazonka.AmplifyUiBuilder.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetMetadata' smart constructor.
data GetMetadata = GetMetadata'
  { -- | The unique ID of the Amplify app.
    appId :: Prelude.Text,
    -- | The name of the backend environment that is part of the Amplify app.
    environmentName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appId', 'getMetadata_appId' - The unique ID of the Amplify app.
--
-- 'environmentName', 'getMetadata_environmentName' - The name of the backend environment that is part of the Amplify app.
newGetMetadata ::
  -- | 'appId'
  Prelude.Text ->
  -- | 'environmentName'
  Prelude.Text ->
  GetMetadata
newGetMetadata pAppId_ pEnvironmentName_ =
  GetMetadata'
    { appId = pAppId_,
      environmentName = pEnvironmentName_
    }

-- | The unique ID of the Amplify app.
getMetadata_appId :: Lens.Lens' GetMetadata Prelude.Text
getMetadata_appId = Lens.lens (\GetMetadata' {appId} -> appId) (\s@GetMetadata' {} a -> s {appId = a} :: GetMetadata)

-- | The name of the backend environment that is part of the Amplify app.
getMetadata_environmentName :: Lens.Lens' GetMetadata Prelude.Text
getMetadata_environmentName = Lens.lens (\GetMetadata' {environmentName} -> environmentName) (\s@GetMetadata' {} a -> s {environmentName = a} :: GetMetadata)

instance Core.AWSRequest GetMetadata where
  type AWSResponse GetMetadata = GetMetadataResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetMetadataResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "features" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable GetMetadata where
  hashWithSalt _salt GetMetadata' {..} =
    _salt `Prelude.hashWithSalt` appId
      `Prelude.hashWithSalt` environmentName

instance Prelude.NFData GetMetadata where
  rnf GetMetadata' {..} =
    Prelude.rnf appId
      `Prelude.seq` Prelude.rnf environmentName

instance Data.ToHeaders GetMetadata where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetMetadata where
  toPath GetMetadata' {..} =
    Prelude.mconcat
      [ "/app/",
        Data.toBS appId,
        "/environment/",
        Data.toBS environmentName,
        "/metadata"
      ]

instance Data.ToQuery GetMetadata where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetMetadataResponse' smart constructor.
data GetMetadataResponse = GetMetadataResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Represents the configuration settings for the features metadata.
    features :: Prelude.HashMap Prelude.Text Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetMetadataResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getMetadataResponse_httpStatus' - The response's http status code.
--
-- 'features', 'getMetadataResponse_features' - Represents the configuration settings for the features metadata.
newGetMetadataResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetMetadataResponse
newGetMetadataResponse pHttpStatus_ =
  GetMetadataResponse'
    { httpStatus = pHttpStatus_,
      features = Prelude.mempty
    }

-- | The response's http status code.
getMetadataResponse_httpStatus :: Lens.Lens' GetMetadataResponse Prelude.Int
getMetadataResponse_httpStatus = Lens.lens (\GetMetadataResponse' {httpStatus} -> httpStatus) (\s@GetMetadataResponse' {} a -> s {httpStatus = a} :: GetMetadataResponse)

-- | Represents the configuration settings for the features metadata.
getMetadataResponse_features :: Lens.Lens' GetMetadataResponse (Prelude.HashMap Prelude.Text Prelude.Text)
getMetadataResponse_features = Lens.lens (\GetMetadataResponse' {features} -> features) (\s@GetMetadataResponse' {} a -> s {features = a} :: GetMetadataResponse) Prelude.. Lens.coerced

instance Prelude.NFData GetMetadataResponse where
  rnf GetMetadataResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf features
