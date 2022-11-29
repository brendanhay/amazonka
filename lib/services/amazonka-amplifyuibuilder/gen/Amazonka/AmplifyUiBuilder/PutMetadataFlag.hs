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
-- Module      : Amazonka.AmplifyUiBuilder.PutMetadataFlag
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stores the metadata information about a feature on a form or view.
module Amazonka.AmplifyUiBuilder.PutMetadataFlag
  ( -- * Creating a Request
    PutMetadataFlag (..),
    newPutMetadataFlag,

    -- * Request Lenses
    putMetadataFlag_appId,
    putMetadataFlag_body,
    putMetadataFlag_environmentName,
    putMetadataFlag_featureName,

    -- * Destructuring the Response
    PutMetadataFlagResponse (..),
    newPutMetadataFlagResponse,
  )
where

import Amazonka.AmplifyUiBuilder.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutMetadataFlag' smart constructor.
data PutMetadataFlag = PutMetadataFlag'
  { -- | The unique ID for the Amplify app.
    appId :: Prelude.Text,
    -- | The metadata information to store.
    body :: PutMetadataFlagBody,
    -- | The name of the backend environment that is part of the Amplify app.
    environmentName :: Prelude.Text,
    -- | The name of the feature associated with the metadata.
    featureName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutMetadataFlag' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appId', 'putMetadataFlag_appId' - The unique ID for the Amplify app.
--
-- 'body', 'putMetadataFlag_body' - The metadata information to store.
--
-- 'environmentName', 'putMetadataFlag_environmentName' - The name of the backend environment that is part of the Amplify app.
--
-- 'featureName', 'putMetadataFlag_featureName' - The name of the feature associated with the metadata.
newPutMetadataFlag ::
  -- | 'appId'
  Prelude.Text ->
  -- | 'body'
  PutMetadataFlagBody ->
  -- | 'environmentName'
  Prelude.Text ->
  -- | 'featureName'
  Prelude.Text ->
  PutMetadataFlag
newPutMetadataFlag
  pAppId_
  pBody_
  pEnvironmentName_
  pFeatureName_ =
    PutMetadataFlag'
      { appId = pAppId_,
        body = pBody_,
        environmentName = pEnvironmentName_,
        featureName = pFeatureName_
      }

-- | The unique ID for the Amplify app.
putMetadataFlag_appId :: Lens.Lens' PutMetadataFlag Prelude.Text
putMetadataFlag_appId = Lens.lens (\PutMetadataFlag' {appId} -> appId) (\s@PutMetadataFlag' {} a -> s {appId = a} :: PutMetadataFlag)

-- | The metadata information to store.
putMetadataFlag_body :: Lens.Lens' PutMetadataFlag PutMetadataFlagBody
putMetadataFlag_body = Lens.lens (\PutMetadataFlag' {body} -> body) (\s@PutMetadataFlag' {} a -> s {body = a} :: PutMetadataFlag)

-- | The name of the backend environment that is part of the Amplify app.
putMetadataFlag_environmentName :: Lens.Lens' PutMetadataFlag Prelude.Text
putMetadataFlag_environmentName = Lens.lens (\PutMetadataFlag' {environmentName} -> environmentName) (\s@PutMetadataFlag' {} a -> s {environmentName = a} :: PutMetadataFlag)

-- | The name of the feature associated with the metadata.
putMetadataFlag_featureName :: Lens.Lens' PutMetadataFlag Prelude.Text
putMetadataFlag_featureName = Lens.lens (\PutMetadataFlag' {featureName} -> featureName) (\s@PutMetadataFlag' {} a -> s {featureName = a} :: PutMetadataFlag)

instance Core.AWSRequest PutMetadataFlag where
  type
    AWSResponse PutMetadataFlag =
      PutMetadataFlagResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveNull PutMetadataFlagResponse'

instance Prelude.Hashable PutMetadataFlag where
  hashWithSalt _salt PutMetadataFlag' {..} =
    _salt `Prelude.hashWithSalt` appId
      `Prelude.hashWithSalt` body
      `Prelude.hashWithSalt` environmentName
      `Prelude.hashWithSalt` featureName

instance Prelude.NFData PutMetadataFlag where
  rnf PutMetadataFlag' {..} =
    Prelude.rnf appId
      `Prelude.seq` Prelude.rnf body
      `Prelude.seq` Prelude.rnf environmentName
      `Prelude.seq` Prelude.rnf featureName

instance Core.ToHeaders PutMetadataFlag where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON PutMetadataFlag where
  toJSON PutMetadataFlag' {..} = Core.toJSON body

instance Core.ToPath PutMetadataFlag where
  toPath PutMetadataFlag' {..} =
    Prelude.mconcat
      [ "/app/",
        Core.toBS appId,
        "/environment/",
        Core.toBS environmentName,
        "/metadata/features/",
        Core.toBS featureName
      ]

instance Core.ToQuery PutMetadataFlag where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutMetadataFlagResponse' smart constructor.
data PutMetadataFlagResponse = PutMetadataFlagResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutMetadataFlagResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newPutMetadataFlagResponse ::
  PutMetadataFlagResponse
newPutMetadataFlagResponse = PutMetadataFlagResponse'

instance Prelude.NFData PutMetadataFlagResponse where
  rnf _ = ()
