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
-- Module      : Amazonka.APIGateway.GetStage
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a Stage resource.
module Amazonka.APIGateway.GetStage
  ( -- * Creating a Request
    GetStage (..),
    newGetStage,

    -- * Request Lenses
    getStage_restApiId,
    getStage_stageName,

    -- * Destructuring the Response
    Stage (..),
    newStage,

    -- * Response Lenses
    stage_accessLogSettings,
    stage_cacheClusterEnabled,
    stage_cacheClusterSize,
    stage_cacheClusterStatus,
    stage_canarySettings,
    stage_clientCertificateId,
    stage_createdDate,
    stage_deploymentId,
    stage_description,
    stage_documentationVersion,
    stage_lastUpdatedDate,
    stage_methodSettings,
    stage_stageName,
    stage_tags,
    stage_tracingEnabled,
    stage_variables,
    stage_webAclArn,
  )
where

import Amazonka.APIGateway.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Requests API Gateway to get information about a Stage resource.
--
-- /See:/ 'newGetStage' smart constructor.
data GetStage = GetStage'
  { -- | The string identifier of the associated RestApi.
    restApiId :: Prelude.Text,
    -- | The name of the Stage resource to get information about.
    stageName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetStage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'restApiId', 'getStage_restApiId' - The string identifier of the associated RestApi.
--
-- 'stageName', 'getStage_stageName' - The name of the Stage resource to get information about.
newGetStage ::
  -- | 'restApiId'
  Prelude.Text ->
  -- | 'stageName'
  Prelude.Text ->
  GetStage
newGetStage pRestApiId_ pStageName_ =
  GetStage'
    { restApiId = pRestApiId_,
      stageName = pStageName_
    }

-- | The string identifier of the associated RestApi.
getStage_restApiId :: Lens.Lens' GetStage Prelude.Text
getStage_restApiId = Lens.lens (\GetStage' {restApiId} -> restApiId) (\s@GetStage' {} a -> s {restApiId = a} :: GetStage)

-- | The name of the Stage resource to get information about.
getStage_stageName :: Lens.Lens' GetStage Prelude.Text
getStage_stageName = Lens.lens (\GetStage' {stageName} -> stageName) (\s@GetStage' {} a -> s {stageName = a} :: GetStage)

instance Core.AWSRequest GetStage where
  type AWSResponse GetStage = Stage
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Data.eitherParseJSON x)

instance Prelude.Hashable GetStage where
  hashWithSalt _salt GetStage' {..} =
    _salt
      `Prelude.hashWithSalt` restApiId
      `Prelude.hashWithSalt` stageName

instance Prelude.NFData GetStage where
  rnf GetStage' {..} =
    Prelude.rnf restApiId
      `Prelude.seq` Prelude.rnf stageName

instance Data.ToHeaders GetStage where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Data.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Data.ToPath GetStage where
  toPath GetStage' {..} =
    Prelude.mconcat
      [ "/restapis/",
        Data.toBS restApiId,
        "/stages/",
        Data.toBS stageName
      ]

instance Data.ToQuery GetStage where
  toQuery = Prelude.const Prelude.mempty
