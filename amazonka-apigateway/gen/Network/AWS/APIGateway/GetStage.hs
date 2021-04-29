{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.APIGateway.GetStage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a Stage resource.
module Network.AWS.APIGateway.GetStage
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
    stage_deploymentId,
    stage_createdDate,
    stage_tracingEnabled,
    stage_webAclArn,
    stage_lastUpdatedDate,
    stage_cacheClusterEnabled,
    stage_stageName,
    stage_documentationVersion,
    stage_variables,
    stage_accessLogSettings,
    stage_tags,
    stage_clientCertificateId,
    stage_description,
    stage_canarySettings,
    stage_cacheClusterSize,
    stage_methodSettings,
    stage_cacheClusterStatus,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Requests API Gateway to get information about a Stage resource.
--
-- /See:/ 'newGetStage' smart constructor.
data GetStage = GetStage'
  { -- | [Required] The string identifier of the associated RestApi.
    restApiId :: Prelude.Text,
    -- | [Required] The name of the Stage resource to get information about.
    stageName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetStage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'restApiId', 'getStage_restApiId' - [Required] The string identifier of the associated RestApi.
--
-- 'stageName', 'getStage_stageName' - [Required] The name of the Stage resource to get information about.
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

-- | [Required] The string identifier of the associated RestApi.
getStage_restApiId :: Lens.Lens' GetStage Prelude.Text
getStage_restApiId = Lens.lens (\GetStage' {restApiId} -> restApiId) (\s@GetStage' {} a -> s {restApiId = a} :: GetStage)

-- | [Required] The name of the Stage resource to get information about.
getStage_stageName :: Lens.Lens' GetStage Prelude.Text
getStage_stageName = Lens.lens (\GetStage' {stageName} -> stageName) (\s@GetStage' {} a -> s {stageName = a} :: GetStage)

instance Prelude.AWSRequest GetStage where
  type Rs GetStage = Stage
  request = Request.get defaultService
  response =
    Response.receiveJSON
      (\s h x -> Prelude.eitherParseJSON x)

instance Prelude.Hashable GetStage

instance Prelude.NFData GetStage

instance Prelude.ToHeaders GetStage where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Prelude.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Prelude.ToPath GetStage where
  toPath GetStage' {..} =
    Prelude.mconcat
      [ "/restapis/",
        Prelude.toBS restApiId,
        "/stages/",
        Prelude.toBS stageName
      ]

instance Prelude.ToQuery GetStage where
  toQuery = Prelude.const Prelude.mempty
