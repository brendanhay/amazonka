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
-- Module      : Network.AWS.Lambda.GetLayerVersion
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a version of an
-- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-layers.html Lambda layer>,
-- with a link to download the layer archive that\'s valid for 10 minutes.
module Network.AWS.Lambda.GetLayerVersion
  ( -- * Creating a Request
    GetLayerVersion (..),
    newGetLayerVersion,

    -- * Request Lenses
    getLayerVersion_layerName,
    getLayerVersion_versionNumber,

    -- * Destructuring the Response
    GetLayerVersionResponse (..),
    newGetLayerVersionResponse,

    -- * Response Lenses
    getLayerVersionResponse_layerVersionArn,
    getLayerVersionResponse_content,
    getLayerVersionResponse_createdDate,
    getLayerVersionResponse_version,
    getLayerVersionResponse_licenseInfo,
    getLayerVersionResponse_compatibleArchitectures,
    getLayerVersionResponse_layerArn,
    getLayerVersionResponse_description,
    getLayerVersionResponse_compatibleRuntimes,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Lambda.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetLayerVersion' smart constructor.
data GetLayerVersion = GetLayerVersion'
  { -- | The name or Amazon Resource Name (ARN) of the layer.
    layerName :: Prelude.Text,
    -- | The version number.
    versionNumber :: Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetLayerVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'layerName', 'getLayerVersion_layerName' - The name or Amazon Resource Name (ARN) of the layer.
--
-- 'versionNumber', 'getLayerVersion_versionNumber' - The version number.
newGetLayerVersion ::
  -- | 'layerName'
  Prelude.Text ->
  -- | 'versionNumber'
  Prelude.Integer ->
  GetLayerVersion
newGetLayerVersion pLayerName_ pVersionNumber_ =
  GetLayerVersion'
    { layerName = pLayerName_,
      versionNumber = pVersionNumber_
    }

-- | The name or Amazon Resource Name (ARN) of the layer.
getLayerVersion_layerName :: Lens.Lens' GetLayerVersion Prelude.Text
getLayerVersion_layerName = Lens.lens (\GetLayerVersion' {layerName} -> layerName) (\s@GetLayerVersion' {} a -> s {layerName = a} :: GetLayerVersion)

-- | The version number.
getLayerVersion_versionNumber :: Lens.Lens' GetLayerVersion Prelude.Integer
getLayerVersion_versionNumber = Lens.lens (\GetLayerVersion' {versionNumber} -> versionNumber) (\s@GetLayerVersion' {} a -> s {versionNumber = a} :: GetLayerVersion)

instance Core.AWSRequest GetLayerVersion where
  type
    AWSResponse GetLayerVersion =
      GetLayerVersionResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      (\s h x -> Core.eitherParseJSON x)

instance Prelude.Hashable GetLayerVersion

instance Prelude.NFData GetLayerVersion

instance Core.ToHeaders GetLayerVersion where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath GetLayerVersion where
  toPath GetLayerVersion' {..} =
    Prelude.mconcat
      [ "/2018-10-31/layers/",
        Core.toBS layerName,
        "/versions/",
        Core.toBS versionNumber
      ]

instance Core.ToQuery GetLayerVersion where
  toQuery = Prelude.const Prelude.mempty
