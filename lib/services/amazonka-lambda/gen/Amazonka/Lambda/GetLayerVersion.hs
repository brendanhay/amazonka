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
-- Module      : Amazonka.Lambda.GetLayerVersion
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a version of an
-- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-layers.html Lambda layer>,
-- with a link to download the layer archive that\'s valid for 10 minutes.
module Amazonka.Lambda.GetLayerVersion
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
    getLayerVersionResponse_compatibleArchitectures,
    getLayerVersionResponse_compatibleRuntimes,
    getLayerVersionResponse_content,
    getLayerVersionResponse_createdDate,
    getLayerVersionResponse_description,
    getLayerVersionResponse_layerArn,
    getLayerVersionResponse_layerVersionArn,
    getLayerVersionResponse_licenseInfo,
    getLayerVersionResponse_version,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lambda.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Data.eitherParseJSON x)

instance Prelude.Hashable GetLayerVersion where
  hashWithSalt _salt GetLayerVersion' {..} =
    _salt `Prelude.hashWithSalt` layerName
      `Prelude.hashWithSalt` versionNumber

instance Prelude.NFData GetLayerVersion where
  rnf GetLayerVersion' {..} =
    Prelude.rnf layerName
      `Prelude.seq` Prelude.rnf versionNumber

instance Data.ToHeaders GetLayerVersion where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetLayerVersion where
  toPath GetLayerVersion' {..} =
    Prelude.mconcat
      [ "/2018-10-31/layers/",
        Data.toBS layerName,
        "/versions/",
        Data.toBS versionNumber
      ]

instance Data.ToQuery GetLayerVersion where
  toQuery = Prelude.const Prelude.mempty
