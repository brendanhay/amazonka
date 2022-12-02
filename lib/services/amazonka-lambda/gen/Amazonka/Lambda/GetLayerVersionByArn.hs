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
-- Module      : Amazonka.Lambda.GetLayerVersionByArn
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a version of an
-- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-layers.html Lambda layer>,
-- with a link to download the layer archive that\'s valid for 10 minutes.
module Amazonka.Lambda.GetLayerVersionByArn
  ( -- * Creating a Request
    GetLayerVersionByArn (..),
    newGetLayerVersionByArn,

    -- * Request Lenses
    getLayerVersionByArn_arn,

    -- * Destructuring the Response
    GetLayerVersionResponse (..),
    newGetLayerVersionResponse,

    -- * Response Lenses
    getLayerVersionResponse_compatibleArchitectures,
    getLayerVersionResponse_layerArn,
    getLayerVersionResponse_layerVersionArn,
    getLayerVersionResponse_licenseInfo,
    getLayerVersionResponse_description,
    getLayerVersionResponse_compatibleRuntimes,
    getLayerVersionResponse_createdDate,
    getLayerVersionResponse_content,
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

-- | /See:/ 'newGetLayerVersionByArn' smart constructor.
data GetLayerVersionByArn = GetLayerVersionByArn'
  { -- | The ARN of the layer version.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetLayerVersionByArn' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'getLayerVersionByArn_arn' - The ARN of the layer version.
newGetLayerVersionByArn ::
  -- | 'arn'
  Prelude.Text ->
  GetLayerVersionByArn
newGetLayerVersionByArn pArn_ =
  GetLayerVersionByArn' {arn = pArn_}

-- | The ARN of the layer version.
getLayerVersionByArn_arn :: Lens.Lens' GetLayerVersionByArn Prelude.Text
getLayerVersionByArn_arn = Lens.lens (\GetLayerVersionByArn' {arn} -> arn) (\s@GetLayerVersionByArn' {} a -> s {arn = a} :: GetLayerVersionByArn)

instance Core.AWSRequest GetLayerVersionByArn where
  type
    AWSResponse GetLayerVersionByArn =
      GetLayerVersionResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Data.eitherParseJSON x)

instance Prelude.Hashable GetLayerVersionByArn where
  hashWithSalt _salt GetLayerVersionByArn' {..} =
    _salt `Prelude.hashWithSalt` arn

instance Prelude.NFData GetLayerVersionByArn where
  rnf GetLayerVersionByArn' {..} = Prelude.rnf arn

instance Data.ToHeaders GetLayerVersionByArn where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetLayerVersionByArn where
  toPath = Prelude.const "/2018-10-31/layers"

instance Data.ToQuery GetLayerVersionByArn where
  toQuery GetLayerVersionByArn' {..} =
    Prelude.mconcat
      ["Arn" Data.=: arn, "find=LayerVersion"]
