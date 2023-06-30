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
-- Module      : Amazonka.APIGateway.CreateBasePathMapping
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new BasePathMapping resource.
module Amazonka.APIGateway.CreateBasePathMapping
  ( -- * Creating a Request
    CreateBasePathMapping (..),
    newCreateBasePathMapping,

    -- * Request Lenses
    createBasePathMapping_basePath,
    createBasePathMapping_stage,
    createBasePathMapping_domainName,
    createBasePathMapping_restApiId,

    -- * Destructuring the Response
    BasePathMapping (..),
    newBasePathMapping,

    -- * Response Lenses
    basePathMapping_basePath,
    basePathMapping_restApiId,
    basePathMapping_stage,
  )
where

import Amazonka.APIGateway.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Requests API Gateway to create a new BasePathMapping resource.
--
-- /See:/ 'newCreateBasePathMapping' smart constructor.
data CreateBasePathMapping = CreateBasePathMapping'
  { -- | The base path name that callers of the API must provide as part of the
    -- URL after the domain name. This value must be unique for all of the
    -- mappings across a single API. Specify \'(none)\' if you do not want
    -- callers to specify a base path name after the domain name.
    basePath :: Prelude.Maybe Prelude.Text,
    -- | The name of the API\'s stage that you want to use for this mapping.
    -- Specify \'(none)\' if you want callers to explicitly specify the stage
    -- name after any base path name.
    stage :: Prelude.Maybe Prelude.Text,
    -- | The domain name of the BasePathMapping resource to create.
    domainName :: Prelude.Text,
    -- | The string identifier of the associated RestApi.
    restApiId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateBasePathMapping' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'basePath', 'createBasePathMapping_basePath' - The base path name that callers of the API must provide as part of the
-- URL after the domain name. This value must be unique for all of the
-- mappings across a single API. Specify \'(none)\' if you do not want
-- callers to specify a base path name after the domain name.
--
-- 'stage', 'createBasePathMapping_stage' - The name of the API\'s stage that you want to use for this mapping.
-- Specify \'(none)\' if you want callers to explicitly specify the stage
-- name after any base path name.
--
-- 'domainName', 'createBasePathMapping_domainName' - The domain name of the BasePathMapping resource to create.
--
-- 'restApiId', 'createBasePathMapping_restApiId' - The string identifier of the associated RestApi.
newCreateBasePathMapping ::
  -- | 'domainName'
  Prelude.Text ->
  -- | 'restApiId'
  Prelude.Text ->
  CreateBasePathMapping
newCreateBasePathMapping pDomainName_ pRestApiId_ =
  CreateBasePathMapping'
    { basePath = Prelude.Nothing,
      stage = Prelude.Nothing,
      domainName = pDomainName_,
      restApiId = pRestApiId_
    }

-- | The base path name that callers of the API must provide as part of the
-- URL after the domain name. This value must be unique for all of the
-- mappings across a single API. Specify \'(none)\' if you do not want
-- callers to specify a base path name after the domain name.
createBasePathMapping_basePath :: Lens.Lens' CreateBasePathMapping (Prelude.Maybe Prelude.Text)
createBasePathMapping_basePath = Lens.lens (\CreateBasePathMapping' {basePath} -> basePath) (\s@CreateBasePathMapping' {} a -> s {basePath = a} :: CreateBasePathMapping)

-- | The name of the API\'s stage that you want to use for this mapping.
-- Specify \'(none)\' if you want callers to explicitly specify the stage
-- name after any base path name.
createBasePathMapping_stage :: Lens.Lens' CreateBasePathMapping (Prelude.Maybe Prelude.Text)
createBasePathMapping_stage = Lens.lens (\CreateBasePathMapping' {stage} -> stage) (\s@CreateBasePathMapping' {} a -> s {stage = a} :: CreateBasePathMapping)

-- | The domain name of the BasePathMapping resource to create.
createBasePathMapping_domainName :: Lens.Lens' CreateBasePathMapping Prelude.Text
createBasePathMapping_domainName = Lens.lens (\CreateBasePathMapping' {domainName} -> domainName) (\s@CreateBasePathMapping' {} a -> s {domainName = a} :: CreateBasePathMapping)

-- | The string identifier of the associated RestApi.
createBasePathMapping_restApiId :: Lens.Lens' CreateBasePathMapping Prelude.Text
createBasePathMapping_restApiId = Lens.lens (\CreateBasePathMapping' {restApiId} -> restApiId) (\s@CreateBasePathMapping' {} a -> s {restApiId = a} :: CreateBasePathMapping)

instance Core.AWSRequest CreateBasePathMapping where
  type
    AWSResponse CreateBasePathMapping =
      BasePathMapping
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Data.eitherParseJSON x)

instance Prelude.Hashable CreateBasePathMapping where
  hashWithSalt _salt CreateBasePathMapping' {..} =
    _salt
      `Prelude.hashWithSalt` basePath
      `Prelude.hashWithSalt` stage
      `Prelude.hashWithSalt` domainName
      `Prelude.hashWithSalt` restApiId

instance Prelude.NFData CreateBasePathMapping where
  rnf CreateBasePathMapping' {..} =
    Prelude.rnf basePath
      `Prelude.seq` Prelude.rnf stage
      `Prelude.seq` Prelude.rnf domainName
      `Prelude.seq` Prelude.rnf restApiId

instance Data.ToHeaders CreateBasePathMapping where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Data.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Data.ToJSON CreateBasePathMapping where
  toJSON CreateBasePathMapping' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("basePath" Data..=) Prelude.<$> basePath,
            ("stage" Data..=) Prelude.<$> stage,
            Prelude.Just ("restApiId" Data..= restApiId)
          ]
      )

instance Data.ToPath CreateBasePathMapping where
  toPath CreateBasePathMapping' {..} =
    Prelude.mconcat
      [ "/domainnames/",
        Data.toBS domainName,
        "/basepathmappings"
      ]

instance Data.ToQuery CreateBasePathMapping where
  toQuery = Prelude.const Prelude.mempty
