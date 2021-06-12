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
-- Module      : Network.AWS.APIGateway.CreateBasePathMapping
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new BasePathMapping resource.
module Network.AWS.APIGateway.CreateBasePathMapping
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
    basePathMapping_stage,
    basePathMapping_restApiId,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Requests API Gateway to create a new BasePathMapping resource.
--
-- /See:/ 'newCreateBasePathMapping' smart constructor.
data CreateBasePathMapping = CreateBasePathMapping'
  { -- | The base path name that callers of the API must provide as part of the
    -- URL after the domain name. This value must be unique for all of the
    -- mappings across a single API. Specify \'(none)\' if you do not want
    -- callers to specify a base path name after the domain name.
    basePath :: Core.Maybe Core.Text,
    -- | The name of the API\'s stage that you want to use for this mapping.
    -- Specify \'(none)\' if you want callers to explicitly specify the stage
    -- name after any base path name.
    stage :: Core.Maybe Core.Text,
    -- | [Required] The domain name of the BasePathMapping resource to create.
    domainName :: Core.Text,
    -- | [Required] The string identifier of the associated RestApi.
    restApiId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- 'domainName', 'createBasePathMapping_domainName' - [Required] The domain name of the BasePathMapping resource to create.
--
-- 'restApiId', 'createBasePathMapping_restApiId' - [Required] The string identifier of the associated RestApi.
newCreateBasePathMapping ::
  -- | 'domainName'
  Core.Text ->
  -- | 'restApiId'
  Core.Text ->
  CreateBasePathMapping
newCreateBasePathMapping pDomainName_ pRestApiId_ =
  CreateBasePathMapping'
    { basePath = Core.Nothing,
      stage = Core.Nothing,
      domainName = pDomainName_,
      restApiId = pRestApiId_
    }

-- | The base path name that callers of the API must provide as part of the
-- URL after the domain name. This value must be unique for all of the
-- mappings across a single API. Specify \'(none)\' if you do not want
-- callers to specify a base path name after the domain name.
createBasePathMapping_basePath :: Lens.Lens' CreateBasePathMapping (Core.Maybe Core.Text)
createBasePathMapping_basePath = Lens.lens (\CreateBasePathMapping' {basePath} -> basePath) (\s@CreateBasePathMapping' {} a -> s {basePath = a} :: CreateBasePathMapping)

-- | The name of the API\'s stage that you want to use for this mapping.
-- Specify \'(none)\' if you want callers to explicitly specify the stage
-- name after any base path name.
createBasePathMapping_stage :: Lens.Lens' CreateBasePathMapping (Core.Maybe Core.Text)
createBasePathMapping_stage = Lens.lens (\CreateBasePathMapping' {stage} -> stage) (\s@CreateBasePathMapping' {} a -> s {stage = a} :: CreateBasePathMapping)

-- | [Required] The domain name of the BasePathMapping resource to create.
createBasePathMapping_domainName :: Lens.Lens' CreateBasePathMapping Core.Text
createBasePathMapping_domainName = Lens.lens (\CreateBasePathMapping' {domainName} -> domainName) (\s@CreateBasePathMapping' {} a -> s {domainName = a} :: CreateBasePathMapping)

-- | [Required] The string identifier of the associated RestApi.
createBasePathMapping_restApiId :: Lens.Lens' CreateBasePathMapping Core.Text
createBasePathMapping_restApiId = Lens.lens (\CreateBasePathMapping' {restApiId} -> restApiId) (\s@CreateBasePathMapping' {} a -> s {restApiId = a} :: CreateBasePathMapping)

instance Core.AWSRequest CreateBasePathMapping where
  type
    AWSResponse CreateBasePathMapping =
      BasePathMapping
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      (\s h x -> Core.eitherParseJSON x)

instance Core.Hashable CreateBasePathMapping

instance Core.NFData CreateBasePathMapping

instance Core.ToHeaders CreateBasePathMapping where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Accept"
              Core.=# ("application/json" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateBasePathMapping where
  toJSON CreateBasePathMapping' {..} =
    Core.object
      ( Core.catMaybes
          [ ("basePath" Core..=) Core.<$> basePath,
            ("stage" Core..=) Core.<$> stage,
            Core.Just ("restApiId" Core..= restApiId)
          ]
      )

instance Core.ToPath CreateBasePathMapping where
  toPath CreateBasePathMapping' {..} =
    Core.mconcat
      [ "/domainnames/",
        Core.toBS domainName,
        "/basepathmappings"
      ]

instance Core.ToQuery CreateBasePathMapping where
  toQuery = Core.const Core.mempty
