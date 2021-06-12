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
-- Module      : Network.AWS.APIGateway.GetBasePathMapping
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describe a BasePathMapping resource.
module Network.AWS.APIGateway.GetBasePathMapping
  ( -- * Creating a Request
    GetBasePathMapping (..),
    newGetBasePathMapping,

    -- * Request Lenses
    getBasePathMapping_domainName,
    getBasePathMapping_basePath,

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

-- | Request to describe a BasePathMapping resource.
--
-- /See:/ 'newGetBasePathMapping' smart constructor.
data GetBasePathMapping = GetBasePathMapping'
  { -- | [Required] The domain name of the BasePathMapping resource to be
    -- described.
    domainName :: Core.Text,
    -- | [Required] The base path name that callers of the API must provide as
    -- part of the URL after the domain name. This value must be unique for all
    -- of the mappings across a single API. Specify \'(none)\' if you do not
    -- want callers to specify any base path name after the domain name.
    basePath :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetBasePathMapping' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainName', 'getBasePathMapping_domainName' - [Required] The domain name of the BasePathMapping resource to be
-- described.
--
-- 'basePath', 'getBasePathMapping_basePath' - [Required] The base path name that callers of the API must provide as
-- part of the URL after the domain name. This value must be unique for all
-- of the mappings across a single API. Specify \'(none)\' if you do not
-- want callers to specify any base path name after the domain name.
newGetBasePathMapping ::
  -- | 'domainName'
  Core.Text ->
  -- | 'basePath'
  Core.Text ->
  GetBasePathMapping
newGetBasePathMapping pDomainName_ pBasePath_ =
  GetBasePathMapping'
    { domainName = pDomainName_,
      basePath = pBasePath_
    }

-- | [Required] The domain name of the BasePathMapping resource to be
-- described.
getBasePathMapping_domainName :: Lens.Lens' GetBasePathMapping Core.Text
getBasePathMapping_domainName = Lens.lens (\GetBasePathMapping' {domainName} -> domainName) (\s@GetBasePathMapping' {} a -> s {domainName = a} :: GetBasePathMapping)

-- | [Required] The base path name that callers of the API must provide as
-- part of the URL after the domain name. This value must be unique for all
-- of the mappings across a single API. Specify \'(none)\' if you do not
-- want callers to specify any base path name after the domain name.
getBasePathMapping_basePath :: Lens.Lens' GetBasePathMapping Core.Text
getBasePathMapping_basePath = Lens.lens (\GetBasePathMapping' {basePath} -> basePath) (\s@GetBasePathMapping' {} a -> s {basePath = a} :: GetBasePathMapping)

instance Core.AWSRequest GetBasePathMapping where
  type AWSResponse GetBasePathMapping = BasePathMapping
  request = Request.get defaultService
  response =
    Response.receiveJSON
      (\s h x -> Core.eitherParseJSON x)

instance Core.Hashable GetBasePathMapping

instance Core.NFData GetBasePathMapping

instance Core.ToHeaders GetBasePathMapping where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Accept"
              Core.=# ("application/json" :: Core.ByteString)
          ]
      )

instance Core.ToPath GetBasePathMapping where
  toPath GetBasePathMapping' {..} =
    Core.mconcat
      [ "/domainnames/",
        Core.toBS domainName,
        "/basepathmappings/",
        Core.toBS basePath
      ]

instance Core.ToQuery GetBasePathMapping where
  toQuery = Core.const Core.mempty
