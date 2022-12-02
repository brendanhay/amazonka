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
-- Module      : Amazonka.APIGateway.GetBasePathMapping
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describe a BasePathMapping resource.
module Amazonka.APIGateway.GetBasePathMapping
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
    basePathMapping_restApiId,
    basePathMapping_stage,
    basePathMapping_basePath,
  )
where

import Amazonka.APIGateway.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Request to describe a BasePathMapping resource.
--
-- /See:/ 'newGetBasePathMapping' smart constructor.
data GetBasePathMapping = GetBasePathMapping'
  { -- | The domain name of the BasePathMapping resource to be described.
    domainName :: Prelude.Text,
    -- | The base path name that callers of the API must provide as part of the
    -- URL after the domain name. This value must be unique for all of the
    -- mappings across a single API. Specify \'(none)\' if you do not want
    -- callers to specify any base path name after the domain name.
    basePath :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetBasePathMapping' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainName', 'getBasePathMapping_domainName' - The domain name of the BasePathMapping resource to be described.
--
-- 'basePath', 'getBasePathMapping_basePath' - The base path name that callers of the API must provide as part of the
-- URL after the domain name. This value must be unique for all of the
-- mappings across a single API. Specify \'(none)\' if you do not want
-- callers to specify any base path name after the domain name.
newGetBasePathMapping ::
  -- | 'domainName'
  Prelude.Text ->
  -- | 'basePath'
  Prelude.Text ->
  GetBasePathMapping
newGetBasePathMapping pDomainName_ pBasePath_ =
  GetBasePathMapping'
    { domainName = pDomainName_,
      basePath = pBasePath_
    }

-- | The domain name of the BasePathMapping resource to be described.
getBasePathMapping_domainName :: Lens.Lens' GetBasePathMapping Prelude.Text
getBasePathMapping_domainName = Lens.lens (\GetBasePathMapping' {domainName} -> domainName) (\s@GetBasePathMapping' {} a -> s {domainName = a} :: GetBasePathMapping)

-- | The base path name that callers of the API must provide as part of the
-- URL after the domain name. This value must be unique for all of the
-- mappings across a single API. Specify \'(none)\' if you do not want
-- callers to specify any base path name after the domain name.
getBasePathMapping_basePath :: Lens.Lens' GetBasePathMapping Prelude.Text
getBasePathMapping_basePath = Lens.lens (\GetBasePathMapping' {basePath} -> basePath) (\s@GetBasePathMapping' {} a -> s {basePath = a} :: GetBasePathMapping)

instance Core.AWSRequest GetBasePathMapping where
  type AWSResponse GetBasePathMapping = BasePathMapping
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Data.eitherParseJSON x)

instance Prelude.Hashable GetBasePathMapping where
  hashWithSalt _salt GetBasePathMapping' {..} =
    _salt `Prelude.hashWithSalt` domainName
      `Prelude.hashWithSalt` basePath

instance Prelude.NFData GetBasePathMapping where
  rnf GetBasePathMapping' {..} =
    Prelude.rnf domainName
      `Prelude.seq` Prelude.rnf basePath

instance Data.ToHeaders GetBasePathMapping where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Data.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Data.ToPath GetBasePathMapping where
  toPath GetBasePathMapping' {..} =
    Prelude.mconcat
      [ "/domainnames/",
        Data.toBS domainName,
        "/basepathmappings/",
        Data.toBS basePath
      ]

instance Data.ToQuery GetBasePathMapping where
  toQuery = Prelude.const Prelude.mempty
