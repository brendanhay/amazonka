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
-- Module      : Amazonka.ElasticSearch.DescribeElasticsearchDomainConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides cluster configuration information about the specified
-- Elasticsearch domain, such as the state, creation date, update version,
-- and update date for cluster options.
module Amazonka.ElasticSearch.DescribeElasticsearchDomainConfig
  ( -- * Creating a Request
    DescribeElasticsearchDomainConfig (..),
    newDescribeElasticsearchDomainConfig,

    -- * Request Lenses
    describeElasticsearchDomainConfig_domainName,

    -- * Destructuring the Response
    DescribeElasticsearchDomainConfigResponse (..),
    newDescribeElasticsearchDomainConfigResponse,

    -- * Response Lenses
    describeElasticsearchDomainConfigResponse_httpStatus,
    describeElasticsearchDomainConfigResponse_domainConfig,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElasticSearch.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Container for the parameters to the @DescribeElasticsearchDomainConfig@
-- operation. Specifies the domain name for which you want configuration
-- information.
--
-- /See:/ 'newDescribeElasticsearchDomainConfig' smart constructor.
data DescribeElasticsearchDomainConfig = DescribeElasticsearchDomainConfig'
  { -- | The Elasticsearch domain that you want to get information about.
    domainName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeElasticsearchDomainConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainName', 'describeElasticsearchDomainConfig_domainName' - The Elasticsearch domain that you want to get information about.
newDescribeElasticsearchDomainConfig ::
  -- | 'domainName'
  Prelude.Text ->
  DescribeElasticsearchDomainConfig
newDescribeElasticsearchDomainConfig pDomainName_ =
  DescribeElasticsearchDomainConfig'
    { domainName =
        pDomainName_
    }

-- | The Elasticsearch domain that you want to get information about.
describeElasticsearchDomainConfig_domainName :: Lens.Lens' DescribeElasticsearchDomainConfig Prelude.Text
describeElasticsearchDomainConfig_domainName = Lens.lens (\DescribeElasticsearchDomainConfig' {domainName} -> domainName) (\s@DescribeElasticsearchDomainConfig' {} a -> s {domainName = a} :: DescribeElasticsearchDomainConfig)

instance
  Core.AWSRequest
    DescribeElasticsearchDomainConfig
  where
  type
    AWSResponse DescribeElasticsearchDomainConfig =
      DescribeElasticsearchDomainConfigResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeElasticsearchDomainConfigResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "DomainConfig")
      )

instance
  Prelude.Hashable
    DescribeElasticsearchDomainConfig
  where
  hashWithSalt
    _salt
    DescribeElasticsearchDomainConfig' {..} =
      _salt `Prelude.hashWithSalt` domainName

instance
  Prelude.NFData
    DescribeElasticsearchDomainConfig
  where
  rnf DescribeElasticsearchDomainConfig' {..} =
    Prelude.rnf domainName

instance
  Data.ToHeaders
    DescribeElasticsearchDomainConfig
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    DescribeElasticsearchDomainConfig
  where
  toPath DescribeElasticsearchDomainConfig' {..} =
    Prelude.mconcat
      [ "/2015-01-01/es/domain/",
        Data.toBS domainName,
        "/config"
      ]

instance
  Data.ToQuery
    DescribeElasticsearchDomainConfig
  where
  toQuery = Prelude.const Prelude.mempty

-- | The result of a @DescribeElasticsearchDomainConfig@ request. Contains
-- the configuration information of the requested domain.
--
-- /See:/ 'newDescribeElasticsearchDomainConfigResponse' smart constructor.
data DescribeElasticsearchDomainConfigResponse = DescribeElasticsearchDomainConfigResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The configuration information of the domain requested in the
    -- @DescribeElasticsearchDomainConfig@ request.
    domainConfig :: ElasticsearchDomainConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeElasticsearchDomainConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'describeElasticsearchDomainConfigResponse_httpStatus' - The response's http status code.
--
-- 'domainConfig', 'describeElasticsearchDomainConfigResponse_domainConfig' - The configuration information of the domain requested in the
-- @DescribeElasticsearchDomainConfig@ request.
newDescribeElasticsearchDomainConfigResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'domainConfig'
  ElasticsearchDomainConfig ->
  DescribeElasticsearchDomainConfigResponse
newDescribeElasticsearchDomainConfigResponse
  pHttpStatus_
  pDomainConfig_ =
    DescribeElasticsearchDomainConfigResponse'
      { httpStatus =
          pHttpStatus_,
        domainConfig = pDomainConfig_
      }

-- | The response's http status code.
describeElasticsearchDomainConfigResponse_httpStatus :: Lens.Lens' DescribeElasticsearchDomainConfigResponse Prelude.Int
describeElasticsearchDomainConfigResponse_httpStatus = Lens.lens (\DescribeElasticsearchDomainConfigResponse' {httpStatus} -> httpStatus) (\s@DescribeElasticsearchDomainConfigResponse' {} a -> s {httpStatus = a} :: DescribeElasticsearchDomainConfigResponse)

-- | The configuration information of the domain requested in the
-- @DescribeElasticsearchDomainConfig@ request.
describeElasticsearchDomainConfigResponse_domainConfig :: Lens.Lens' DescribeElasticsearchDomainConfigResponse ElasticsearchDomainConfig
describeElasticsearchDomainConfigResponse_domainConfig = Lens.lens (\DescribeElasticsearchDomainConfigResponse' {domainConfig} -> domainConfig) (\s@DescribeElasticsearchDomainConfigResponse' {} a -> s {domainConfig = a} :: DescribeElasticsearchDomainConfigResponse)

instance
  Prelude.NFData
    DescribeElasticsearchDomainConfigResponse
  where
  rnf DescribeElasticsearchDomainConfigResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf domainConfig
