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
-- Module      : Network.AWS.ElasticSearch.DescribeElasticsearchDomainConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides cluster configuration information about the specified
-- Elasticsearch domain, such as the state, creation date, update version,
-- and update date for cluster options.
module Network.AWS.ElasticSearch.DescribeElasticsearchDomainConfig
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

import Network.AWS.ElasticSearch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Container for the parameters to the @DescribeElasticsearchDomainConfig@
-- operation. Specifies the domain name for which you want configuration
-- information.
--
-- /See:/ 'newDescribeElasticsearchDomainConfig' smart constructor.
data DescribeElasticsearchDomainConfig = DescribeElasticsearchDomainConfig'
  { -- | The Elasticsearch domain that you want to get information about.
    domainName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.AWSRequest
    DescribeElasticsearchDomainConfig
  where
  type
    Rs DescribeElasticsearchDomainConfig =
      DescribeElasticsearchDomainConfigResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeElasticsearchDomainConfigResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
              Prelude.<*> (x Prelude..:> "DomainConfig")
      )

instance
  Prelude.Hashable
    DescribeElasticsearchDomainConfig

instance
  Prelude.NFData
    DescribeElasticsearchDomainConfig

instance
  Prelude.ToHeaders
    DescribeElasticsearchDomainConfig
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Prelude.ToPath
    DescribeElasticsearchDomainConfig
  where
  toPath DescribeElasticsearchDomainConfig' {..} =
    Prelude.mconcat
      [ "/2015-01-01/es/domain/",
        Prelude.toBS domainName,
        "/config"
      ]

instance
  Prelude.ToQuery
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
