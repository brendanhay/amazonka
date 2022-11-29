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
-- Module      : Amazonka.OpenSearch.DescribeDomainConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the configuration of an Amazon OpenSearch Service domain.
module Amazonka.OpenSearch.DescribeDomainConfig
  ( -- * Creating a Request
    DescribeDomainConfig (..),
    newDescribeDomainConfig,

    -- * Request Lenses
    describeDomainConfig_domainName,

    -- * Destructuring the Response
    DescribeDomainConfigResponse (..),
    newDescribeDomainConfigResponse,

    -- * Response Lenses
    describeDomainConfigResponse_httpStatus,
    describeDomainConfigResponse_domainConfig,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.OpenSearch.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Container for the parameters to the @DescribeDomainConfig@ operation.
--
-- /See:/ 'newDescribeDomainConfig' smart constructor.
data DescribeDomainConfig = DescribeDomainConfig'
  { -- | Name of the OpenSearch Service domain configuration that you want to
    -- describe.
    domainName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDomainConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainName', 'describeDomainConfig_domainName' - Name of the OpenSearch Service domain configuration that you want to
-- describe.
newDescribeDomainConfig ::
  -- | 'domainName'
  Prelude.Text ->
  DescribeDomainConfig
newDescribeDomainConfig pDomainName_ =
  DescribeDomainConfig' {domainName = pDomainName_}

-- | Name of the OpenSearch Service domain configuration that you want to
-- describe.
describeDomainConfig_domainName :: Lens.Lens' DescribeDomainConfig Prelude.Text
describeDomainConfig_domainName = Lens.lens (\DescribeDomainConfig' {domainName} -> domainName) (\s@DescribeDomainConfig' {} a -> s {domainName = a} :: DescribeDomainConfig)

instance Core.AWSRequest DescribeDomainConfig where
  type
    AWSResponse DescribeDomainConfig =
      DescribeDomainConfigResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeDomainConfigResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "DomainConfig")
      )

instance Prelude.Hashable DescribeDomainConfig where
  hashWithSalt _salt DescribeDomainConfig' {..} =
    _salt `Prelude.hashWithSalt` domainName

instance Prelude.NFData DescribeDomainConfig where
  rnf DescribeDomainConfig' {..} =
    Prelude.rnf domainName

instance Core.ToHeaders DescribeDomainConfig where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DescribeDomainConfig where
  toPath DescribeDomainConfig' {..} =
    Prelude.mconcat
      [ "/2021-01-01/opensearch/domain/",
        Core.toBS domainName,
        "/config"
      ]

instance Core.ToQuery DescribeDomainConfig where
  toQuery = Prelude.const Prelude.mempty

-- | Contains the configuration information of the requested domain.
--
-- /See:/ 'newDescribeDomainConfigResponse' smart constructor.
data DescribeDomainConfigResponse = DescribeDomainConfigResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Container for the configuration of the OpenSearch Service domain.
    domainConfig :: DomainConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDomainConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'describeDomainConfigResponse_httpStatus' - The response's http status code.
--
-- 'domainConfig', 'describeDomainConfigResponse_domainConfig' - Container for the configuration of the OpenSearch Service domain.
newDescribeDomainConfigResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'domainConfig'
  DomainConfig ->
  DescribeDomainConfigResponse
newDescribeDomainConfigResponse
  pHttpStatus_
  pDomainConfig_ =
    DescribeDomainConfigResponse'
      { httpStatus =
          pHttpStatus_,
        domainConfig = pDomainConfig_
      }

-- | The response's http status code.
describeDomainConfigResponse_httpStatus :: Lens.Lens' DescribeDomainConfigResponse Prelude.Int
describeDomainConfigResponse_httpStatus = Lens.lens (\DescribeDomainConfigResponse' {httpStatus} -> httpStatus) (\s@DescribeDomainConfigResponse' {} a -> s {httpStatus = a} :: DescribeDomainConfigResponse)

-- | Container for the configuration of the OpenSearch Service domain.
describeDomainConfigResponse_domainConfig :: Lens.Lens' DescribeDomainConfigResponse DomainConfig
describeDomainConfigResponse_domainConfig = Lens.lens (\DescribeDomainConfigResponse' {domainConfig} -> domainConfig) (\s@DescribeDomainConfigResponse' {} a -> s {domainConfig = a} :: DescribeDomainConfigResponse)

instance Prelude.NFData DescribeDomainConfigResponse where
  rnf DescribeDomainConfigResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf domainConfig
