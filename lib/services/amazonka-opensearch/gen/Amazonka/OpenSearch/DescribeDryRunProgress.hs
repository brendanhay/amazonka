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
-- Module      : Amazonka.OpenSearch.DescribeDryRunProgress
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the progress of a pre-update dry run analysis on an Amazon
-- OpenSearch Service domain. For more information, see
-- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/managedomains-configuration-changes#dryrun Determining whether a change will cause a blue\/green deployment>.
module Amazonka.OpenSearch.DescribeDryRunProgress
  ( -- * Creating a Request
    DescribeDryRunProgress (..),
    newDescribeDryRunProgress,

    -- * Request Lenses
    describeDryRunProgress_dryRunId,
    describeDryRunProgress_loadDryRunConfig,
    describeDryRunProgress_domainName,

    -- * Destructuring the Response
    DescribeDryRunProgressResponse (..),
    newDescribeDryRunProgressResponse,

    -- * Response Lenses
    describeDryRunProgressResponse_dryRunConfig,
    describeDryRunProgressResponse_dryRunProgressStatus,
    describeDryRunProgressResponse_dryRunResults,
    describeDryRunProgressResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpenSearch.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeDryRunProgress' smart constructor.
data DescribeDryRunProgress = DescribeDryRunProgress'
  { -- | The unique identifier of the dry run.
    dryRunId :: Prelude.Maybe Prelude.Text,
    -- | Whether to include the configuration of the dry run in the response. The
    -- configuration specifies the updates that you\'re planning to make on the
    -- domain.
    loadDryRunConfig :: Prelude.Maybe Prelude.Bool,
    -- | The name of the domain.
    domainName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDryRunProgress' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRunId', 'describeDryRunProgress_dryRunId' - The unique identifier of the dry run.
--
-- 'loadDryRunConfig', 'describeDryRunProgress_loadDryRunConfig' - Whether to include the configuration of the dry run in the response. The
-- configuration specifies the updates that you\'re planning to make on the
-- domain.
--
-- 'domainName', 'describeDryRunProgress_domainName' - The name of the domain.
newDescribeDryRunProgress ::
  -- | 'domainName'
  Prelude.Text ->
  DescribeDryRunProgress
newDescribeDryRunProgress pDomainName_ =
  DescribeDryRunProgress'
    { dryRunId = Prelude.Nothing,
      loadDryRunConfig = Prelude.Nothing,
      domainName = pDomainName_
    }

-- | The unique identifier of the dry run.
describeDryRunProgress_dryRunId :: Lens.Lens' DescribeDryRunProgress (Prelude.Maybe Prelude.Text)
describeDryRunProgress_dryRunId = Lens.lens (\DescribeDryRunProgress' {dryRunId} -> dryRunId) (\s@DescribeDryRunProgress' {} a -> s {dryRunId = a} :: DescribeDryRunProgress)

-- | Whether to include the configuration of the dry run in the response. The
-- configuration specifies the updates that you\'re planning to make on the
-- domain.
describeDryRunProgress_loadDryRunConfig :: Lens.Lens' DescribeDryRunProgress (Prelude.Maybe Prelude.Bool)
describeDryRunProgress_loadDryRunConfig = Lens.lens (\DescribeDryRunProgress' {loadDryRunConfig} -> loadDryRunConfig) (\s@DescribeDryRunProgress' {} a -> s {loadDryRunConfig = a} :: DescribeDryRunProgress)

-- | The name of the domain.
describeDryRunProgress_domainName :: Lens.Lens' DescribeDryRunProgress Prelude.Text
describeDryRunProgress_domainName = Lens.lens (\DescribeDryRunProgress' {domainName} -> domainName) (\s@DescribeDryRunProgress' {} a -> s {domainName = a} :: DescribeDryRunProgress)

instance Core.AWSRequest DescribeDryRunProgress where
  type
    AWSResponse DescribeDryRunProgress =
      DescribeDryRunProgressResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeDryRunProgressResponse'
            Prelude.<$> (x Data..?> "DryRunConfig")
            Prelude.<*> (x Data..?> "DryRunProgressStatus")
            Prelude.<*> (x Data..?> "DryRunResults")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeDryRunProgress where
  hashWithSalt _salt DescribeDryRunProgress' {..} =
    _salt
      `Prelude.hashWithSalt` dryRunId
      `Prelude.hashWithSalt` loadDryRunConfig
      `Prelude.hashWithSalt` domainName

instance Prelude.NFData DescribeDryRunProgress where
  rnf DescribeDryRunProgress' {..} =
    Prelude.rnf dryRunId
      `Prelude.seq` Prelude.rnf loadDryRunConfig
      `Prelude.seq` Prelude.rnf domainName

instance Data.ToHeaders DescribeDryRunProgress where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeDryRunProgress where
  toPath DescribeDryRunProgress' {..} =
    Prelude.mconcat
      [ "/2021-01-01/opensearch/domain/",
        Data.toBS domainName,
        "/dryRun"
      ]

instance Data.ToQuery DescribeDryRunProgress where
  toQuery DescribeDryRunProgress' {..} =
    Prelude.mconcat
      [ "dryRunId" Data.=: dryRunId,
        "loadDryRunConfig" Data.=: loadDryRunConfig
      ]

-- | /See:/ 'newDescribeDryRunProgressResponse' smart constructor.
data DescribeDryRunProgressResponse = DescribeDryRunProgressResponse'
  { -- | Details about the changes you\'re planning to make on the domain.
    dryRunConfig :: Prelude.Maybe DomainStatus,
    -- | The current status of the dry run, including any validation errors.
    dryRunProgressStatus :: Prelude.Maybe DryRunProgressStatus,
    -- | The results of the dry run.
    dryRunResults :: Prelude.Maybe DryRunResults,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDryRunProgressResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRunConfig', 'describeDryRunProgressResponse_dryRunConfig' - Details about the changes you\'re planning to make on the domain.
--
-- 'dryRunProgressStatus', 'describeDryRunProgressResponse_dryRunProgressStatus' - The current status of the dry run, including any validation errors.
--
-- 'dryRunResults', 'describeDryRunProgressResponse_dryRunResults' - The results of the dry run.
--
-- 'httpStatus', 'describeDryRunProgressResponse_httpStatus' - The response's http status code.
newDescribeDryRunProgressResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeDryRunProgressResponse
newDescribeDryRunProgressResponse pHttpStatus_ =
  DescribeDryRunProgressResponse'
    { dryRunConfig =
        Prelude.Nothing,
      dryRunProgressStatus = Prelude.Nothing,
      dryRunResults = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Details about the changes you\'re planning to make on the domain.
describeDryRunProgressResponse_dryRunConfig :: Lens.Lens' DescribeDryRunProgressResponse (Prelude.Maybe DomainStatus)
describeDryRunProgressResponse_dryRunConfig = Lens.lens (\DescribeDryRunProgressResponse' {dryRunConfig} -> dryRunConfig) (\s@DescribeDryRunProgressResponse' {} a -> s {dryRunConfig = a} :: DescribeDryRunProgressResponse)

-- | The current status of the dry run, including any validation errors.
describeDryRunProgressResponse_dryRunProgressStatus :: Lens.Lens' DescribeDryRunProgressResponse (Prelude.Maybe DryRunProgressStatus)
describeDryRunProgressResponse_dryRunProgressStatus = Lens.lens (\DescribeDryRunProgressResponse' {dryRunProgressStatus} -> dryRunProgressStatus) (\s@DescribeDryRunProgressResponse' {} a -> s {dryRunProgressStatus = a} :: DescribeDryRunProgressResponse)

-- | The results of the dry run.
describeDryRunProgressResponse_dryRunResults :: Lens.Lens' DescribeDryRunProgressResponse (Prelude.Maybe DryRunResults)
describeDryRunProgressResponse_dryRunResults = Lens.lens (\DescribeDryRunProgressResponse' {dryRunResults} -> dryRunResults) (\s@DescribeDryRunProgressResponse' {} a -> s {dryRunResults = a} :: DescribeDryRunProgressResponse)

-- | The response's http status code.
describeDryRunProgressResponse_httpStatus :: Lens.Lens' DescribeDryRunProgressResponse Prelude.Int
describeDryRunProgressResponse_httpStatus = Lens.lens (\DescribeDryRunProgressResponse' {httpStatus} -> httpStatus) (\s@DescribeDryRunProgressResponse' {} a -> s {httpStatus = a} :: DescribeDryRunProgressResponse)

instance
  Prelude.NFData
    DescribeDryRunProgressResponse
  where
  rnf DescribeDryRunProgressResponse' {..} =
    Prelude.rnf dryRunConfig
      `Prelude.seq` Prelude.rnf dryRunProgressStatus
      `Prelude.seq` Prelude.rnf dryRunResults
      `Prelude.seq` Prelude.rnf httpStatus
