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
-- Module      : Amazonka.ElasticSearch.DescribeDomainAutoTunes
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides scheduled Auto-Tune action details for the Elasticsearch
-- domain, such as Auto-Tune action type, description, severity, and
-- scheduled date.
module Amazonka.ElasticSearch.DescribeDomainAutoTunes
  ( -- * Creating a Request
    DescribeDomainAutoTunes (..),
    newDescribeDomainAutoTunes,

    -- * Request Lenses
    describeDomainAutoTunes_maxResults,
    describeDomainAutoTunes_nextToken,
    describeDomainAutoTunes_domainName,

    -- * Destructuring the Response
    DescribeDomainAutoTunesResponse (..),
    newDescribeDomainAutoTunesResponse,

    -- * Response Lenses
    describeDomainAutoTunesResponse_autoTunes,
    describeDomainAutoTunesResponse_nextToken,
    describeDomainAutoTunesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElasticSearch.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Container for the parameters to the @DescribeDomainAutoTunes@ operation.
--
-- /See:/ 'newDescribeDomainAutoTunes' smart constructor.
data DescribeDomainAutoTunes = DescribeDomainAutoTunes'
  { -- | Set this value to limit the number of results returned. If not
    -- specified, defaults to 100.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | NextToken is sent in case the earlier API call results contain the
    -- NextToken. It is used for pagination.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Specifies the domain name for which you want Auto-Tune action details.
    domainName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDomainAutoTunes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'describeDomainAutoTunes_maxResults' - Set this value to limit the number of results returned. If not
-- specified, defaults to 100.
--
-- 'nextToken', 'describeDomainAutoTunes_nextToken' - NextToken is sent in case the earlier API call results contain the
-- NextToken. It is used for pagination.
--
-- 'domainName', 'describeDomainAutoTunes_domainName' - Specifies the domain name for which you want Auto-Tune action details.
newDescribeDomainAutoTunes ::
  -- | 'domainName'
  Prelude.Text ->
  DescribeDomainAutoTunes
newDescribeDomainAutoTunes pDomainName_ =
  DescribeDomainAutoTunes'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      domainName = pDomainName_
    }

-- | Set this value to limit the number of results returned. If not
-- specified, defaults to 100.
describeDomainAutoTunes_maxResults :: Lens.Lens' DescribeDomainAutoTunes (Prelude.Maybe Prelude.Int)
describeDomainAutoTunes_maxResults = Lens.lens (\DescribeDomainAutoTunes' {maxResults} -> maxResults) (\s@DescribeDomainAutoTunes' {} a -> s {maxResults = a} :: DescribeDomainAutoTunes)

-- | NextToken is sent in case the earlier API call results contain the
-- NextToken. It is used for pagination.
describeDomainAutoTunes_nextToken :: Lens.Lens' DescribeDomainAutoTunes (Prelude.Maybe Prelude.Text)
describeDomainAutoTunes_nextToken = Lens.lens (\DescribeDomainAutoTunes' {nextToken} -> nextToken) (\s@DescribeDomainAutoTunes' {} a -> s {nextToken = a} :: DescribeDomainAutoTunes)

-- | Specifies the domain name for which you want Auto-Tune action details.
describeDomainAutoTunes_domainName :: Lens.Lens' DescribeDomainAutoTunes Prelude.Text
describeDomainAutoTunes_domainName = Lens.lens (\DescribeDomainAutoTunes' {domainName} -> domainName) (\s@DescribeDomainAutoTunes' {} a -> s {domainName = a} :: DescribeDomainAutoTunes)

instance Core.AWSRequest DescribeDomainAutoTunes where
  type
    AWSResponse DescribeDomainAutoTunes =
      DescribeDomainAutoTunesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeDomainAutoTunesResponse'
            Prelude.<$> (x Data..?> "AutoTunes" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeDomainAutoTunes where
  hashWithSalt _salt DescribeDomainAutoTunes' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` domainName

instance Prelude.NFData DescribeDomainAutoTunes where
  rnf DescribeDomainAutoTunes' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf domainName

instance Data.ToHeaders DescribeDomainAutoTunes where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeDomainAutoTunes where
  toPath DescribeDomainAutoTunes' {..} =
    Prelude.mconcat
      [ "/2015-01-01/es/domain/",
        Data.toBS domainName,
        "/autoTunes"
      ]

instance Data.ToQuery DescribeDomainAutoTunes where
  toQuery = Prelude.const Prelude.mempty

-- | The result of @DescribeDomainAutoTunes@ request. See the
-- <https://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/auto-tune.html Developer Guide>
-- for more information.
--
-- /See:/ 'newDescribeDomainAutoTunesResponse' smart constructor.
data DescribeDomainAutoTunesResponse = DescribeDomainAutoTunesResponse'
  { -- | Specifies the list of setting adjustments that Auto-Tune has made to the
    -- domain. See the
    -- <https://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/auto-tune.html Developer Guide>
    -- for more information.
    autoTunes :: Prelude.Maybe [AutoTune],
    -- | Specifies an identifier to allow retrieval of paginated results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDomainAutoTunesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'autoTunes', 'describeDomainAutoTunesResponse_autoTunes' - Specifies the list of setting adjustments that Auto-Tune has made to the
-- domain. See the
-- <https://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/auto-tune.html Developer Guide>
-- for more information.
--
-- 'nextToken', 'describeDomainAutoTunesResponse_nextToken' - Specifies an identifier to allow retrieval of paginated results.
--
-- 'httpStatus', 'describeDomainAutoTunesResponse_httpStatus' - The response's http status code.
newDescribeDomainAutoTunesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeDomainAutoTunesResponse
newDescribeDomainAutoTunesResponse pHttpStatus_ =
  DescribeDomainAutoTunesResponse'
    { autoTunes =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Specifies the list of setting adjustments that Auto-Tune has made to the
-- domain. See the
-- <https://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/auto-tune.html Developer Guide>
-- for more information.
describeDomainAutoTunesResponse_autoTunes :: Lens.Lens' DescribeDomainAutoTunesResponse (Prelude.Maybe [AutoTune])
describeDomainAutoTunesResponse_autoTunes = Lens.lens (\DescribeDomainAutoTunesResponse' {autoTunes} -> autoTunes) (\s@DescribeDomainAutoTunesResponse' {} a -> s {autoTunes = a} :: DescribeDomainAutoTunesResponse) Prelude.. Lens.mapping Lens.coerced

-- | Specifies an identifier to allow retrieval of paginated results.
describeDomainAutoTunesResponse_nextToken :: Lens.Lens' DescribeDomainAutoTunesResponse (Prelude.Maybe Prelude.Text)
describeDomainAutoTunesResponse_nextToken = Lens.lens (\DescribeDomainAutoTunesResponse' {nextToken} -> nextToken) (\s@DescribeDomainAutoTunesResponse' {} a -> s {nextToken = a} :: DescribeDomainAutoTunesResponse)

-- | The response's http status code.
describeDomainAutoTunesResponse_httpStatus :: Lens.Lens' DescribeDomainAutoTunesResponse Prelude.Int
describeDomainAutoTunesResponse_httpStatus = Lens.lens (\DescribeDomainAutoTunesResponse' {httpStatus} -> httpStatus) (\s@DescribeDomainAutoTunesResponse' {} a -> s {httpStatus = a} :: DescribeDomainAutoTunesResponse)

instance
  Prelude.NFData
    DescribeDomainAutoTunesResponse
  where
  rnf DescribeDomainAutoTunesResponse' {..} =
    Prelude.rnf autoTunes
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
