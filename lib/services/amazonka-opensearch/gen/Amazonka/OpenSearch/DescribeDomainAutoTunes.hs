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
-- Module      : Amazonka.OpenSearch.DescribeDomainAutoTunes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the list of optimizations that Auto-Tune has made to an Amazon
-- OpenSearch Service domain. For more information, see
-- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/auto-tune.html Auto-Tune for Amazon OpenSearch Service>.
module Amazonka.OpenSearch.DescribeDomainAutoTunes
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
import Amazonka.OpenSearch.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Container for the parameters to the @DescribeDomainAutoTunes@ operation.
--
-- /See:/ 'newDescribeDomainAutoTunes' smart constructor.
data DescribeDomainAutoTunes = DescribeDomainAutoTunes'
  { -- | An optional parameter that specifies the maximum number of results to
    -- return. You can use @nextToken@ to get the next page of results.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | If your initial @DescribeDomainAutoTunes@ operation returns a
    -- @nextToken@, you can include the returned @nextToken@ in subsequent
    -- @DescribeDomainAutoTunes@ operations, which returns results in the next
    -- page.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Name of the domain that you want Auto-Tune details about.
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
-- 'maxResults', 'describeDomainAutoTunes_maxResults' - An optional parameter that specifies the maximum number of results to
-- return. You can use @nextToken@ to get the next page of results.
--
-- 'nextToken', 'describeDomainAutoTunes_nextToken' - If your initial @DescribeDomainAutoTunes@ operation returns a
-- @nextToken@, you can include the returned @nextToken@ in subsequent
-- @DescribeDomainAutoTunes@ operations, which returns results in the next
-- page.
--
-- 'domainName', 'describeDomainAutoTunes_domainName' - Name of the domain that you want Auto-Tune details about.
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

-- | An optional parameter that specifies the maximum number of results to
-- return. You can use @nextToken@ to get the next page of results.
describeDomainAutoTunes_maxResults :: Lens.Lens' DescribeDomainAutoTunes (Prelude.Maybe Prelude.Int)
describeDomainAutoTunes_maxResults = Lens.lens (\DescribeDomainAutoTunes' {maxResults} -> maxResults) (\s@DescribeDomainAutoTunes' {} a -> s {maxResults = a} :: DescribeDomainAutoTunes)

-- | If your initial @DescribeDomainAutoTunes@ operation returns a
-- @nextToken@, you can include the returned @nextToken@ in subsequent
-- @DescribeDomainAutoTunes@ operations, which returns results in the next
-- page.
describeDomainAutoTunes_nextToken :: Lens.Lens' DescribeDomainAutoTunes (Prelude.Maybe Prelude.Text)
describeDomainAutoTunes_nextToken = Lens.lens (\DescribeDomainAutoTunes' {nextToken} -> nextToken) (\s@DescribeDomainAutoTunes' {} a -> s {nextToken = a} :: DescribeDomainAutoTunes)

-- | Name of the domain that you want Auto-Tune details about.
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
    _salt
      `Prelude.hashWithSalt` maxResults
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
      [ "/2021-01-01/opensearch/domain/",
        Data.toBS domainName,
        "/autoTunes"
      ]

instance Data.ToQuery DescribeDomainAutoTunes where
  toQuery = Prelude.const Prelude.mempty

-- | The result of a @DescribeDomainAutoTunes@ request.
--
-- /See:/ 'newDescribeDomainAutoTunesResponse' smart constructor.
data DescribeDomainAutoTunesResponse = DescribeDomainAutoTunesResponse'
  { -- | The list of setting adjustments that Auto-Tune has made to the domain.
    autoTunes :: Prelude.Maybe [AutoTune],
    -- | When @nextToken@ is returned, there are more results available. The
    -- value of @nextToken@ is a unique pagination token for each page. Make
    -- the call again using the returned token to retrieve the next page.
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
-- 'autoTunes', 'describeDomainAutoTunesResponse_autoTunes' - The list of setting adjustments that Auto-Tune has made to the domain.
--
-- 'nextToken', 'describeDomainAutoTunesResponse_nextToken' - When @nextToken@ is returned, there are more results available. The
-- value of @nextToken@ is a unique pagination token for each page. Make
-- the call again using the returned token to retrieve the next page.
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

-- | The list of setting adjustments that Auto-Tune has made to the domain.
describeDomainAutoTunesResponse_autoTunes :: Lens.Lens' DescribeDomainAutoTunesResponse (Prelude.Maybe [AutoTune])
describeDomainAutoTunesResponse_autoTunes = Lens.lens (\DescribeDomainAutoTunesResponse' {autoTunes} -> autoTunes) (\s@DescribeDomainAutoTunesResponse' {} a -> s {autoTunes = a} :: DescribeDomainAutoTunesResponse) Prelude.. Lens.mapping Lens.coerced

-- | When @nextToken@ is returned, there are more results available. The
-- value of @nextToken@ is a unique pagination token for each page. Make
-- the call again using the returned token to retrieve the next page.
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
