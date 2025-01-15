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
-- Module      : Amazonka.OpenSearch.DescribeDomainChangeProgress
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the current blue\/green deployment happening
-- on an Amazon OpenSearch Service domain. For more information, see
-- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/managedomains-configuration-changes.html Making configuration changes in Amazon OpenSearch Service>.
module Amazonka.OpenSearch.DescribeDomainChangeProgress
  ( -- * Creating a Request
    DescribeDomainChangeProgress (..),
    newDescribeDomainChangeProgress,

    -- * Request Lenses
    describeDomainChangeProgress_changeId,
    describeDomainChangeProgress_domainName,

    -- * Destructuring the Response
    DescribeDomainChangeProgressResponse (..),
    newDescribeDomainChangeProgressResponse,

    -- * Response Lenses
    describeDomainChangeProgressResponse_changeProgressStatus,
    describeDomainChangeProgressResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpenSearch.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Container for the parameters to the @DescribeDomainChangeProgress@
-- operation.
--
-- /See:/ 'newDescribeDomainChangeProgress' smart constructor.
data DescribeDomainChangeProgress = DescribeDomainChangeProgress'
  { -- | The specific change ID for which you want to get progress information.
    -- If omitted, the request returns information about the most recent
    -- configuration change.
    changeId :: Prelude.Maybe Prelude.Text,
    -- | The name of the domain to get progress information for.
    domainName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDomainChangeProgress' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'changeId', 'describeDomainChangeProgress_changeId' - The specific change ID for which you want to get progress information.
-- If omitted, the request returns information about the most recent
-- configuration change.
--
-- 'domainName', 'describeDomainChangeProgress_domainName' - The name of the domain to get progress information for.
newDescribeDomainChangeProgress ::
  -- | 'domainName'
  Prelude.Text ->
  DescribeDomainChangeProgress
newDescribeDomainChangeProgress pDomainName_ =
  DescribeDomainChangeProgress'
    { changeId =
        Prelude.Nothing,
      domainName = pDomainName_
    }

-- | The specific change ID for which you want to get progress information.
-- If omitted, the request returns information about the most recent
-- configuration change.
describeDomainChangeProgress_changeId :: Lens.Lens' DescribeDomainChangeProgress (Prelude.Maybe Prelude.Text)
describeDomainChangeProgress_changeId = Lens.lens (\DescribeDomainChangeProgress' {changeId} -> changeId) (\s@DescribeDomainChangeProgress' {} a -> s {changeId = a} :: DescribeDomainChangeProgress)

-- | The name of the domain to get progress information for.
describeDomainChangeProgress_domainName :: Lens.Lens' DescribeDomainChangeProgress Prelude.Text
describeDomainChangeProgress_domainName = Lens.lens (\DescribeDomainChangeProgress' {domainName} -> domainName) (\s@DescribeDomainChangeProgress' {} a -> s {domainName = a} :: DescribeDomainChangeProgress)

instance Core.AWSRequest DescribeDomainChangeProgress where
  type
    AWSResponse DescribeDomainChangeProgress =
      DescribeDomainChangeProgressResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeDomainChangeProgressResponse'
            Prelude.<$> (x Data..?> "ChangeProgressStatus")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeDomainChangeProgress
  where
  hashWithSalt _salt DescribeDomainChangeProgress' {..} =
    _salt
      `Prelude.hashWithSalt` changeId
      `Prelude.hashWithSalt` domainName

instance Prelude.NFData DescribeDomainChangeProgress where
  rnf DescribeDomainChangeProgress' {..} =
    Prelude.rnf changeId `Prelude.seq`
      Prelude.rnf domainName

instance Data.ToHeaders DescribeDomainChangeProgress where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeDomainChangeProgress where
  toPath DescribeDomainChangeProgress' {..} =
    Prelude.mconcat
      [ "/2021-01-01/opensearch/domain/",
        Data.toBS domainName,
        "/progress"
      ]

instance Data.ToQuery DescribeDomainChangeProgress where
  toQuery DescribeDomainChangeProgress' {..} =
    Prelude.mconcat ["changeid" Data.=: changeId]

-- | The result of a @DescribeDomainChangeProgress@ request. Contains
-- progress information for the requested domain change.
--
-- /See:/ 'newDescribeDomainChangeProgressResponse' smart constructor.
data DescribeDomainChangeProgressResponse = DescribeDomainChangeProgressResponse'
  { -- | Container for information about the stages of a configuration change
    -- happening on a domain.
    changeProgressStatus :: Prelude.Maybe ChangeProgressStatusDetails,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDomainChangeProgressResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'changeProgressStatus', 'describeDomainChangeProgressResponse_changeProgressStatus' - Container for information about the stages of a configuration change
-- happening on a domain.
--
-- 'httpStatus', 'describeDomainChangeProgressResponse_httpStatus' - The response's http status code.
newDescribeDomainChangeProgressResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeDomainChangeProgressResponse
newDescribeDomainChangeProgressResponse pHttpStatus_ =
  DescribeDomainChangeProgressResponse'
    { changeProgressStatus =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Container for information about the stages of a configuration change
-- happening on a domain.
describeDomainChangeProgressResponse_changeProgressStatus :: Lens.Lens' DescribeDomainChangeProgressResponse (Prelude.Maybe ChangeProgressStatusDetails)
describeDomainChangeProgressResponse_changeProgressStatus = Lens.lens (\DescribeDomainChangeProgressResponse' {changeProgressStatus} -> changeProgressStatus) (\s@DescribeDomainChangeProgressResponse' {} a -> s {changeProgressStatus = a} :: DescribeDomainChangeProgressResponse)

-- | The response's http status code.
describeDomainChangeProgressResponse_httpStatus :: Lens.Lens' DescribeDomainChangeProgressResponse Prelude.Int
describeDomainChangeProgressResponse_httpStatus = Lens.lens (\DescribeDomainChangeProgressResponse' {httpStatus} -> httpStatus) (\s@DescribeDomainChangeProgressResponse' {} a -> s {httpStatus = a} :: DescribeDomainChangeProgressResponse)

instance
  Prelude.NFData
    DescribeDomainChangeProgressResponse
  where
  rnf DescribeDomainChangeProgressResponse' {..} =
    Prelude.rnf changeProgressStatus `Prelude.seq`
      Prelude.rnf httpStatus
