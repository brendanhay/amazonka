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
-- Module      : Amazonka.ElasticSearch.DescribeDomainChangeProgress
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the current blue\/green deployment happening
-- on a domain, including a change ID, status, and progress stages.
module Amazonka.ElasticSearch.DescribeDomainChangeProgress
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
import Amazonka.ElasticSearch.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Container for the parameters to the @DescribeDomainChangeProgress@
-- operation. Specifies the domain name and optional change specific
-- identity for which you want progress information.
--
-- /See:/ 'newDescribeDomainChangeProgress' smart constructor.
data DescribeDomainChangeProgress = DescribeDomainChangeProgress'
  { -- | The specific change ID for which you want to get progress information.
    -- This is an optional parameter. If omitted, the service returns
    -- information about the most recent configuration change.
    changeId :: Prelude.Maybe Prelude.Text,
    -- | The domain you want to get the progress information about.
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
-- This is an optional parameter. If omitted, the service returns
-- information about the most recent configuration change.
--
-- 'domainName', 'describeDomainChangeProgress_domainName' - The domain you want to get the progress information about.
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
-- This is an optional parameter. If omitted, the service returns
-- information about the most recent configuration change.
describeDomainChangeProgress_changeId :: Lens.Lens' DescribeDomainChangeProgress (Prelude.Maybe Prelude.Text)
describeDomainChangeProgress_changeId = Lens.lens (\DescribeDomainChangeProgress' {changeId} -> changeId) (\s@DescribeDomainChangeProgress' {} a -> s {changeId = a} :: DescribeDomainChangeProgress)

-- | The domain you want to get the progress information about.
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
    Prelude.rnf changeId
      `Prelude.seq` Prelude.rnf domainName

instance Data.ToHeaders DescribeDomainChangeProgress where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeDomainChangeProgress where
  toPath DescribeDomainChangeProgress' {..} =
    Prelude.mconcat
      [ "/2015-01-01/es/domain/",
        Data.toBS domainName,
        "/progress"
      ]

instance Data.ToQuery DescribeDomainChangeProgress where
  toQuery DescribeDomainChangeProgress' {..} =
    Prelude.mconcat ["changeid" Data.=: changeId]

-- | The result of a @DescribeDomainChangeProgress@ request. Contains the
-- progress information of the requested domain change.
--
-- /See:/ 'newDescribeDomainChangeProgressResponse' smart constructor.
data DescribeDomainChangeProgressResponse = DescribeDomainChangeProgressResponse'
  { -- | Progress information for the configuration change that is requested in
    -- the @DescribeDomainChangeProgress@ request.
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
-- 'changeProgressStatus', 'describeDomainChangeProgressResponse_changeProgressStatus' - Progress information for the configuration change that is requested in
-- the @DescribeDomainChangeProgress@ request.
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

-- | Progress information for the configuration change that is requested in
-- the @DescribeDomainChangeProgress@ request.
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
    Prelude.rnf changeProgressStatus
      `Prelude.seq` Prelude.rnf httpStatus
