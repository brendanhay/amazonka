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
-- Module      : Amazonka.Proton.GetResourcesSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get counts of Proton resources.
--
-- For infrastructure-provisioning resources (environments, services,
-- service instances, pipelines), the action returns staleness counts. A
-- resource is stale when it\'s behind the recommended version of the
-- Proton template that it uses and it needs an update to become current.
--
-- The action returns staleness counts (counts of resources that are
-- up-to-date, behind a template major version, or behind a template minor
-- version), the total number of resources, and the number of resources
-- that are in a failed state, grouped by resource type. Components,
-- environments, and service templates return less information - see the
-- @components@, @environments@, and @serviceTemplates@ field descriptions.
--
-- For context, the action also returns the total number of each type of
-- Proton template in the Amazon Web Services account.
--
-- For more information, see
-- <https://docs.aws.amazon.com/proton/latest/userguide/monitoring-dashboard.html Proton dashboard>
-- in the /Proton User Guide/.
module Amazonka.Proton.GetResourcesSummary
  ( -- * Creating a Request
    GetResourcesSummary (..),
    newGetResourcesSummary,

    -- * Destructuring the Response
    GetResourcesSummaryResponse (..),
    newGetResourcesSummaryResponse,

    -- * Response Lenses
    getResourcesSummaryResponse_httpStatus,
    getResourcesSummaryResponse_counts,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Proton.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetResourcesSummary' smart constructor.
data GetResourcesSummary = GetResourcesSummary'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetResourcesSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newGetResourcesSummary ::
  GetResourcesSummary
newGetResourcesSummary = GetResourcesSummary'

instance Core.AWSRequest GetResourcesSummary where
  type
    AWSResponse GetResourcesSummary =
      GetResourcesSummaryResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetResourcesSummaryResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "counts")
      )

instance Prelude.Hashable GetResourcesSummary where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance Prelude.NFData GetResourcesSummary where
  rnf _ = ()

instance Data.ToHeaders GetResourcesSummary where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AwsProton20200720.GetResourcesSummary" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetResourcesSummary where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath GetResourcesSummary where
  toPath = Prelude.const "/"

instance Data.ToQuery GetResourcesSummary where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetResourcesSummaryResponse' smart constructor.
data GetResourcesSummaryResponse = GetResourcesSummaryResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Summary counts of each Proton resource type.
    counts :: CountsSummary
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetResourcesSummaryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getResourcesSummaryResponse_httpStatus' - The response's http status code.
--
-- 'counts', 'getResourcesSummaryResponse_counts' - Summary counts of each Proton resource type.
newGetResourcesSummaryResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'counts'
  CountsSummary ->
  GetResourcesSummaryResponse
newGetResourcesSummaryResponse pHttpStatus_ pCounts_ =
  GetResourcesSummaryResponse'
    { httpStatus =
        pHttpStatus_,
      counts = pCounts_
    }

-- | The response's http status code.
getResourcesSummaryResponse_httpStatus :: Lens.Lens' GetResourcesSummaryResponse Prelude.Int
getResourcesSummaryResponse_httpStatus = Lens.lens (\GetResourcesSummaryResponse' {httpStatus} -> httpStatus) (\s@GetResourcesSummaryResponse' {} a -> s {httpStatus = a} :: GetResourcesSummaryResponse)

-- | Summary counts of each Proton resource type.
getResourcesSummaryResponse_counts :: Lens.Lens' GetResourcesSummaryResponse CountsSummary
getResourcesSummaryResponse_counts = Lens.lens (\GetResourcesSummaryResponse' {counts} -> counts) (\s@GetResourcesSummaryResponse' {} a -> s {counts = a} :: GetResourcesSummaryResponse)

instance Prelude.NFData GetResourcesSummaryResponse where
  rnf GetResourcesSummaryResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf counts
