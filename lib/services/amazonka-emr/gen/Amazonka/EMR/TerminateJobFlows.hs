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
-- Module      : Amazonka.EMR.TerminateJobFlows
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- TerminateJobFlows shuts a list of clusters (job flows) down. When a job
-- flow is shut down, any step not yet completed is canceled and the EC2
-- instances on which the cluster is running are stopped. Any log files not
-- already saved are uploaded to Amazon S3 if a LogUri was specified when
-- the cluster was created.
--
-- The maximum number of clusters allowed is 10. The call to
-- @TerminateJobFlows@ is asynchronous. Depending on the configuration of
-- the cluster, it may take up to 1-5 minutes for the cluster to completely
-- terminate and release allocated resources, such as Amazon EC2 instances.
module Amazonka.EMR.TerminateJobFlows
  ( -- * Creating a Request
    TerminateJobFlows (..),
    newTerminateJobFlows,

    -- * Request Lenses
    terminateJobFlows_jobFlowIds,

    -- * Destructuring the Response
    TerminateJobFlowsResponse (..),
    newTerminateJobFlowsResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EMR.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Input to the TerminateJobFlows operation.
--
-- /See:/ 'newTerminateJobFlows' smart constructor.
data TerminateJobFlows = TerminateJobFlows'
  { -- | A list of job flows to be shut down.
    jobFlowIds :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TerminateJobFlows' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobFlowIds', 'terminateJobFlows_jobFlowIds' - A list of job flows to be shut down.
newTerminateJobFlows ::
  TerminateJobFlows
newTerminateJobFlows =
  TerminateJobFlows' {jobFlowIds = Prelude.mempty}

-- | A list of job flows to be shut down.
terminateJobFlows_jobFlowIds :: Lens.Lens' TerminateJobFlows [Prelude.Text]
terminateJobFlows_jobFlowIds = Lens.lens (\TerminateJobFlows' {jobFlowIds} -> jobFlowIds) (\s@TerminateJobFlows' {} a -> s {jobFlowIds = a} :: TerminateJobFlows) Prelude.. Lens.coerced

instance Core.AWSRequest TerminateJobFlows where
  type
    AWSResponse TerminateJobFlows =
      TerminateJobFlowsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull TerminateJobFlowsResponse'

instance Prelude.Hashable TerminateJobFlows where
  hashWithSalt _salt TerminateJobFlows' {..} =
    _salt `Prelude.hashWithSalt` jobFlowIds

instance Prelude.NFData TerminateJobFlows where
  rnf TerminateJobFlows' {..} = Prelude.rnf jobFlowIds

instance Data.ToHeaders TerminateJobFlows where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "ElasticMapReduce.TerminateJobFlows" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON TerminateJobFlows where
  toJSON TerminateJobFlows' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("JobFlowIds" Data..= jobFlowIds)]
      )

instance Data.ToPath TerminateJobFlows where
  toPath = Prelude.const "/"

instance Data.ToQuery TerminateJobFlows where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newTerminateJobFlowsResponse' smart constructor.
data TerminateJobFlowsResponse = TerminateJobFlowsResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TerminateJobFlowsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newTerminateJobFlowsResponse ::
  TerminateJobFlowsResponse
newTerminateJobFlowsResponse =
  TerminateJobFlowsResponse'

instance Prelude.NFData TerminateJobFlowsResponse where
  rnf _ = ()
