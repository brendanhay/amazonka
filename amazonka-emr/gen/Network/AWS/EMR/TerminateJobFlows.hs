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
-- Module      : Network.AWS.EMR.TerminateJobFlows
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.EMR.TerminateJobFlows
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

import Network.AWS.EMR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Input to the TerminateJobFlows operation.
--
-- /See:/ 'newTerminateJobFlows' smart constructor.
data TerminateJobFlows = TerminateJobFlows'
  { -- | A list of job flows to be shut down.
    jobFlowIds :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
terminateJobFlows_jobFlowIds = Lens.lens (\TerminateJobFlows' {jobFlowIds} -> jobFlowIds) (\s@TerminateJobFlows' {} a -> s {jobFlowIds = a} :: TerminateJobFlows) Prelude.. Prelude._Coerce

instance Prelude.AWSRequest TerminateJobFlows where
  type Rs TerminateJobFlows = TerminateJobFlowsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull TerminateJobFlowsResponse'

instance Prelude.Hashable TerminateJobFlows

instance Prelude.NFData TerminateJobFlows

instance Prelude.ToHeaders TerminateJobFlows where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "ElasticMapReduce.TerminateJobFlows" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON TerminateJobFlows where
  toJSON TerminateJobFlows' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("JobFlowIds" Prelude..= jobFlowIds)]
      )

instance Prelude.ToPath TerminateJobFlows where
  toPath = Prelude.const "/"

instance Prelude.ToQuery TerminateJobFlows where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newTerminateJobFlowsResponse' smart constructor.
data TerminateJobFlowsResponse = TerminateJobFlowsResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'TerminateJobFlowsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newTerminateJobFlowsResponse ::
  TerminateJobFlowsResponse
newTerminateJobFlowsResponse =
  TerminateJobFlowsResponse'

instance Prelude.NFData TerminateJobFlowsResponse
