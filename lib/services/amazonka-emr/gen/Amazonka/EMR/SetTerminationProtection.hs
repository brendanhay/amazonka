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
-- Module      : Amazonka.EMR.SetTerminationProtection
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- SetTerminationProtection locks a cluster (job flow) so the EC2 instances
-- in the cluster cannot be terminated by user intervention, an API call,
-- or in the event of a job-flow error. The cluster still terminates upon
-- successful completion of the job flow. Calling
-- @SetTerminationProtection@ on a cluster is similar to calling the Amazon
-- EC2 @DisableAPITermination@ API on all EC2 instances in a cluster.
--
-- @SetTerminationProtection@ is used to prevent accidental termination of
-- a cluster and to ensure that in the event of an error, the instances
-- persist so that you can recover any data stored in their ephemeral
-- instance storage.
--
-- To terminate a cluster that has been locked by setting
-- @SetTerminationProtection@ to @true@, you must first unlock the job flow
-- by a subsequent call to @SetTerminationProtection@ in which you set the
-- value to @false@.
--
-- For more information,
-- see<https://docs.aws.amazon.com/emr/latest/ManagementGuide/UsingEMR_TerminationProtection.html Managing Cluster Termination>
-- in the /Amazon EMR Management Guide/.
module Amazonka.EMR.SetTerminationProtection
  ( -- * Creating a Request
    SetTerminationProtection (..),
    newSetTerminationProtection,

    -- * Request Lenses
    setTerminationProtection_jobFlowIds,
    setTerminationProtection_terminationProtected,

    -- * Destructuring the Response
    SetTerminationProtectionResponse (..),
    newSetTerminationProtectionResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EMR.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The input argument to the TerminationProtection operation.
--
-- /See:/ 'newSetTerminationProtection' smart constructor.
data SetTerminationProtection = SetTerminationProtection'
  { -- | A list of strings that uniquely identify the clusters to protect. This
    -- identifier is returned by RunJobFlow and can also be obtained from
    -- DescribeJobFlows .
    jobFlowIds :: [Prelude.Text],
    -- | A Boolean that indicates whether to protect the cluster and prevent the
    -- Amazon EC2 instances in the cluster from shutting down due to API calls,
    -- user intervention, or job-flow error.
    terminationProtected :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SetTerminationProtection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobFlowIds', 'setTerminationProtection_jobFlowIds' - A list of strings that uniquely identify the clusters to protect. This
-- identifier is returned by RunJobFlow and can also be obtained from
-- DescribeJobFlows .
--
-- 'terminationProtected', 'setTerminationProtection_terminationProtected' - A Boolean that indicates whether to protect the cluster and prevent the
-- Amazon EC2 instances in the cluster from shutting down due to API calls,
-- user intervention, or job-flow error.
newSetTerminationProtection ::
  -- | 'terminationProtected'
  Prelude.Bool ->
  SetTerminationProtection
newSetTerminationProtection pTerminationProtected_ =
  SetTerminationProtection'
    { jobFlowIds =
        Prelude.mempty,
      terminationProtected = pTerminationProtected_
    }

-- | A list of strings that uniquely identify the clusters to protect. This
-- identifier is returned by RunJobFlow and can also be obtained from
-- DescribeJobFlows .
setTerminationProtection_jobFlowIds :: Lens.Lens' SetTerminationProtection [Prelude.Text]
setTerminationProtection_jobFlowIds = Lens.lens (\SetTerminationProtection' {jobFlowIds} -> jobFlowIds) (\s@SetTerminationProtection' {} a -> s {jobFlowIds = a} :: SetTerminationProtection) Prelude.. Lens.coerced

-- | A Boolean that indicates whether to protect the cluster and prevent the
-- Amazon EC2 instances in the cluster from shutting down due to API calls,
-- user intervention, or job-flow error.
setTerminationProtection_terminationProtected :: Lens.Lens' SetTerminationProtection Prelude.Bool
setTerminationProtection_terminationProtected = Lens.lens (\SetTerminationProtection' {terminationProtected} -> terminationProtected) (\s@SetTerminationProtection' {} a -> s {terminationProtected = a} :: SetTerminationProtection)

instance Core.AWSRequest SetTerminationProtection where
  type
    AWSResponse SetTerminationProtection =
      SetTerminationProtectionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull
      SetTerminationProtectionResponse'

instance Prelude.Hashable SetTerminationProtection where
  hashWithSalt _salt SetTerminationProtection' {..} =
    _salt
      `Prelude.hashWithSalt` jobFlowIds
      `Prelude.hashWithSalt` terminationProtected

instance Prelude.NFData SetTerminationProtection where
  rnf SetTerminationProtection' {..} =
    Prelude.rnf jobFlowIds `Prelude.seq`
      Prelude.rnf terminationProtected

instance Data.ToHeaders SetTerminationProtection where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "ElasticMapReduce.SetTerminationProtection" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON SetTerminationProtection where
  toJSON SetTerminationProtection' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("JobFlowIds" Data..= jobFlowIds),
            Prelude.Just
              ( "TerminationProtected"
                  Data..= terminationProtected
              )
          ]
      )

instance Data.ToPath SetTerminationProtection where
  toPath = Prelude.const "/"

instance Data.ToQuery SetTerminationProtection where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSetTerminationProtectionResponse' smart constructor.
data SetTerminationProtectionResponse = SetTerminationProtectionResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SetTerminationProtectionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newSetTerminationProtectionResponse ::
  SetTerminationProtectionResponse
newSetTerminationProtectionResponse =
  SetTerminationProtectionResponse'

instance
  Prelude.NFData
    SetTerminationProtectionResponse
  where
  rnf _ = ()
