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
-- Module      : Network.AWS.EMR.SetTerminationProtection
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
module Network.AWS.EMR.SetTerminationProtection
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

import Network.AWS.EMR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
setTerminationProtection_jobFlowIds = Lens.lens (\SetTerminationProtection' {jobFlowIds} -> jobFlowIds) (\s@SetTerminationProtection' {} a -> s {jobFlowIds = a} :: SetTerminationProtection) Prelude.. Prelude._Coerce

-- | A Boolean that indicates whether to protect the cluster and prevent the
-- Amazon EC2 instances in the cluster from shutting down due to API calls,
-- user intervention, or job-flow error.
setTerminationProtection_terminationProtected :: Lens.Lens' SetTerminationProtection Prelude.Bool
setTerminationProtection_terminationProtected = Lens.lens (\SetTerminationProtection' {terminationProtected} -> terminationProtected) (\s@SetTerminationProtection' {} a -> s {terminationProtected = a} :: SetTerminationProtection)

instance Prelude.AWSRequest SetTerminationProtection where
  type
    Rs SetTerminationProtection =
      SetTerminationProtectionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull
      SetTerminationProtectionResponse'

instance Prelude.Hashable SetTerminationProtection

instance Prelude.NFData SetTerminationProtection

instance Prelude.ToHeaders SetTerminationProtection where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "ElasticMapReduce.SetTerminationProtection" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON SetTerminationProtection where
  toJSON SetTerminationProtection' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("JobFlowIds" Prelude..= jobFlowIds),
            Prelude.Just
              ( "TerminationProtected"
                  Prelude..= terminationProtected
              )
          ]
      )

instance Prelude.ToPath SetTerminationProtection where
  toPath = Prelude.const "/"

instance Prelude.ToQuery SetTerminationProtection where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSetTerminationProtectionResponse' smart constructor.
data SetTerminationProtectionResponse = SetTerminationProtectionResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
