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
-- Module      : Amazonka.ElasticBeanstalk.TerminateEnvironment
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Terminates the specified environment.
module Amazonka.ElasticBeanstalk.TerminateEnvironment
  ( -- * Creating a Request
    TerminateEnvironment (..),
    newTerminateEnvironment,

    -- * Request Lenses
    terminateEnvironment_forceTerminate,
    terminateEnvironment_terminateResources,
    terminateEnvironment_environmentName,
    terminateEnvironment_environmentId,

    -- * Destructuring the Response
    EnvironmentDescription (..),
    newEnvironmentDescription,

    -- * Response Lenses
    environmentDescription_status,
    environmentDescription_cname,
    environmentDescription_templateName,
    environmentDescription_abortableOperationInProgress,
    environmentDescription_endpointURL,
    environmentDescription_resources,
    environmentDescription_dateUpdated,
    environmentDescription_dateCreated,
    environmentDescription_health,
    environmentDescription_versionLabel,
    environmentDescription_operationsRole,
    environmentDescription_platformArn,
    environmentDescription_tier,
    environmentDescription_environmentName,
    environmentDescription_applicationName,
    environmentDescription_environmentArn,
    environmentDescription_solutionStackName,
    environmentDescription_environmentId,
    environmentDescription_healthStatus,
    environmentDescription_environmentLinks,
    environmentDescription_description,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.ElasticBeanstalk.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Request to terminate an environment.
--
-- /See:/ 'newTerminateEnvironment' smart constructor.
data TerminateEnvironment = TerminateEnvironment'
  { -- | Terminates the target environment even if another environment in the
    -- same group is dependent on it.
    forceTerminate :: Prelude.Maybe Prelude.Bool,
    -- | Indicates whether the associated AWS resources should shut down when the
    -- environment is terminated:
    --
    -- -   @true@: The specified environment as well as the associated AWS
    --     resources, such as Auto Scaling group and LoadBalancer, are
    --     terminated.
    --
    -- -   @false@: AWS Elastic Beanstalk resource management is removed from
    --     the environment, but the AWS resources continue to operate.
    --
    -- For more information, see the
    -- <https://docs.aws.amazon.com/elasticbeanstalk/latest/ug/ AWS Elastic Beanstalk User Guide.>
    --
    -- Default: @true@
    --
    -- Valid Values: @true@ | @false@
    terminateResources :: Prelude.Maybe Prelude.Bool,
    -- | The name of the environment to terminate.
    --
    -- Condition: You must specify either this or an EnvironmentId, or both. If
    -- you do not specify either, AWS Elastic Beanstalk returns
    -- @MissingRequiredParameter@ error.
    environmentName :: Prelude.Maybe Prelude.Text,
    -- | The ID of the environment to terminate.
    --
    -- Condition: You must specify either this or an EnvironmentName, or both.
    -- If you do not specify either, AWS Elastic Beanstalk returns
    -- @MissingRequiredParameter@ error.
    environmentId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TerminateEnvironment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'forceTerminate', 'terminateEnvironment_forceTerminate' - Terminates the target environment even if another environment in the
-- same group is dependent on it.
--
-- 'terminateResources', 'terminateEnvironment_terminateResources' - Indicates whether the associated AWS resources should shut down when the
-- environment is terminated:
--
-- -   @true@: The specified environment as well as the associated AWS
--     resources, such as Auto Scaling group and LoadBalancer, are
--     terminated.
--
-- -   @false@: AWS Elastic Beanstalk resource management is removed from
--     the environment, but the AWS resources continue to operate.
--
-- For more information, see the
-- <https://docs.aws.amazon.com/elasticbeanstalk/latest/ug/ AWS Elastic Beanstalk User Guide.>
--
-- Default: @true@
--
-- Valid Values: @true@ | @false@
--
-- 'environmentName', 'terminateEnvironment_environmentName' - The name of the environment to terminate.
--
-- Condition: You must specify either this or an EnvironmentId, or both. If
-- you do not specify either, AWS Elastic Beanstalk returns
-- @MissingRequiredParameter@ error.
--
-- 'environmentId', 'terminateEnvironment_environmentId' - The ID of the environment to terminate.
--
-- Condition: You must specify either this or an EnvironmentName, or both.
-- If you do not specify either, AWS Elastic Beanstalk returns
-- @MissingRequiredParameter@ error.
newTerminateEnvironment ::
  TerminateEnvironment
newTerminateEnvironment =
  TerminateEnvironment'
    { forceTerminate =
        Prelude.Nothing,
      terminateResources = Prelude.Nothing,
      environmentName = Prelude.Nothing,
      environmentId = Prelude.Nothing
    }

-- | Terminates the target environment even if another environment in the
-- same group is dependent on it.
terminateEnvironment_forceTerminate :: Lens.Lens' TerminateEnvironment (Prelude.Maybe Prelude.Bool)
terminateEnvironment_forceTerminate = Lens.lens (\TerminateEnvironment' {forceTerminate} -> forceTerminate) (\s@TerminateEnvironment' {} a -> s {forceTerminate = a} :: TerminateEnvironment)

-- | Indicates whether the associated AWS resources should shut down when the
-- environment is terminated:
--
-- -   @true@: The specified environment as well as the associated AWS
--     resources, such as Auto Scaling group and LoadBalancer, are
--     terminated.
--
-- -   @false@: AWS Elastic Beanstalk resource management is removed from
--     the environment, but the AWS resources continue to operate.
--
-- For more information, see the
-- <https://docs.aws.amazon.com/elasticbeanstalk/latest/ug/ AWS Elastic Beanstalk User Guide.>
--
-- Default: @true@
--
-- Valid Values: @true@ | @false@
terminateEnvironment_terminateResources :: Lens.Lens' TerminateEnvironment (Prelude.Maybe Prelude.Bool)
terminateEnvironment_terminateResources = Lens.lens (\TerminateEnvironment' {terminateResources} -> terminateResources) (\s@TerminateEnvironment' {} a -> s {terminateResources = a} :: TerminateEnvironment)

-- | The name of the environment to terminate.
--
-- Condition: You must specify either this or an EnvironmentId, or both. If
-- you do not specify either, AWS Elastic Beanstalk returns
-- @MissingRequiredParameter@ error.
terminateEnvironment_environmentName :: Lens.Lens' TerminateEnvironment (Prelude.Maybe Prelude.Text)
terminateEnvironment_environmentName = Lens.lens (\TerminateEnvironment' {environmentName} -> environmentName) (\s@TerminateEnvironment' {} a -> s {environmentName = a} :: TerminateEnvironment)

-- | The ID of the environment to terminate.
--
-- Condition: You must specify either this or an EnvironmentName, or both.
-- If you do not specify either, AWS Elastic Beanstalk returns
-- @MissingRequiredParameter@ error.
terminateEnvironment_environmentId :: Lens.Lens' TerminateEnvironment (Prelude.Maybe Prelude.Text)
terminateEnvironment_environmentId = Lens.lens (\TerminateEnvironment' {environmentId} -> environmentId) (\s@TerminateEnvironment' {} a -> s {environmentId = a} :: TerminateEnvironment)

instance Core.AWSRequest TerminateEnvironment where
  type
    AWSResponse TerminateEnvironment =
      EnvironmentDescription
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "TerminateEnvironmentResult"
      (\s h x -> Core.parseXML x)

instance Prelude.Hashable TerminateEnvironment

instance Prelude.NFData TerminateEnvironment

instance Core.ToHeaders TerminateEnvironment where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath TerminateEnvironment where
  toPath = Prelude.const "/"

instance Core.ToQuery TerminateEnvironment where
  toQuery TerminateEnvironment' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("TerminateEnvironment" :: Prelude.ByteString),
        "Version"
          Core.=: ("2010-12-01" :: Prelude.ByteString),
        "ForceTerminate" Core.=: forceTerminate,
        "TerminateResources" Core.=: terminateResources,
        "EnvironmentName" Core.=: environmentName,
        "EnvironmentId" Core.=: environmentId
      ]
