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
-- Module      : Network.AWS.ElasticBeanstalk.TerminateEnvironment
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Terminates the specified environment.
module Network.AWS.ElasticBeanstalk.TerminateEnvironment
  ( -- * Creating a Request
    TerminateEnvironment (..),
    newTerminateEnvironment,

    -- * Request Lenses
    terminateEnvironment_forceTerminate,
    terminateEnvironment_environmentId,
    terminateEnvironment_terminateResources,
    terminateEnvironment_environmentName,

    -- * Destructuring the Response
    EnvironmentDescription (..),
    newEnvironmentDescription,

    -- * Response Lenses
    environmentDescription_abortableOperationInProgress,
    environmentDescription_templateName,
    environmentDescription_status,
    environmentDescription_dateCreated,
    environmentDescription_environmentLinks,
    environmentDescription_solutionStackName,
    environmentDescription_environmentId,
    environmentDescription_environmentName,
    environmentDescription_platformArn,
    environmentDescription_versionLabel,
    environmentDescription_health,
    environmentDescription_cname,
    environmentDescription_resources,
    environmentDescription_dateUpdated,
    environmentDescription_description,
    environmentDescription_healthStatus,
    environmentDescription_environmentArn,
    environmentDescription_endpointURL,
    environmentDescription_applicationName,
    environmentDescription_tier,
    environmentDescription_operationsRole,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ElasticBeanstalk.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request to terminate an environment.
--
-- /See:/ 'newTerminateEnvironment' smart constructor.
data TerminateEnvironment = TerminateEnvironment'
  { -- | Terminates the target environment even if another environment in the
    -- same group is dependent on it.
    forceTerminate :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the environment to terminate.
    --
    -- Condition: You must specify either this or an EnvironmentName, or both.
    -- If you do not specify either, AWS Elastic Beanstalk returns
    -- @MissingRequiredParameter@ error.
    environmentId :: Prelude.Maybe Prelude.Text,
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
    environmentName :: Prelude.Maybe Prelude.Text
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
-- 'environmentId', 'terminateEnvironment_environmentId' - The ID of the environment to terminate.
--
-- Condition: You must specify either this or an EnvironmentName, or both.
-- If you do not specify either, AWS Elastic Beanstalk returns
-- @MissingRequiredParameter@ error.
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
newTerminateEnvironment ::
  TerminateEnvironment
newTerminateEnvironment =
  TerminateEnvironment'
    { forceTerminate =
        Prelude.Nothing,
      environmentId = Prelude.Nothing,
      terminateResources = Prelude.Nothing,
      environmentName = Prelude.Nothing
    }

-- | Terminates the target environment even if another environment in the
-- same group is dependent on it.
terminateEnvironment_forceTerminate :: Lens.Lens' TerminateEnvironment (Prelude.Maybe Prelude.Bool)
terminateEnvironment_forceTerminate = Lens.lens (\TerminateEnvironment' {forceTerminate} -> forceTerminate) (\s@TerminateEnvironment' {} a -> s {forceTerminate = a} :: TerminateEnvironment)

-- | The ID of the environment to terminate.
--
-- Condition: You must specify either this or an EnvironmentName, or both.
-- If you do not specify either, AWS Elastic Beanstalk returns
-- @MissingRequiredParameter@ error.
terminateEnvironment_environmentId :: Lens.Lens' TerminateEnvironment (Prelude.Maybe Prelude.Text)
terminateEnvironment_environmentId = Lens.lens (\TerminateEnvironment' {environmentId} -> environmentId) (\s@TerminateEnvironment' {} a -> s {environmentId = a} :: TerminateEnvironment)

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
        "EnvironmentId" Core.=: environmentId,
        "TerminateResources" Core.=: terminateResources,
        "EnvironmentName" Core.=: environmentName
      ]
