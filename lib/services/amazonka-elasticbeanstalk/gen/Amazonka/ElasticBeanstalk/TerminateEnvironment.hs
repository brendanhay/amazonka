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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Terminates the specified environment.
module Amazonka.ElasticBeanstalk.TerminateEnvironment
  ( -- * Creating a Request
    TerminateEnvironment (..),
    newTerminateEnvironment,

    -- * Request Lenses
    terminateEnvironment_environmentId,
    terminateEnvironment_environmentName,
    terminateEnvironment_forceTerminate,
    terminateEnvironment_terminateResources,

    -- * Destructuring the Response
    EnvironmentDescription (..),
    newEnvironmentDescription,

    -- * Response Lenses
    environmentDescription_abortableOperationInProgress,
    environmentDescription_applicationName,
    environmentDescription_cname,
    environmentDescription_dateCreated,
    environmentDescription_dateUpdated,
    environmentDescription_description,
    environmentDescription_endpointURL,
    environmentDescription_environmentArn,
    environmentDescription_environmentId,
    environmentDescription_environmentLinks,
    environmentDescription_environmentName,
    environmentDescription_health,
    environmentDescription_healthStatus,
    environmentDescription_operationsRole,
    environmentDescription_platformArn,
    environmentDescription_resources,
    environmentDescription_solutionStackName,
    environmentDescription_status,
    environmentDescription_templateName,
    environmentDescription_tier,
    environmentDescription_versionLabel,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElasticBeanstalk.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Request to terminate an environment.
--
-- /See:/ 'newTerminateEnvironment' smart constructor.
data TerminateEnvironment = TerminateEnvironment'
  { -- | The ID of the environment to terminate.
    --
    -- Condition: You must specify either this or an EnvironmentName, or both.
    -- If you do not specify either, AWS Elastic Beanstalk returns
    -- @MissingRequiredParameter@ error.
    environmentId :: Prelude.Maybe Prelude.Text,
    -- | The name of the environment to terminate.
    --
    -- Condition: You must specify either this or an EnvironmentId, or both. If
    -- you do not specify either, AWS Elastic Beanstalk returns
    -- @MissingRequiredParameter@ error.
    environmentName :: Prelude.Maybe Prelude.Text,
    -- | Terminates the target environment even if another environment in the
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
    terminateResources :: Prelude.Maybe Prelude.Bool
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
-- 'environmentId', 'terminateEnvironment_environmentId' - The ID of the environment to terminate.
--
-- Condition: You must specify either this or an EnvironmentName, or both.
-- If you do not specify either, AWS Elastic Beanstalk returns
-- @MissingRequiredParameter@ error.
--
-- 'environmentName', 'terminateEnvironment_environmentName' - The name of the environment to terminate.
--
-- Condition: You must specify either this or an EnvironmentId, or both. If
-- you do not specify either, AWS Elastic Beanstalk returns
-- @MissingRequiredParameter@ error.
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
newTerminateEnvironment ::
  TerminateEnvironment
newTerminateEnvironment =
  TerminateEnvironment'
    { environmentId =
        Prelude.Nothing,
      environmentName = Prelude.Nothing,
      forceTerminate = Prelude.Nothing,
      terminateResources = Prelude.Nothing
    }

-- | The ID of the environment to terminate.
--
-- Condition: You must specify either this or an EnvironmentName, or both.
-- If you do not specify either, AWS Elastic Beanstalk returns
-- @MissingRequiredParameter@ error.
terminateEnvironment_environmentId :: Lens.Lens' TerminateEnvironment (Prelude.Maybe Prelude.Text)
terminateEnvironment_environmentId = Lens.lens (\TerminateEnvironment' {environmentId} -> environmentId) (\s@TerminateEnvironment' {} a -> s {environmentId = a} :: TerminateEnvironment)

-- | The name of the environment to terminate.
--
-- Condition: You must specify either this or an EnvironmentId, or both. If
-- you do not specify either, AWS Elastic Beanstalk returns
-- @MissingRequiredParameter@ error.
terminateEnvironment_environmentName :: Lens.Lens' TerminateEnvironment (Prelude.Maybe Prelude.Text)
terminateEnvironment_environmentName = Lens.lens (\TerminateEnvironment' {environmentName} -> environmentName) (\s@TerminateEnvironment' {} a -> s {environmentName = a} :: TerminateEnvironment)

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

instance Core.AWSRequest TerminateEnvironment where
  type
    AWSResponse TerminateEnvironment =
      EnvironmentDescription
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "TerminateEnvironmentResult"
      (\s h x -> Data.parseXML x)

instance Prelude.Hashable TerminateEnvironment where
  hashWithSalt _salt TerminateEnvironment' {..} =
    _salt
      `Prelude.hashWithSalt` environmentId
      `Prelude.hashWithSalt` environmentName
      `Prelude.hashWithSalt` forceTerminate
      `Prelude.hashWithSalt` terminateResources

instance Prelude.NFData TerminateEnvironment where
  rnf TerminateEnvironment' {..} =
    Prelude.rnf environmentId
      `Prelude.seq` Prelude.rnf environmentName
      `Prelude.seq` Prelude.rnf forceTerminate
      `Prelude.seq` Prelude.rnf terminateResources

instance Data.ToHeaders TerminateEnvironment where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath TerminateEnvironment where
  toPath = Prelude.const "/"

instance Data.ToQuery TerminateEnvironment where
  toQuery TerminateEnvironment' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("TerminateEnvironment" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-12-01" :: Prelude.ByteString),
        "EnvironmentId" Data.=: environmentId,
        "EnvironmentName" Data.=: environmentName,
        "ForceTerminate" Data.=: forceTerminate,
        "TerminateResources" Data.=: terminateResources
      ]
