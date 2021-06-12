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
-- Module      : Network.AWS.ElasticBeanstalk.DeleteEnvironmentConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the draft configuration associated with the running environment.
--
-- Updating a running environment with any configuration changes creates a
-- draft configuration set. You can get the draft configuration using
-- DescribeConfigurationSettings while the update is in progress or if the
-- update fails. The @DeploymentStatus@ for the draft configuration
-- indicates whether the deployment is in process or has failed. The draft
-- configuration remains in existence until it is deleted with this action.
module Network.AWS.ElasticBeanstalk.DeleteEnvironmentConfiguration
  ( -- * Creating a Request
    DeleteEnvironmentConfiguration (..),
    newDeleteEnvironmentConfiguration,

    -- * Request Lenses
    deleteEnvironmentConfiguration_applicationName,
    deleteEnvironmentConfiguration_environmentName,

    -- * Destructuring the Response
    DeleteEnvironmentConfigurationResponse (..),
    newDeleteEnvironmentConfigurationResponse,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ElasticBeanstalk.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request to delete a draft environment configuration.
--
-- /See:/ 'newDeleteEnvironmentConfiguration' smart constructor.
data DeleteEnvironmentConfiguration = DeleteEnvironmentConfiguration'
  { -- | The name of the application the environment is associated with.
    applicationName :: Core.Text,
    -- | The name of the environment to delete the draft configuration from.
    environmentName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteEnvironmentConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationName', 'deleteEnvironmentConfiguration_applicationName' - The name of the application the environment is associated with.
--
-- 'environmentName', 'deleteEnvironmentConfiguration_environmentName' - The name of the environment to delete the draft configuration from.
newDeleteEnvironmentConfiguration ::
  -- | 'applicationName'
  Core.Text ->
  -- | 'environmentName'
  Core.Text ->
  DeleteEnvironmentConfiguration
newDeleteEnvironmentConfiguration
  pApplicationName_
  pEnvironmentName_ =
    DeleteEnvironmentConfiguration'
      { applicationName =
          pApplicationName_,
        environmentName = pEnvironmentName_
      }

-- | The name of the application the environment is associated with.
deleteEnvironmentConfiguration_applicationName :: Lens.Lens' DeleteEnvironmentConfiguration Core.Text
deleteEnvironmentConfiguration_applicationName = Lens.lens (\DeleteEnvironmentConfiguration' {applicationName} -> applicationName) (\s@DeleteEnvironmentConfiguration' {} a -> s {applicationName = a} :: DeleteEnvironmentConfiguration)

-- | The name of the environment to delete the draft configuration from.
deleteEnvironmentConfiguration_environmentName :: Lens.Lens' DeleteEnvironmentConfiguration Core.Text
deleteEnvironmentConfiguration_environmentName = Lens.lens (\DeleteEnvironmentConfiguration' {environmentName} -> environmentName) (\s@DeleteEnvironmentConfiguration' {} a -> s {environmentName = a} :: DeleteEnvironmentConfiguration)

instance
  Core.AWSRequest
    DeleteEnvironmentConfiguration
  where
  type
    AWSResponse DeleteEnvironmentConfiguration =
      DeleteEnvironmentConfigurationResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull
      DeleteEnvironmentConfigurationResponse'

instance Core.Hashable DeleteEnvironmentConfiguration

instance Core.NFData DeleteEnvironmentConfiguration

instance
  Core.ToHeaders
    DeleteEnvironmentConfiguration
  where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DeleteEnvironmentConfiguration where
  toPath = Core.const "/"

instance Core.ToQuery DeleteEnvironmentConfiguration where
  toQuery DeleteEnvironmentConfiguration' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ( "DeleteEnvironmentConfiguration" ::
                      Core.ByteString
                  ),
        "Version" Core.=: ("2010-12-01" :: Core.ByteString),
        "ApplicationName" Core.=: applicationName,
        "EnvironmentName" Core.=: environmentName
      ]

-- | /See:/ 'newDeleteEnvironmentConfigurationResponse' smart constructor.
data DeleteEnvironmentConfigurationResponse = DeleteEnvironmentConfigurationResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteEnvironmentConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteEnvironmentConfigurationResponse ::
  DeleteEnvironmentConfigurationResponse
newDeleteEnvironmentConfigurationResponse =
  DeleteEnvironmentConfigurationResponse'

instance
  Core.NFData
    DeleteEnvironmentConfigurationResponse
