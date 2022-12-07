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
-- Module      : Amazonka.ElasticBeanstalk.DeleteEnvironmentConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
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
module Amazonka.ElasticBeanstalk.DeleteEnvironmentConfiguration
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElasticBeanstalk.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Request to delete a draft environment configuration.
--
-- /See:/ 'newDeleteEnvironmentConfiguration' smart constructor.
data DeleteEnvironmentConfiguration = DeleteEnvironmentConfiguration'
  { -- | The name of the application the environment is associated with.
    applicationName :: Prelude.Text,
    -- | The name of the environment to delete the draft configuration from.
    environmentName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'environmentName'
  Prelude.Text ->
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
deleteEnvironmentConfiguration_applicationName :: Lens.Lens' DeleteEnvironmentConfiguration Prelude.Text
deleteEnvironmentConfiguration_applicationName = Lens.lens (\DeleteEnvironmentConfiguration' {applicationName} -> applicationName) (\s@DeleteEnvironmentConfiguration' {} a -> s {applicationName = a} :: DeleteEnvironmentConfiguration)

-- | The name of the environment to delete the draft configuration from.
deleteEnvironmentConfiguration_environmentName :: Lens.Lens' DeleteEnvironmentConfiguration Prelude.Text
deleteEnvironmentConfiguration_environmentName = Lens.lens (\DeleteEnvironmentConfiguration' {environmentName} -> environmentName) (\s@DeleteEnvironmentConfiguration' {} a -> s {environmentName = a} :: DeleteEnvironmentConfiguration)

instance
  Core.AWSRequest
    DeleteEnvironmentConfiguration
  where
  type
    AWSResponse DeleteEnvironmentConfiguration =
      DeleteEnvironmentConfigurationResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveNull
      DeleteEnvironmentConfigurationResponse'

instance
  Prelude.Hashable
    DeleteEnvironmentConfiguration
  where
  hashWithSalt
    _salt
    DeleteEnvironmentConfiguration' {..} =
      _salt `Prelude.hashWithSalt` applicationName
        `Prelude.hashWithSalt` environmentName

instance
  Prelude.NFData
    DeleteEnvironmentConfiguration
  where
  rnf DeleteEnvironmentConfiguration' {..} =
    Prelude.rnf applicationName
      `Prelude.seq` Prelude.rnf environmentName

instance
  Data.ToHeaders
    DeleteEnvironmentConfiguration
  where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteEnvironmentConfiguration where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteEnvironmentConfiguration where
  toQuery DeleteEnvironmentConfiguration' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "DeleteEnvironmentConfiguration" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2010-12-01" :: Prelude.ByteString),
        "ApplicationName" Data.=: applicationName,
        "EnvironmentName" Data.=: environmentName
      ]

-- | /See:/ 'newDeleteEnvironmentConfigurationResponse' smart constructor.
data DeleteEnvironmentConfigurationResponse = DeleteEnvironmentConfigurationResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteEnvironmentConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteEnvironmentConfigurationResponse ::
  DeleteEnvironmentConfigurationResponse
newDeleteEnvironmentConfigurationResponse =
  DeleteEnvironmentConfigurationResponse'

instance
  Prelude.NFData
    DeleteEnvironmentConfigurationResponse
  where
  rnf _ = ()
