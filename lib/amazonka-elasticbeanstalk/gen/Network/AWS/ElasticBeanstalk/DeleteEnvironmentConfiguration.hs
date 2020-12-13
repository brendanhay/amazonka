{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.DeleteEnvironmentConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the draft configuration associated with the running environment.
--
-- Updating a running environment with any configuration changes creates a draft configuration set. You can get the draft configuration using 'DescribeConfigurationSettings' while the update is in progress or if the update fails. The @DeploymentStatus@ for the draft configuration indicates whether the deployment is in process or has failed. The draft configuration remains in existence until it is deleted with this action.
module Network.AWS.ElasticBeanstalk.DeleteEnvironmentConfiguration
  ( -- * Creating a request
    DeleteEnvironmentConfiguration (..),
    mkDeleteEnvironmentConfiguration,

    -- ** Request lenses
    decEnvironmentName,
    decApplicationName,

    -- * Destructuring the response
    DeleteEnvironmentConfigurationResponse (..),
    mkDeleteEnvironmentConfigurationResponse,
  )
where

import Network.AWS.ElasticBeanstalk.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Request to delete a draft environment configuration.
--
-- /See:/ 'mkDeleteEnvironmentConfiguration' smart constructor.
data DeleteEnvironmentConfiguration = DeleteEnvironmentConfiguration'
  { -- | The name of the environment to delete the draft configuration from.
    environmentName :: Lude.Text,
    -- | The name of the application the environment is associated with.
    applicationName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteEnvironmentConfiguration' with the minimum fields required to make a request.
--
-- * 'environmentName' - The name of the environment to delete the draft configuration from.
-- * 'applicationName' - The name of the application the environment is associated with.
mkDeleteEnvironmentConfiguration ::
  -- | 'environmentName'
  Lude.Text ->
  -- | 'applicationName'
  Lude.Text ->
  DeleteEnvironmentConfiguration
mkDeleteEnvironmentConfiguration
  pEnvironmentName_
  pApplicationName_ =
    DeleteEnvironmentConfiguration'
      { environmentName =
          pEnvironmentName_,
        applicationName = pApplicationName_
      }

-- | The name of the environment to delete the draft configuration from.
--
-- /Note:/ Consider using 'environmentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
decEnvironmentName :: Lens.Lens' DeleteEnvironmentConfiguration Lude.Text
decEnvironmentName = Lens.lens (environmentName :: DeleteEnvironmentConfiguration -> Lude.Text) (\s a -> s {environmentName = a} :: DeleteEnvironmentConfiguration)
{-# DEPRECATED decEnvironmentName "Use generic-lens or generic-optics with 'environmentName' instead." #-}

-- | The name of the application the environment is associated with.
--
-- /Note:/ Consider using 'applicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
decApplicationName :: Lens.Lens' DeleteEnvironmentConfiguration Lude.Text
decApplicationName = Lens.lens (applicationName :: DeleteEnvironmentConfiguration -> Lude.Text) (\s a -> s {applicationName = a} :: DeleteEnvironmentConfiguration)
{-# DEPRECATED decApplicationName "Use generic-lens or generic-optics with 'applicationName' instead." #-}

instance Lude.AWSRequest DeleteEnvironmentConfiguration where
  type
    Rs DeleteEnvironmentConfiguration =
      DeleteEnvironmentConfigurationResponse
  request = Req.postQuery elasticBeanstalkService
  response = Res.receiveNull DeleteEnvironmentConfigurationResponse'

instance Lude.ToHeaders DeleteEnvironmentConfiguration where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteEnvironmentConfiguration where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteEnvironmentConfiguration where
  toQuery DeleteEnvironmentConfiguration' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DeleteEnvironmentConfiguration" :: Lude.ByteString),
        "Version" Lude.=: ("2010-12-01" :: Lude.ByteString),
        "EnvironmentName" Lude.=: environmentName,
        "ApplicationName" Lude.=: applicationName
      ]

-- | /See:/ 'mkDeleteEnvironmentConfigurationResponse' smart constructor.
data DeleteEnvironmentConfigurationResponse = DeleteEnvironmentConfigurationResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteEnvironmentConfigurationResponse' with the minimum fields required to make a request.
mkDeleteEnvironmentConfigurationResponse ::
  DeleteEnvironmentConfigurationResponse
mkDeleteEnvironmentConfigurationResponse =
  DeleteEnvironmentConfigurationResponse'
