{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.RestartAppServer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Causes the environment to restart the application container server running on each Amazon EC2 instance.
module Network.AWS.ElasticBeanstalk.RestartAppServer
  ( -- * Creating a request
    RestartAppServer (..),
    mkRestartAppServer,

    -- ** Request lenses
    rasEnvironmentName,
    rasEnvironmentId,

    -- * Destructuring the response
    RestartAppServerResponse (..),
    mkRestartAppServerResponse,
  )
where

import Network.AWS.ElasticBeanstalk.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkRestartAppServer' smart constructor.
data RestartAppServer = RestartAppServer'
  { -- | The name of the environment to restart the server for.
    --
    -- Condition: You must specify either this or an EnvironmentId, or both. If you do not specify either, AWS Elastic Beanstalk returns @MissingRequiredParameter@ error.
    environmentName :: Lude.Maybe Lude.Text,
    -- | The ID of the environment to restart the server for.
    --
    -- Condition: You must specify either this or an EnvironmentName, or both. If you do not specify either, AWS Elastic Beanstalk returns @MissingRequiredParameter@ error.
    environmentId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RestartAppServer' with the minimum fields required to make a request.
--
-- * 'environmentName' - The name of the environment to restart the server for.
--
-- Condition: You must specify either this or an EnvironmentId, or both. If you do not specify either, AWS Elastic Beanstalk returns @MissingRequiredParameter@ error.
-- * 'environmentId' - The ID of the environment to restart the server for.
--
-- Condition: You must specify either this or an EnvironmentName, or both. If you do not specify either, AWS Elastic Beanstalk returns @MissingRequiredParameter@ error.
mkRestartAppServer ::
  RestartAppServer
mkRestartAppServer =
  RestartAppServer'
    { environmentName = Lude.Nothing,
      environmentId = Lude.Nothing
    }

-- | The name of the environment to restart the server for.
--
-- Condition: You must specify either this or an EnvironmentId, or both. If you do not specify either, AWS Elastic Beanstalk returns @MissingRequiredParameter@ error.
--
-- /Note:/ Consider using 'environmentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rasEnvironmentName :: Lens.Lens' RestartAppServer (Lude.Maybe Lude.Text)
rasEnvironmentName = Lens.lens (environmentName :: RestartAppServer -> Lude.Maybe Lude.Text) (\s a -> s {environmentName = a} :: RestartAppServer)
{-# DEPRECATED rasEnvironmentName "Use generic-lens or generic-optics with 'environmentName' instead." #-}

-- | The ID of the environment to restart the server for.
--
-- Condition: You must specify either this or an EnvironmentName, or both. If you do not specify either, AWS Elastic Beanstalk returns @MissingRequiredParameter@ error.
--
-- /Note:/ Consider using 'environmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rasEnvironmentId :: Lens.Lens' RestartAppServer (Lude.Maybe Lude.Text)
rasEnvironmentId = Lens.lens (environmentId :: RestartAppServer -> Lude.Maybe Lude.Text) (\s a -> s {environmentId = a} :: RestartAppServer)
{-# DEPRECATED rasEnvironmentId "Use generic-lens or generic-optics with 'environmentId' instead." #-}

instance Lude.AWSRequest RestartAppServer where
  type Rs RestartAppServer = RestartAppServerResponse
  request = Req.postQuery elasticBeanstalkService
  response = Res.receiveNull RestartAppServerResponse'

instance Lude.ToHeaders RestartAppServer where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath RestartAppServer where
  toPath = Lude.const "/"

instance Lude.ToQuery RestartAppServer where
  toQuery RestartAppServer' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("RestartAppServer" :: Lude.ByteString),
        "Version" Lude.=: ("2010-12-01" :: Lude.ByteString),
        "EnvironmentName" Lude.=: environmentName,
        "EnvironmentId" Lude.=: environmentId
      ]

-- | /See:/ 'mkRestartAppServerResponse' smart constructor.
data RestartAppServerResponse = RestartAppServerResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RestartAppServerResponse' with the minimum fields required to make a request.
mkRestartAppServerResponse ::
  RestartAppServerResponse
mkRestartAppServerResponse = RestartAppServerResponse'
