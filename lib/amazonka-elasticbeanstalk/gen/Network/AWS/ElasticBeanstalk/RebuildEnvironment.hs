{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.RebuildEnvironment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes and recreates all of the AWS resources (for example: the Auto Scaling group, load balancer, etc.) for a specified environment and forces a restart.
module Network.AWS.ElasticBeanstalk.RebuildEnvironment
  ( -- * Creating a request
    RebuildEnvironment (..),
    mkRebuildEnvironment,

    -- ** Request lenses
    reEnvironmentName,
    reEnvironmentId,

    -- * Destructuring the response
    RebuildEnvironmentResponse (..),
    mkRebuildEnvironmentResponse,
  )
where

import Network.AWS.ElasticBeanstalk.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkRebuildEnvironment' smart constructor.
data RebuildEnvironment = RebuildEnvironment'
  { -- | The name of the environment to rebuild.
    --
    -- Condition: You must specify either this or an EnvironmentId, or both. If you do not specify either, AWS Elastic Beanstalk returns @MissingRequiredParameter@ error.
    environmentName :: Lude.Maybe Lude.Text,
    -- | The ID of the environment to rebuild.
    --
    -- Condition: You must specify either this or an EnvironmentName, or both. If you do not specify either, AWS Elastic Beanstalk returns @MissingRequiredParameter@ error.
    environmentId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RebuildEnvironment' with the minimum fields required to make a request.
--
-- * 'environmentName' - The name of the environment to rebuild.
--
-- Condition: You must specify either this or an EnvironmentId, or both. If you do not specify either, AWS Elastic Beanstalk returns @MissingRequiredParameter@ error.
-- * 'environmentId' - The ID of the environment to rebuild.
--
-- Condition: You must specify either this or an EnvironmentName, or both. If you do not specify either, AWS Elastic Beanstalk returns @MissingRequiredParameter@ error.
mkRebuildEnvironment ::
  RebuildEnvironment
mkRebuildEnvironment =
  RebuildEnvironment'
    { environmentName = Lude.Nothing,
      environmentId = Lude.Nothing
    }

-- | The name of the environment to rebuild.
--
-- Condition: You must specify either this or an EnvironmentId, or both. If you do not specify either, AWS Elastic Beanstalk returns @MissingRequiredParameter@ error.
--
-- /Note:/ Consider using 'environmentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
reEnvironmentName :: Lens.Lens' RebuildEnvironment (Lude.Maybe Lude.Text)
reEnvironmentName = Lens.lens (environmentName :: RebuildEnvironment -> Lude.Maybe Lude.Text) (\s a -> s {environmentName = a} :: RebuildEnvironment)
{-# DEPRECATED reEnvironmentName "Use generic-lens or generic-optics with 'environmentName' instead." #-}

-- | The ID of the environment to rebuild.
--
-- Condition: You must specify either this or an EnvironmentName, or both. If you do not specify either, AWS Elastic Beanstalk returns @MissingRequiredParameter@ error.
--
-- /Note:/ Consider using 'environmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
reEnvironmentId :: Lens.Lens' RebuildEnvironment (Lude.Maybe Lude.Text)
reEnvironmentId = Lens.lens (environmentId :: RebuildEnvironment -> Lude.Maybe Lude.Text) (\s a -> s {environmentId = a} :: RebuildEnvironment)
{-# DEPRECATED reEnvironmentId "Use generic-lens or generic-optics with 'environmentId' instead." #-}

instance Lude.AWSRequest RebuildEnvironment where
  type Rs RebuildEnvironment = RebuildEnvironmentResponse
  request = Req.postQuery elasticBeanstalkService
  response = Res.receiveNull RebuildEnvironmentResponse'

instance Lude.ToHeaders RebuildEnvironment where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath RebuildEnvironment where
  toPath = Lude.const "/"

instance Lude.ToQuery RebuildEnvironment where
  toQuery RebuildEnvironment' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("RebuildEnvironment" :: Lude.ByteString),
        "Version" Lude.=: ("2010-12-01" :: Lude.ByteString),
        "EnvironmentName" Lude.=: environmentName,
        "EnvironmentId" Lude.=: environmentId
      ]

-- | /See:/ 'mkRebuildEnvironmentResponse' smart constructor.
data RebuildEnvironmentResponse = RebuildEnvironmentResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RebuildEnvironmentResponse' with the minimum fields required to make a request.
mkRebuildEnvironmentResponse ::
  RebuildEnvironmentResponse
mkRebuildEnvironmentResponse = RebuildEnvironmentResponse'
