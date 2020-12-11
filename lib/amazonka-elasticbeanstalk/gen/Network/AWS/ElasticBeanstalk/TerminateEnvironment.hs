{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.TerminateEnvironment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Terminates the specified environment.
module Network.AWS.ElasticBeanstalk.TerminateEnvironment
  ( -- * Creating a request
    TerminateEnvironment (..),
    mkTerminateEnvironment,

    -- ** Request lenses
    teForceTerminate,
    teTerminateResources,
    teEnvironmentName,
    teEnvironmentId,

    -- * Destructuring the response
    EnvironmentDescription (..),
    mkEnvironmentDescription,

    -- ** Response lenses
    eStatus,
    eCNAME,
    eTemplateName,
    eAbortableOperationInProgress,
    eEndpointURL,
    eResources,
    eDateUpdated,
    eDateCreated,
    eHealth,
    eVersionLabel,
    eOperationsRole,
    ePlatformARN,
    eTier,
    eEnvironmentName,
    eApplicationName,
    eEnvironmentARN,
    eSolutionStackName,
    eEnvironmentId,
    eHealthStatus,
    eEnvironmentLinks,
    eDescription,
  )
where

import Network.AWS.ElasticBeanstalk.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Request to terminate an environment.
--
-- /See:/ 'mkTerminateEnvironment' smart constructor.
data TerminateEnvironment = TerminateEnvironment'
  { forceTerminate ::
      Lude.Maybe Lude.Bool,
    terminateResources :: Lude.Maybe Lude.Bool,
    environmentName :: Lude.Maybe Lude.Text,
    environmentId :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TerminateEnvironment' with the minimum fields required to make a request.
--
-- * 'environmentId' - The ID of the environment to terminate.
--
-- Condition: You must specify either this or an EnvironmentName, or both. If you do not specify either, AWS Elastic Beanstalk returns @MissingRequiredParameter@ error.
-- * 'environmentName' - The name of the environment to terminate.
--
-- Condition: You must specify either this or an EnvironmentId, or both. If you do not specify either, AWS Elastic Beanstalk returns @MissingRequiredParameter@ error.
-- * 'forceTerminate' - Terminates the target environment even if another environment in the same group is dependent on it.
-- * 'terminateResources' - Indicates whether the associated AWS resources should shut down when the environment is terminated:
--
--
--     * @true@ : The specified environment as well as the associated AWS resources, such as Auto Scaling group and LoadBalancer, are terminated.
--
--
--     * @false@ : AWS Elastic Beanstalk resource management is removed from the environment, but the AWS resources continue to operate.
--
--
-- For more information, see the <https://docs.aws.amazon.com/elasticbeanstalk/latest/ug/ AWS Elastic Beanstalk User Guide. >
-- Default: @true@
-- Valid Values: @true@ | @false@
mkTerminateEnvironment ::
  TerminateEnvironment
mkTerminateEnvironment =
  TerminateEnvironment'
    { forceTerminate = Lude.Nothing,
      terminateResources = Lude.Nothing,
      environmentName = Lude.Nothing,
      environmentId = Lude.Nothing
    }

-- | Terminates the target environment even if another environment in the same group is dependent on it.
--
-- /Note:/ Consider using 'forceTerminate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
teForceTerminate :: Lens.Lens' TerminateEnvironment (Lude.Maybe Lude.Bool)
teForceTerminate = Lens.lens (forceTerminate :: TerminateEnvironment -> Lude.Maybe Lude.Bool) (\s a -> s {forceTerminate = a} :: TerminateEnvironment)
{-# DEPRECATED teForceTerminate "Use generic-lens or generic-optics with 'forceTerminate' instead." #-}

-- | Indicates whether the associated AWS resources should shut down when the environment is terminated:
--
--
--     * @true@ : The specified environment as well as the associated AWS resources, such as Auto Scaling group and LoadBalancer, are terminated.
--
--
--     * @false@ : AWS Elastic Beanstalk resource management is removed from the environment, but the AWS resources continue to operate.
--
--
-- For more information, see the <https://docs.aws.amazon.com/elasticbeanstalk/latest/ug/ AWS Elastic Beanstalk User Guide. >
-- Default: @true@
-- Valid Values: @true@ | @false@
--
-- /Note:/ Consider using 'terminateResources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
teTerminateResources :: Lens.Lens' TerminateEnvironment (Lude.Maybe Lude.Bool)
teTerminateResources = Lens.lens (terminateResources :: TerminateEnvironment -> Lude.Maybe Lude.Bool) (\s a -> s {terminateResources = a} :: TerminateEnvironment)
{-# DEPRECATED teTerminateResources "Use generic-lens or generic-optics with 'terminateResources' instead." #-}

-- | The name of the environment to terminate.
--
-- Condition: You must specify either this or an EnvironmentId, or both. If you do not specify either, AWS Elastic Beanstalk returns @MissingRequiredParameter@ error.
--
-- /Note:/ Consider using 'environmentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
teEnvironmentName :: Lens.Lens' TerminateEnvironment (Lude.Maybe Lude.Text)
teEnvironmentName = Lens.lens (environmentName :: TerminateEnvironment -> Lude.Maybe Lude.Text) (\s a -> s {environmentName = a} :: TerminateEnvironment)
{-# DEPRECATED teEnvironmentName "Use generic-lens or generic-optics with 'environmentName' instead." #-}

-- | The ID of the environment to terminate.
--
-- Condition: You must specify either this or an EnvironmentName, or both. If you do not specify either, AWS Elastic Beanstalk returns @MissingRequiredParameter@ error.
--
-- /Note:/ Consider using 'environmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
teEnvironmentId :: Lens.Lens' TerminateEnvironment (Lude.Maybe Lude.Text)
teEnvironmentId = Lens.lens (environmentId :: TerminateEnvironment -> Lude.Maybe Lude.Text) (\s a -> s {environmentId = a} :: TerminateEnvironment)
{-# DEPRECATED teEnvironmentId "Use generic-lens or generic-optics with 'environmentId' instead." #-}

instance Lude.AWSRequest TerminateEnvironment where
  type Rs TerminateEnvironment = EnvironmentDescription
  request = Req.postQuery elasticBeanstalkService
  response =
    Res.receiveXMLWrapper
      "TerminateEnvironmentResult"
      (\s h x -> Lude.parseXML x)

instance Lude.ToHeaders TerminateEnvironment where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath TerminateEnvironment where
  toPath = Lude.const "/"

instance Lude.ToQuery TerminateEnvironment where
  toQuery TerminateEnvironment' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("TerminateEnvironment" :: Lude.ByteString),
        "Version" Lude.=: ("2010-12-01" :: Lude.ByteString),
        "ForceTerminate" Lude.=: forceTerminate,
        "TerminateResources" Lude.=: terminateResources,
        "EnvironmentName" Lude.=: environmentName,
        "EnvironmentId" Lude.=: environmentId
      ]
