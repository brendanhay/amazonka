{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.ComposeEnvironments
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create or update a group of environments that each run a separate component of a single application. Takes a list of version labels that specify application source bundles for each of the environments to create or update. The name of each environment and other required information must be included in the source bundles in an environment manifest named @env.yaml@ . See <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/environment-mgmt-compose.html Compose Environments> for details.
module Network.AWS.ElasticBeanstalk.ComposeEnvironments
  ( -- * Creating a request
    ComposeEnvironments (..),
    mkComposeEnvironments,

    -- ** Request lenses
    ceVersionLabels,
    ceApplicationName,
    ceGroupName,

    -- * Destructuring the response
    EnvironmentDescriptionsMessage (..),
    mkEnvironmentDescriptionsMessage,

    -- ** Response lenses
    edmNextToken,
    edmEnvironments,
  )
where

import Network.AWS.ElasticBeanstalk.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Request to create or update a group of environments.
--
-- /See:/ 'mkComposeEnvironments' smart constructor.
data ComposeEnvironments = ComposeEnvironments'
  { versionLabels ::
      Lude.Maybe [Lude.Text],
    applicationName :: Lude.Maybe Lude.Text,
    groupName :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ComposeEnvironments' with the minimum fields required to make a request.
--
-- * 'applicationName' - The name of the application to which the specified source bundles belong.
-- * 'groupName' - The name of the group to which the target environments belong. Specify a group name only if the environment name defined in each target environment's manifest ends with a + (plus) character. See <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/environment-cfg-manifest.html Environment Manifest (env.yaml)> for details.
-- * 'versionLabels' - A list of version labels, specifying one or more application source bundles that belong to the target application. Each source bundle must include an environment manifest that specifies the name of the environment and the name of the solution stack to use, and optionally can specify environment links to create.
mkComposeEnvironments ::
  ComposeEnvironments
mkComposeEnvironments =
  ComposeEnvironments'
    { versionLabels = Lude.Nothing,
      applicationName = Lude.Nothing,
      groupName = Lude.Nothing
    }

-- | A list of version labels, specifying one or more application source bundles that belong to the target application. Each source bundle must include an environment manifest that specifies the name of the environment and the name of the solution stack to use, and optionally can specify environment links to create.
--
-- /Note:/ Consider using 'versionLabels' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceVersionLabels :: Lens.Lens' ComposeEnvironments (Lude.Maybe [Lude.Text])
ceVersionLabels = Lens.lens (versionLabels :: ComposeEnvironments -> Lude.Maybe [Lude.Text]) (\s a -> s {versionLabels = a} :: ComposeEnvironments)
{-# DEPRECATED ceVersionLabels "Use generic-lens or generic-optics with 'versionLabels' instead." #-}

-- | The name of the application to which the specified source bundles belong.
--
-- /Note:/ Consider using 'applicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceApplicationName :: Lens.Lens' ComposeEnvironments (Lude.Maybe Lude.Text)
ceApplicationName = Lens.lens (applicationName :: ComposeEnvironments -> Lude.Maybe Lude.Text) (\s a -> s {applicationName = a} :: ComposeEnvironments)
{-# DEPRECATED ceApplicationName "Use generic-lens or generic-optics with 'applicationName' instead." #-}

-- | The name of the group to which the target environments belong. Specify a group name only if the environment name defined in each target environment's manifest ends with a + (plus) character. See <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/environment-cfg-manifest.html Environment Manifest (env.yaml)> for details.
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceGroupName :: Lens.Lens' ComposeEnvironments (Lude.Maybe Lude.Text)
ceGroupName = Lens.lens (groupName :: ComposeEnvironments -> Lude.Maybe Lude.Text) (\s a -> s {groupName = a} :: ComposeEnvironments)
{-# DEPRECATED ceGroupName "Use generic-lens or generic-optics with 'groupName' instead." #-}

instance Lude.AWSRequest ComposeEnvironments where
  type Rs ComposeEnvironments = EnvironmentDescriptionsMessage
  request = Req.postQuery elasticBeanstalkService
  response =
    Res.receiveXMLWrapper
      "ComposeEnvironmentsResult"
      (\s h x -> Lude.parseXML x)

instance Lude.ToHeaders ComposeEnvironments where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ComposeEnvironments where
  toPath = Lude.const "/"

instance Lude.ToQuery ComposeEnvironments where
  toQuery ComposeEnvironments' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("ComposeEnvironments" :: Lude.ByteString),
        "Version" Lude.=: ("2010-12-01" :: Lude.ByteString),
        "VersionLabels"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> versionLabels),
        "ApplicationName" Lude.=: applicationName,
        "GroupName" Lude.=: groupName
      ]
