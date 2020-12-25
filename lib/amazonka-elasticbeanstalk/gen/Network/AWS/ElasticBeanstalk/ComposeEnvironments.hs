{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    ceApplicationName,
    ceGroupName,
    ceVersionLabels,

    -- * Destructuring the response
    Types.EnvironmentDescriptionsMessage (..),
    Types.mkEnvironmentDescriptionsMessage,

    -- ** Response lenses
    Types.edmEnvironments,
    Types.edmNextToken,
  )
where

import qualified Network.AWS.ElasticBeanstalk.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request to create or update a group of environments.
--
-- /See:/ 'mkComposeEnvironments' smart constructor.
data ComposeEnvironments = ComposeEnvironments'
  { -- | The name of the application to which the specified source bundles belong.
    applicationName :: Core.Maybe Types.ApplicationName,
    -- | The name of the group to which the target environments belong. Specify a group name only if the environment name defined in each target environment's manifest ends with a + (plus) character. See <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/environment-cfg-manifest.html Environment Manifest (env.yaml)> for details.
    groupName :: Core.Maybe Types.GroupName,
    -- | A list of version labels, specifying one or more application source bundles that belong to the target application. Each source bundle must include an environment manifest that specifies the name of the environment and the name of the solution stack to use, and optionally can specify environment links to create.
    versionLabels :: Core.Maybe [Types.VersionLabel]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ComposeEnvironments' value with any optional fields omitted.
mkComposeEnvironments ::
  ComposeEnvironments
mkComposeEnvironments =
  ComposeEnvironments'
    { applicationName = Core.Nothing,
      groupName = Core.Nothing,
      versionLabels = Core.Nothing
    }

-- | The name of the application to which the specified source bundles belong.
--
-- /Note:/ Consider using 'applicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceApplicationName :: Lens.Lens' ComposeEnvironments (Core.Maybe Types.ApplicationName)
ceApplicationName = Lens.field @"applicationName"
{-# DEPRECATED ceApplicationName "Use generic-lens or generic-optics with 'applicationName' instead." #-}

-- | The name of the group to which the target environments belong. Specify a group name only if the environment name defined in each target environment's manifest ends with a + (plus) character. See <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/environment-cfg-manifest.html Environment Manifest (env.yaml)> for details.
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceGroupName :: Lens.Lens' ComposeEnvironments (Core.Maybe Types.GroupName)
ceGroupName = Lens.field @"groupName"
{-# DEPRECATED ceGroupName "Use generic-lens or generic-optics with 'groupName' instead." #-}

-- | A list of version labels, specifying one or more application source bundles that belong to the target application. Each source bundle must include an environment manifest that specifies the name of the environment and the name of the solution stack to use, and optionally can specify environment links to create.
--
-- /Note:/ Consider using 'versionLabels' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceVersionLabels :: Lens.Lens' ComposeEnvironments (Core.Maybe [Types.VersionLabel])
ceVersionLabels = Lens.field @"versionLabels"
{-# DEPRECATED ceVersionLabels "Use generic-lens or generic-optics with 'versionLabels' instead." #-}

instance Core.AWSRequest ComposeEnvironments where
  type Rs ComposeEnvironments = Types.EnvironmentDescriptionsMessage
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "ComposeEnvironments")
                Core.<> (Core.pure ("Version", "2010-12-01"))
                Core.<> (Core.toQueryValue "ApplicationName" Core.<$> applicationName)
                Core.<> (Core.toQueryValue "GroupName" Core.<$> groupName)
                Core.<> ( Core.toQueryValue
                            "VersionLabels"
                            (Core.toQueryList "member" Core.<$> versionLabels)
                        )
            )
      }
  response =
    Response.receiveXMLWrapper
      "ComposeEnvironmentsResult"
      (\s h x -> Core.parseXML x)
