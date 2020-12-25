{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.CreateApplication
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an application that has one configuration template named @default@ and no application versions.
module Network.AWS.ElasticBeanstalk.CreateApplication
  ( -- * Creating a request
    CreateApplication (..),
    mkCreateApplication,

    -- ** Request lenses
    caApplicationName,
    caDescription,
    caResourceLifecycleConfig,
    caTags,

    -- * Destructuring the response
    Types.ApplicationDescriptionMessage (..),
    Types.mkApplicationDescriptionMessage,

    -- ** Response lenses
    Types.admApplication,
  )
where

import qualified Network.AWS.ElasticBeanstalk.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request to create an application.
--
-- /See:/ 'mkCreateApplication' smart constructor.
data CreateApplication = CreateApplication'
  { -- | The name of the application. Must be unique within your account.
    applicationName :: Types.ApplicationName,
    -- | Your description of the application.
    description :: Core.Maybe Types.Description,
    -- | Specifies an application resource lifecycle configuration to prevent your application from accumulating too many versions.
    resourceLifecycleConfig :: Core.Maybe Types.ApplicationResourceLifecycleConfig,
    -- | Specifies the tags applied to the application.
    --
    -- Elastic Beanstalk applies these tags only to the application. Environments that you create in the application don't inherit the tags.
    tags :: Core.Maybe [Types.Tag]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateApplication' value with any optional fields omitted.
mkCreateApplication ::
  -- | 'applicationName'
  Types.ApplicationName ->
  CreateApplication
mkCreateApplication applicationName =
  CreateApplication'
    { applicationName,
      description = Core.Nothing,
      resourceLifecycleConfig = Core.Nothing,
      tags = Core.Nothing
    }

-- | The name of the application. Must be unique within your account.
--
-- /Note:/ Consider using 'applicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caApplicationName :: Lens.Lens' CreateApplication Types.ApplicationName
caApplicationName = Lens.field @"applicationName"
{-# DEPRECATED caApplicationName "Use generic-lens or generic-optics with 'applicationName' instead." #-}

-- | Your description of the application.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caDescription :: Lens.Lens' CreateApplication (Core.Maybe Types.Description)
caDescription = Lens.field @"description"
{-# DEPRECATED caDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Specifies an application resource lifecycle configuration to prevent your application from accumulating too many versions.
--
-- /Note:/ Consider using 'resourceLifecycleConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caResourceLifecycleConfig :: Lens.Lens' CreateApplication (Core.Maybe Types.ApplicationResourceLifecycleConfig)
caResourceLifecycleConfig = Lens.field @"resourceLifecycleConfig"
{-# DEPRECATED caResourceLifecycleConfig "Use generic-lens or generic-optics with 'resourceLifecycleConfig' instead." #-}

-- | Specifies the tags applied to the application.
--
-- Elastic Beanstalk applies these tags only to the application. Environments that you create in the application don't inherit the tags.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caTags :: Lens.Lens' CreateApplication (Core.Maybe [Types.Tag])
caTags = Lens.field @"tags"
{-# DEPRECATED caTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.AWSRequest CreateApplication where
  type Rs CreateApplication = Types.ApplicationDescriptionMessage
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
            ( Core.pure ("Action", "CreateApplication")
                Core.<> (Core.pure ("Version", "2010-12-01"))
                Core.<> (Core.toQueryValue "ApplicationName" applicationName)
                Core.<> (Core.toQueryValue "Description" Core.<$> description)
                Core.<> ( Core.toQueryValue "ResourceLifecycleConfig"
                            Core.<$> resourceLifecycleConfig
                        )
                Core.<> ( Core.toQueryValue
                            "Tags"
                            (Core.toQueryList "member" Core.<$> tags)
                        )
            )
      }
  response =
    Response.receiveXMLWrapper
      "CreateApplicationResult"
      (\s h x -> Core.parseXML x)
