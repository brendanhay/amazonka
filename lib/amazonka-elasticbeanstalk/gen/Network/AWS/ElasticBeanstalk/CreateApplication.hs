{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      CreateApplication (..)
    , mkCreateApplication
    -- ** Request lenses
    , caApplicationName
    , caDescription
    , caResourceLifecycleConfig
    , caTags

     -- * Destructuring the response
    , Types.ApplicationDescriptionMessage (..)
    , Types.mkApplicationDescriptionMessage
    -- ** Response lenses
    , Types.admApplication
    ) where

import qualified Network.AWS.ElasticBeanstalk.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request to create an application.
--
-- /See:/ 'mkCreateApplication' smart constructor.
data CreateApplication = CreateApplication'
  { applicationName :: Types.ApplicationName
    -- ^ The name of the application. Must be unique within your account.
  , description :: Core.Maybe Types.Description
    -- ^ Your description of the application.
  , resourceLifecycleConfig :: Core.Maybe Types.ApplicationResourceLifecycleConfig
    -- ^ Specifies an application resource lifecycle configuration to prevent your application from accumulating too many versions.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ Specifies the tags applied to the application.
--
-- Elastic Beanstalk applies these tags only to the application. Environments that you create in the application don't inherit the tags.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateApplication' value with any optional fields omitted.
mkCreateApplication
    :: Types.ApplicationName -- ^ 'applicationName'
    -> CreateApplication
mkCreateApplication applicationName
  = CreateApplication'{applicationName, description = Core.Nothing,
                       resourceLifecycleConfig = Core.Nothing, tags = Core.Nothing}

-- | The name of the application. Must be unique within your account.
--
-- /Note:/ Consider using 'applicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caApplicationName :: Lens.Lens' CreateApplication Types.ApplicationName
caApplicationName = Lens.field @"applicationName"
{-# INLINEABLE caApplicationName #-}
{-# DEPRECATED applicationName "Use generic-lens or generic-optics with 'applicationName' instead"  #-}

-- | Your description of the application.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caDescription :: Lens.Lens' CreateApplication (Core.Maybe Types.Description)
caDescription = Lens.field @"description"
{-# INLINEABLE caDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | Specifies an application resource lifecycle configuration to prevent your application from accumulating too many versions.
--
-- /Note:/ Consider using 'resourceLifecycleConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caResourceLifecycleConfig :: Lens.Lens' CreateApplication (Core.Maybe Types.ApplicationResourceLifecycleConfig)
caResourceLifecycleConfig = Lens.field @"resourceLifecycleConfig"
{-# INLINEABLE caResourceLifecycleConfig #-}
{-# DEPRECATED resourceLifecycleConfig "Use generic-lens or generic-optics with 'resourceLifecycleConfig' instead"  #-}

-- | Specifies the tags applied to the application.
--
-- Elastic Beanstalk applies these tags only to the application. Environments that you create in the application don't inherit the tags.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caTags :: Lens.Lens' CreateApplication (Core.Maybe [Types.Tag])
caTags = Lens.field @"tags"
{-# INLINEABLE caTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery CreateApplication where
        toQuery CreateApplication{..}
          = Core.toQueryPair "Action" ("CreateApplication" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-12-01" :: Core.Text)
              Core.<> Core.toQueryPair "ApplicationName" applicationName
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "Description") description
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "ResourceLifecycleConfig")
                resourceLifecycleConfig
              Core.<>
              Core.toQueryPair "Tags"
                (Core.maybe Core.mempty (Core.toQueryList "member") tags)

instance Core.ToHeaders CreateApplication where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest CreateApplication where
        type Rs CreateApplication = Types.ApplicationDescriptionMessage
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXMLWrapper "CreateApplicationResult"
              (\ s h x -> Core.parseXML x)
        
        {-# INLINE parseResponse #-}
