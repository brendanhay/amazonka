{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.CreatePlatformVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create a new version of your custom platform.
module Network.AWS.ElasticBeanstalk.CreatePlatformVersion
    (
    -- * Creating a request
      CreatePlatformVersion (..)
    , mkCreatePlatformVersion
    -- ** Request lenses
    , cpvPlatformName
    , cpvPlatformVersion
    , cpvPlatformDefinitionBundle
    , cpvEnvironmentName
    , cpvOptionSettings
    , cpvTags

    -- * Destructuring the response
    , CreatePlatformVersionResponse (..)
    , mkCreatePlatformVersionResponse
    -- ** Response lenses
    , cpvrrsBuilder
    , cpvrrsPlatformSummary
    , cpvrrsResponseStatus
    ) where

import qualified Network.AWS.ElasticBeanstalk.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request to create a new platform version.
--
-- /See:/ 'mkCreatePlatformVersion' smart constructor.
data CreatePlatformVersion = CreatePlatformVersion'
  { platformName :: Types.PlatformName
    -- ^ The name of your custom platform.
  , platformVersion :: Types.PlatformVersion
    -- ^ The number, such as 1.0.2, for the new platform version.
  , platformDefinitionBundle :: Types.S3Location
    -- ^ The location of the platform definition archive in Amazon S3.
  , environmentName :: Core.Maybe Types.EnvironmentName
    -- ^ The name of the builder environment.
  , optionSettings :: Core.Maybe [Types.ConfigurationOptionSetting]
    -- ^ The configuration option settings to apply to the builder environment.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ Specifies the tags applied to the new platform version.
--
-- Elastic Beanstalk applies these tags only to the platform version. Environments that you create using the platform version don't inherit the tags.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreatePlatformVersion' value with any optional fields omitted.
mkCreatePlatformVersion
    :: Types.PlatformName -- ^ 'platformName'
    -> Types.PlatformVersion -- ^ 'platformVersion'
    -> Types.S3Location -- ^ 'platformDefinitionBundle'
    -> CreatePlatformVersion
mkCreatePlatformVersion platformName platformVersion
  platformDefinitionBundle
  = CreatePlatformVersion'{platformName, platformVersion,
                           platformDefinitionBundle, environmentName = Core.Nothing,
                           optionSettings = Core.Nothing, tags = Core.Nothing}

-- | The name of your custom platform.
--
-- /Note:/ Consider using 'platformName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpvPlatformName :: Lens.Lens' CreatePlatformVersion Types.PlatformName
cpvPlatformName = Lens.field @"platformName"
{-# INLINEABLE cpvPlatformName #-}
{-# DEPRECATED platformName "Use generic-lens or generic-optics with 'platformName' instead"  #-}

-- | The number, such as 1.0.2, for the new platform version.
--
-- /Note:/ Consider using 'platformVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpvPlatformVersion :: Lens.Lens' CreatePlatformVersion Types.PlatformVersion
cpvPlatformVersion = Lens.field @"platformVersion"
{-# INLINEABLE cpvPlatformVersion #-}
{-# DEPRECATED platformVersion "Use generic-lens or generic-optics with 'platformVersion' instead"  #-}

-- | The location of the platform definition archive in Amazon S3.
--
-- /Note:/ Consider using 'platformDefinitionBundle' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpvPlatformDefinitionBundle :: Lens.Lens' CreatePlatformVersion Types.S3Location
cpvPlatformDefinitionBundle = Lens.field @"platformDefinitionBundle"
{-# INLINEABLE cpvPlatformDefinitionBundle #-}
{-# DEPRECATED platformDefinitionBundle "Use generic-lens or generic-optics with 'platformDefinitionBundle' instead"  #-}

-- | The name of the builder environment.
--
-- /Note:/ Consider using 'environmentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpvEnvironmentName :: Lens.Lens' CreatePlatformVersion (Core.Maybe Types.EnvironmentName)
cpvEnvironmentName = Lens.field @"environmentName"
{-# INLINEABLE cpvEnvironmentName #-}
{-# DEPRECATED environmentName "Use generic-lens or generic-optics with 'environmentName' instead"  #-}

-- | The configuration option settings to apply to the builder environment.
--
-- /Note:/ Consider using 'optionSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpvOptionSettings :: Lens.Lens' CreatePlatformVersion (Core.Maybe [Types.ConfigurationOptionSetting])
cpvOptionSettings = Lens.field @"optionSettings"
{-# INLINEABLE cpvOptionSettings #-}
{-# DEPRECATED optionSettings "Use generic-lens or generic-optics with 'optionSettings' instead"  #-}

-- | Specifies the tags applied to the new platform version.
--
-- Elastic Beanstalk applies these tags only to the platform version. Environments that you create using the platform version don't inherit the tags.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpvTags :: Lens.Lens' CreatePlatformVersion (Core.Maybe [Types.Tag])
cpvTags = Lens.field @"tags"
{-# INLINEABLE cpvTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery CreatePlatformVersion where
        toQuery CreatePlatformVersion{..}
          = Core.toQueryPair "Action" ("CreatePlatformVersion" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-12-01" :: Core.Text)
              Core.<> Core.toQueryPair "PlatformName" platformName
              Core.<> Core.toQueryPair "PlatformVersion" platformVersion
              Core.<>
              Core.toQueryPair "PlatformDefinitionBundle"
                platformDefinitionBundle
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "EnvironmentName")
                environmentName
              Core.<>
              Core.toQueryPair "OptionSettings"
                (Core.maybe Core.mempty (Core.toQueryList "member") optionSettings)
              Core.<>
              Core.toQueryPair "Tags"
                (Core.maybe Core.mempty (Core.toQueryList "member") tags)

instance Core.ToHeaders CreatePlatformVersion where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest CreatePlatformVersion where
        type Rs CreatePlatformVersion = CreatePlatformVersionResponse
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
          = Response.receiveXMLWrapper "CreatePlatformVersionResult"
              (\ s h x ->
                 CreatePlatformVersionResponse' Core.<$>
                   (x Core..@? "Builder") Core.<*> x Core..@? "PlatformSummary"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreatePlatformVersionResponse' smart constructor.
data CreatePlatformVersionResponse = CreatePlatformVersionResponse'
  { builder :: Core.Maybe Types.Builder
    -- ^ The builder used to create the custom platform.
  , platformSummary :: Core.Maybe Types.PlatformSummary
    -- ^ Detailed information about the new version of the custom platform.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreatePlatformVersionResponse' value with any optional fields omitted.
mkCreatePlatformVersionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreatePlatformVersionResponse
mkCreatePlatformVersionResponse responseStatus
  = CreatePlatformVersionResponse'{builder = Core.Nothing,
                                   platformSummary = Core.Nothing, responseStatus}

-- | The builder used to create the custom platform.
--
-- /Note:/ Consider using 'builder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpvrrsBuilder :: Lens.Lens' CreatePlatformVersionResponse (Core.Maybe Types.Builder)
cpvrrsBuilder = Lens.field @"builder"
{-# INLINEABLE cpvrrsBuilder #-}
{-# DEPRECATED builder "Use generic-lens or generic-optics with 'builder' instead"  #-}

-- | Detailed information about the new version of the custom platform.
--
-- /Note:/ Consider using 'platformSummary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpvrrsPlatformSummary :: Lens.Lens' CreatePlatformVersionResponse (Core.Maybe Types.PlatformSummary)
cpvrrsPlatformSummary = Lens.field @"platformSummary"
{-# INLINEABLE cpvrrsPlatformSummary #-}
{-# DEPRECATED platformSummary "Use generic-lens or generic-optics with 'platformSummary' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpvrrsResponseStatus :: Lens.Lens' CreatePlatformVersionResponse Core.Int
cpvrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cpvrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
