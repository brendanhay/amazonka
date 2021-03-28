{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SNS.CreatePlatformApplication
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a platform application object for one of the supported push notification services, such as APNS and GCM (Firebase Cloud Messaging), to which devices and mobile apps may register. You must specify @PlatformPrincipal@ and @PlatformCredential@ attributes when using the @CreatePlatformApplication@ action.
--
-- @PlatformPrincipal@ and @PlatformCredential@ are received from the notification service.
--
--     * For @ADM@ , @PlatformPrincipal@ is @client id@ and @PlatformCredential@ is @client secret@ .
--
--
--     * For @Baidu@ , @PlatformPrincipal@ is @API key@ and @PlatformCredential@ is @secret key@ .
--
--
--     * For @APNS@ and @APNS_SANDBOX@ , @PlatformPrincipal@ is @SSL certificate@ and @PlatformCredential@ is @private key@ .
--
--
--     * For @GCM@ (Firebase Cloud Messaging), there is no @PlatformPrincipal@ and the @PlatformCredential@ is @API key@ .
--
--
--     * For @MPNS@ , @PlatformPrincipal@ is @TLS certificate@ and @PlatformCredential@ is @private key@ .
--
--
--     * For @WNS@ , @PlatformPrincipal@ is @Package Security Identifier@ and @PlatformCredential@ is @secret key@ .
--
--
-- You can use the returned @PlatformApplicationArn@ as an attribute for the @CreatePlatformEndpoint@ action.
module Network.AWS.SNS.CreatePlatformApplication
    (
    -- * Creating a request
      CreatePlatformApplication (..)
    , mkCreatePlatformApplication
    -- ** Request lenses
    , cpaName
    , cpaPlatform
    , cpaAttributes

    -- * Destructuring the response
    , CreatePlatformApplicationResponse (..)
    , mkCreatePlatformApplicationResponse
    -- ** Response lenses
    , cparrsPlatformApplicationArn
    , cparrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SNS.Types as Types

-- | Input for CreatePlatformApplication action.
--
-- /See:/ 'mkCreatePlatformApplication' smart constructor.
data CreatePlatformApplication = CreatePlatformApplication'
  { name :: Core.Text
    -- ^ Application names must be made up of only uppercase and lowercase ASCII letters, numbers, underscores, hyphens, and periods, and must be between 1 and 256 characters long.
  , platform :: Core.Text
    -- ^ The following platforms are supported: ADM (Amazon Device Messaging), APNS (Apple Push Notification Service), APNS_SANDBOX, and GCM (Firebase Cloud Messaging).
  , attributes :: Core.HashMap Core.Text Core.Text
    -- ^ For a list of attributes, see <https://docs.aws.amazon.com/sns/latest/api/API_SetPlatformApplicationAttributes.html SetPlatformApplicationAttributes> 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreatePlatformApplication' value with any optional fields omitted.
mkCreatePlatformApplication
    :: Core.Text -- ^ 'name'
    -> Core.Text -- ^ 'platform'
    -> CreatePlatformApplication
mkCreatePlatformApplication name platform
  = CreatePlatformApplication'{name, platform,
                               attributes = Core.mempty}

-- | Application names must be made up of only uppercase and lowercase ASCII letters, numbers, underscores, hyphens, and periods, and must be between 1 and 256 characters long.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpaName :: Lens.Lens' CreatePlatformApplication Core.Text
cpaName = Lens.field @"name"
{-# INLINEABLE cpaName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The following platforms are supported: ADM (Amazon Device Messaging), APNS (Apple Push Notification Service), APNS_SANDBOX, and GCM (Firebase Cloud Messaging).
--
-- /Note:/ Consider using 'platform' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpaPlatform :: Lens.Lens' CreatePlatformApplication Core.Text
cpaPlatform = Lens.field @"platform"
{-# INLINEABLE cpaPlatform #-}
{-# DEPRECATED platform "Use generic-lens or generic-optics with 'platform' instead"  #-}

-- | For a list of attributes, see <https://docs.aws.amazon.com/sns/latest/api/API_SetPlatformApplicationAttributes.html SetPlatformApplicationAttributes> 
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpaAttributes :: Lens.Lens' CreatePlatformApplication (Core.HashMap Core.Text Core.Text)
cpaAttributes = Lens.field @"attributes"
{-# INLINEABLE cpaAttributes #-}
{-# DEPRECATED attributes "Use generic-lens or generic-optics with 'attributes' instead"  #-}

instance Core.ToQuery CreatePlatformApplication where
        toQuery CreatePlatformApplication{..}
          = Core.toQueryPair "Action"
              ("CreatePlatformApplication" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-03-31" :: Core.Text)
              Core.<> Core.toQueryPair "Name" name
              Core.<> Core.toQueryPair "Platform" platform
              Core.<>
              Core.toQueryPair "Attributes"
                (Core.toQueryMap "entry" "key" "value" attributes)

instance Core.ToHeaders CreatePlatformApplication where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest CreatePlatformApplication where
        type Rs CreatePlatformApplication =
             CreatePlatformApplicationResponse
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
          = Response.receiveXMLWrapper "CreatePlatformApplicationResult"
              (\ s h x ->
                 CreatePlatformApplicationResponse' Core.<$>
                   (x Core..@? "PlatformApplicationArn") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Response from CreatePlatformApplication action.
--
-- /See:/ 'mkCreatePlatformApplicationResponse' smart constructor.
data CreatePlatformApplicationResponse = CreatePlatformApplicationResponse'
  { platformApplicationArn :: Core.Maybe Core.Text
    -- ^ PlatformApplicationArn is returned.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreatePlatformApplicationResponse' value with any optional fields omitted.
mkCreatePlatformApplicationResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreatePlatformApplicationResponse
mkCreatePlatformApplicationResponse responseStatus
  = CreatePlatformApplicationResponse'{platformApplicationArn =
                                         Core.Nothing,
                                       responseStatus}

-- | PlatformApplicationArn is returned.
--
-- /Note:/ Consider using 'platformApplicationArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cparrsPlatformApplicationArn :: Lens.Lens' CreatePlatformApplicationResponse (Core.Maybe Core.Text)
cparrsPlatformApplicationArn = Lens.field @"platformApplicationArn"
{-# INLINEABLE cparrsPlatformApplicationArn #-}
{-# DEPRECATED platformApplicationArn "Use generic-lens or generic-optics with 'platformApplicationArn' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cparrsResponseStatus :: Lens.Lens' CreatePlatformApplicationResponse Core.Int
cparrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cparrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
