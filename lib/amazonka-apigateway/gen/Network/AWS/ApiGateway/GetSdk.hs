{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.GetSdk
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Generates a client SDK for a 'RestApi' and 'Stage' .
module Network.AWS.ApiGateway.GetSdk
    (
    -- * Creating a request
      GetSdk (..)
    , mkGetSdk
    -- ** Request lenses
    , gsgRestApiId
    , gsgStageName
    , gsgSdkType
    , gsgParameters

    -- * Destructuring the response
    , GetSdkResponse (..)
    , mkGetSdkResponse
    -- ** Response lenses
    , grsBody
    , grsContentDisposition
    , grsContentType
    , grsResponseStatus
    ) where

import qualified Network.AWS.ApiGateway.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request a new generated client SDK for a 'RestApi' and 'Stage' .
--
-- /See:/ 'mkGetSdk' smart constructor.
data GetSdk = GetSdk'
  { restApiId :: Core.Text
    -- ^ [Required] The string identifier of the associated 'RestApi' .
  , stageName :: Core.Text
    -- ^ [Required] The name of the 'Stage' that the SDK will use.
  , sdkType :: Core.Text
    -- ^ [Required] The language for the generated SDK. Currently @java@ , @javascript@ , @android@ , @objectivec@ (for iOS), @swift@ (for iOS), and @ruby@ are supported.
  , parameters :: Core.Maybe (Core.HashMap Core.Text Core.Text)
    -- ^ A string-to-string key-value map of query parameters @sdkType@ -dependent properties of the SDK. For @sdkType@ of @objectivec@ or @swift@ , a parameter named @classPrefix@ is required. For @sdkType@ of @android@ , parameters named @groupId@ , @artifactId@ , @artifactVersion@ , and @invokerPackage@ are required. For @sdkType@ of @java@ , parameters named @serviceName@ and @javaPackageName@ are required. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetSdk' value with any optional fields omitted.
mkGetSdk
    :: Core.Text -- ^ 'restApiId'
    -> Core.Text -- ^ 'stageName'
    -> Core.Text -- ^ 'sdkType'
    -> GetSdk
mkGetSdk restApiId stageName sdkType
  = GetSdk'{restApiId, stageName, sdkType, parameters = Core.Nothing}

-- | [Required] The string identifier of the associated 'RestApi' .
--
-- /Note:/ Consider using 'restApiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsgRestApiId :: Lens.Lens' GetSdk Core.Text
gsgRestApiId = Lens.field @"restApiId"
{-# INLINEABLE gsgRestApiId #-}
{-# DEPRECATED restApiId "Use generic-lens or generic-optics with 'restApiId' instead"  #-}

-- | [Required] The name of the 'Stage' that the SDK will use.
--
-- /Note:/ Consider using 'stageName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsgStageName :: Lens.Lens' GetSdk Core.Text
gsgStageName = Lens.field @"stageName"
{-# INLINEABLE gsgStageName #-}
{-# DEPRECATED stageName "Use generic-lens or generic-optics with 'stageName' instead"  #-}

-- | [Required] The language for the generated SDK. Currently @java@ , @javascript@ , @android@ , @objectivec@ (for iOS), @swift@ (for iOS), and @ruby@ are supported.
--
-- /Note:/ Consider using 'sdkType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsgSdkType :: Lens.Lens' GetSdk Core.Text
gsgSdkType = Lens.field @"sdkType"
{-# INLINEABLE gsgSdkType #-}
{-# DEPRECATED sdkType "Use generic-lens or generic-optics with 'sdkType' instead"  #-}

-- | A string-to-string key-value map of query parameters @sdkType@ -dependent properties of the SDK. For @sdkType@ of @objectivec@ or @swift@ , a parameter named @classPrefix@ is required. For @sdkType@ of @android@ , parameters named @groupId@ , @artifactId@ , @artifactVersion@ , and @invokerPackage@ are required. For @sdkType@ of @java@ , parameters named @serviceName@ and @javaPackageName@ are required. 
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsgParameters :: Lens.Lens' GetSdk (Core.Maybe (Core.HashMap Core.Text Core.Text))
gsgParameters = Lens.field @"parameters"
{-# INLINEABLE gsgParameters #-}
{-# DEPRECATED parameters "Use generic-lens or generic-optics with 'parameters' instead"  #-}

instance Core.ToQuery GetSdk where
        toQuery GetSdk{..}
          = Core.toQueryPair "parameters"
              (Core.maybe Core.mempty (Core.toQueryMap "entry" "key" "value")
                 parameters)

instance Core.ToHeaders GetSdk where
        toHeaders GetSdk{..} = Core.pure ("Accept", "application/json")

instance Core.AWSRequest GetSdk where
        type Rs GetSdk = GetSdkResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/restapis/" Core.<> Core.toText restApiId Core.<> "/stages/"
                             Core.<> Core.toText stageName
                             Core.<> "/sdks/"
                             Core.<> Core.toText sdkType,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveBytes
              (\ s h x ->
                 GetSdkResponse' Core.<$>
                   (Core.pure x) Core.<*>
                     Core.parseHeaderMaybe "Content-Disposition" h
                     Core.<*> Core.parseHeaderMaybe "Content-Type" h
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | The binary blob response to 'GetSdk' , which contains the generated SDK.
--
-- /See:/ 'mkGetSdkResponse' smart constructor.
data GetSdkResponse = GetSdkResponse'
  { body :: Core.Maybe Core.ByteString
    -- ^ The binary blob response to 'GetSdk' , which contains the generated SDK.
  , contentDisposition :: Core.Maybe Core.Text
    -- ^ The content-disposition header value in the HTTP response.
  , contentType :: Core.Maybe Core.Text
    -- ^ The content-type header value in the HTTP response.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetSdkResponse' value with any optional fields omitted.
mkGetSdkResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetSdkResponse
mkGetSdkResponse responseStatus
  = GetSdkResponse'{body = Core.Nothing,
                    contentDisposition = Core.Nothing, contentType = Core.Nothing,
                    responseStatus}

-- | The binary blob response to 'GetSdk' , which contains the generated SDK.
--
-- /Note:/ Consider using 'body' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grsBody :: Lens.Lens' GetSdkResponse (Core.Maybe Core.ByteString)
grsBody = Lens.field @"body"
{-# INLINEABLE grsBody #-}
{-# DEPRECATED body "Use generic-lens or generic-optics with 'body' instead"  #-}

-- | The content-disposition header value in the HTTP response.
--
-- /Note:/ Consider using 'contentDisposition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grsContentDisposition :: Lens.Lens' GetSdkResponse (Core.Maybe Core.Text)
grsContentDisposition = Lens.field @"contentDisposition"
{-# INLINEABLE grsContentDisposition #-}
{-# DEPRECATED contentDisposition "Use generic-lens or generic-optics with 'contentDisposition' instead"  #-}

-- | The content-type header value in the HTTP response.
--
-- /Note:/ Consider using 'contentType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grsContentType :: Lens.Lens' GetSdkResponse (Core.Maybe Core.Text)
grsContentType = Lens.field @"contentType"
{-# INLINEABLE grsContentType #-}
{-# DEPRECATED contentType "Use generic-lens or generic-optics with 'contentType' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grsResponseStatus :: Lens.Lens' GetSdkResponse Core.Int
grsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE grsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
