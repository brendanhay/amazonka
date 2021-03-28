{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.GetOpenIDConnectProvider
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the specified OpenID Connect (OIDC) provider resource object in IAM.
module Network.AWS.IAM.GetOpenIDConnectProvider
    (
    -- * Creating a request
      GetOpenIDConnectProvider (..)
    , mkGetOpenIDConnectProvider
    -- ** Request lenses
    , goidcpOpenIDConnectProviderArn

    -- * Destructuring the response
    , GetOpenIDConnectProviderResponse (..)
    , mkGetOpenIDConnectProviderResponse
    -- ** Response lenses
    , goidcprrsClientIDList
    , goidcprrsCreateDate
    , goidcprrsThumbprintList
    , goidcprrsUrl
    , goidcprrsResponseStatus
    ) where

import qualified Network.AWS.IAM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetOpenIDConnectProvider' smart constructor.
newtype GetOpenIDConnectProvider = GetOpenIDConnectProvider'
  { openIDConnectProviderArn :: Types.ArnType
    -- ^ The Amazon Resource Name (ARN) of the OIDC provider resource object in IAM to get information for. You can get a list of OIDC provider resource ARNs by using the 'ListOpenIDConnectProviders' operation.
--
-- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetOpenIDConnectProvider' value with any optional fields omitted.
mkGetOpenIDConnectProvider
    :: Types.ArnType -- ^ 'openIDConnectProviderArn'
    -> GetOpenIDConnectProvider
mkGetOpenIDConnectProvider openIDConnectProviderArn
  = GetOpenIDConnectProvider'{openIDConnectProviderArn}

-- | The Amazon Resource Name (ARN) of the OIDC provider resource object in IAM to get information for. You can get a list of OIDC provider resource ARNs by using the 'ListOpenIDConnectProviders' operation.
--
-- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
--
-- /Note:/ Consider using 'openIDConnectProviderArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goidcpOpenIDConnectProviderArn :: Lens.Lens' GetOpenIDConnectProvider Types.ArnType
goidcpOpenIDConnectProviderArn = Lens.field @"openIDConnectProviderArn"
{-# INLINEABLE goidcpOpenIDConnectProviderArn #-}
{-# DEPRECATED openIDConnectProviderArn "Use generic-lens or generic-optics with 'openIDConnectProviderArn' instead"  #-}

instance Core.ToQuery GetOpenIDConnectProvider where
        toQuery GetOpenIDConnectProvider{..}
          = Core.toQueryPair "Action"
              ("GetOpenIDConnectProvider" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-05-08" :: Core.Text)
              Core.<>
              Core.toQueryPair "OpenIDConnectProviderArn"
                openIDConnectProviderArn

instance Core.ToHeaders GetOpenIDConnectProvider where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest GetOpenIDConnectProvider where
        type Rs GetOpenIDConnectProvider = GetOpenIDConnectProviderResponse
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
          = Response.receiveXMLWrapper "GetOpenIDConnectProviderResult"
              (\ s h x ->
                 GetOpenIDConnectProviderResponse' Core.<$>
                   (x Core..@? "ClientIDList" Core..<@> Core.parseXMLList "member")
                     Core.<*> x Core..@? "CreateDate"
                     Core.<*>
                     x Core..@? "ThumbprintList" Core..<@> Core.parseXMLList "member"
                     Core.<*> x Core..@? "Url"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Contains the response to a successful 'GetOpenIDConnectProvider' request. 
--
-- /See:/ 'mkGetOpenIDConnectProviderResponse' smart constructor.
data GetOpenIDConnectProviderResponse = GetOpenIDConnectProviderResponse'
  { clientIDList :: Core.Maybe [Types.ClientIDType]
    -- ^ A list of client IDs (also known as audiences) that are associated with the specified IAM OIDC provider resource object. For more information, see 'CreateOpenIDConnectProvider' .
  , createDate :: Core.Maybe Core.UTCTime
    -- ^ The date and time when the IAM OIDC provider resource object was created in the AWS account.
  , thumbprintList :: Core.Maybe [Types.ThumbprintType]
    -- ^ A list of certificate thumbprints that are associated with the specified IAM OIDC provider resource object. For more information, see 'CreateOpenIDConnectProvider' . 
  , url :: Core.Maybe Types.OpenIDConnectProviderUrlType
    -- ^ The URL that the IAM OIDC provider resource object is associated with. For more information, see 'CreateOpenIDConnectProvider' .
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetOpenIDConnectProviderResponse' value with any optional fields omitted.
mkGetOpenIDConnectProviderResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetOpenIDConnectProviderResponse
mkGetOpenIDConnectProviderResponse responseStatus
  = GetOpenIDConnectProviderResponse'{clientIDList = Core.Nothing,
                                      createDate = Core.Nothing, thumbprintList = Core.Nothing,
                                      url = Core.Nothing, responseStatus}

-- | A list of client IDs (also known as audiences) that are associated with the specified IAM OIDC provider resource object. For more information, see 'CreateOpenIDConnectProvider' .
--
-- /Note:/ Consider using 'clientIDList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goidcprrsClientIDList :: Lens.Lens' GetOpenIDConnectProviderResponse (Core.Maybe [Types.ClientIDType])
goidcprrsClientIDList = Lens.field @"clientIDList"
{-# INLINEABLE goidcprrsClientIDList #-}
{-# DEPRECATED clientIDList "Use generic-lens or generic-optics with 'clientIDList' instead"  #-}

-- | The date and time when the IAM OIDC provider resource object was created in the AWS account.
--
-- /Note:/ Consider using 'createDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goidcprrsCreateDate :: Lens.Lens' GetOpenIDConnectProviderResponse (Core.Maybe Core.UTCTime)
goidcprrsCreateDate = Lens.field @"createDate"
{-# INLINEABLE goidcprrsCreateDate #-}
{-# DEPRECATED createDate "Use generic-lens or generic-optics with 'createDate' instead"  #-}

-- | A list of certificate thumbprints that are associated with the specified IAM OIDC provider resource object. For more information, see 'CreateOpenIDConnectProvider' . 
--
-- /Note:/ Consider using 'thumbprintList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goidcprrsThumbprintList :: Lens.Lens' GetOpenIDConnectProviderResponse (Core.Maybe [Types.ThumbprintType])
goidcprrsThumbprintList = Lens.field @"thumbprintList"
{-# INLINEABLE goidcprrsThumbprintList #-}
{-# DEPRECATED thumbprintList "Use generic-lens or generic-optics with 'thumbprintList' instead"  #-}

-- | The URL that the IAM OIDC provider resource object is associated with. For more information, see 'CreateOpenIDConnectProvider' .
--
-- /Note:/ Consider using 'url' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goidcprrsUrl :: Lens.Lens' GetOpenIDConnectProviderResponse (Core.Maybe Types.OpenIDConnectProviderUrlType)
goidcprrsUrl = Lens.field @"url"
{-# INLINEABLE goidcprrsUrl #-}
{-# DEPRECATED url "Use generic-lens or generic-optics with 'url' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goidcprrsResponseStatus :: Lens.Lens' GetOpenIDConnectProviderResponse Core.Int
goidcprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE goidcprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
