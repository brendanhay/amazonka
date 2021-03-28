{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.UpdateOpenIDConnectProviderThumbprint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Replaces the existing list of server certificate thumbprints associated with an OpenID Connect (OIDC) provider resource object with a new list of thumbprints.
--
-- The list that you pass with this operation completely replaces the existing list of thumbprints. (The lists are not merged.)
-- Typically, you need to update a thumbprint only when the identity provider's certificate changes, which occurs rarely. However, if the provider's certificate /does/ change, any attempt to assume an IAM role that specifies the OIDC provider as a principal fails until the certificate thumbprint is updated.
module Network.AWS.IAM.UpdateOpenIDConnectProviderThumbprint
    (
    -- * Creating a request
      UpdateOpenIDConnectProviderThumbprint (..)
    , mkUpdateOpenIDConnectProviderThumbprint
    -- ** Request lenses
    , uoidcptOpenIDConnectProviderArn
    , uoidcptThumbprintList

    -- * Destructuring the response
    , UpdateOpenIDConnectProviderThumbprintResponse (..)
    , mkUpdateOpenIDConnectProviderThumbprintResponse
    ) where

import qualified Network.AWS.IAM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateOpenIDConnectProviderThumbprint' smart constructor.
data UpdateOpenIDConnectProviderThumbprint = UpdateOpenIDConnectProviderThumbprint'
  { openIDConnectProviderArn :: Types.ArnType
    -- ^ The Amazon Resource Name (ARN) of the IAM OIDC provider resource object for which you want to update the thumbprint. You can get a list of OIDC provider ARNs by using the 'ListOpenIDConnectProviders' operation.
--
-- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
  , thumbprintList :: [Types.ThumbprintType]
    -- ^ A list of certificate thumbprints that are associated with the specified IAM OpenID Connect provider. For more information, see 'CreateOpenIDConnectProvider' . 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateOpenIDConnectProviderThumbprint' value with any optional fields omitted.
mkUpdateOpenIDConnectProviderThumbprint
    :: Types.ArnType -- ^ 'openIDConnectProviderArn'
    -> UpdateOpenIDConnectProviderThumbprint
mkUpdateOpenIDConnectProviderThumbprint openIDConnectProviderArn
  = UpdateOpenIDConnectProviderThumbprint'{openIDConnectProviderArn,
                                           thumbprintList = Core.mempty}

-- | The Amazon Resource Name (ARN) of the IAM OIDC provider resource object for which you want to update the thumbprint. You can get a list of OIDC provider ARNs by using the 'ListOpenIDConnectProviders' operation.
--
-- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
--
-- /Note:/ Consider using 'openIDConnectProviderArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uoidcptOpenIDConnectProviderArn :: Lens.Lens' UpdateOpenIDConnectProviderThumbprint Types.ArnType
uoidcptOpenIDConnectProviderArn = Lens.field @"openIDConnectProviderArn"
{-# INLINEABLE uoidcptOpenIDConnectProviderArn #-}
{-# DEPRECATED openIDConnectProviderArn "Use generic-lens or generic-optics with 'openIDConnectProviderArn' instead"  #-}

-- | A list of certificate thumbprints that are associated with the specified IAM OpenID Connect provider. For more information, see 'CreateOpenIDConnectProvider' . 
--
-- /Note:/ Consider using 'thumbprintList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uoidcptThumbprintList :: Lens.Lens' UpdateOpenIDConnectProviderThumbprint [Types.ThumbprintType]
uoidcptThumbprintList = Lens.field @"thumbprintList"
{-# INLINEABLE uoidcptThumbprintList #-}
{-# DEPRECATED thumbprintList "Use generic-lens or generic-optics with 'thumbprintList' instead"  #-}

instance Core.ToQuery UpdateOpenIDConnectProviderThumbprint where
        toQuery UpdateOpenIDConnectProviderThumbprint{..}
          = Core.toQueryPair "Action"
              ("UpdateOpenIDConnectProviderThumbprint" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-05-08" :: Core.Text)
              Core.<>
              Core.toQueryPair "OpenIDConnectProviderArn"
                openIDConnectProviderArn
              Core.<>
              Core.toQueryPair "ThumbprintList"
                (Core.toQueryList "member" thumbprintList)

instance Core.ToHeaders UpdateOpenIDConnectProviderThumbprint where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest UpdateOpenIDConnectProviderThumbprint
         where
        type Rs UpdateOpenIDConnectProviderThumbprint =
             UpdateOpenIDConnectProviderThumbprintResponse
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
          = Response.receiveNull
              UpdateOpenIDConnectProviderThumbprintResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateOpenIDConnectProviderThumbprintResponse' smart constructor.
data UpdateOpenIDConnectProviderThumbprintResponse = UpdateOpenIDConnectProviderThumbprintResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateOpenIDConnectProviderThumbprintResponse' value with any optional fields omitted.
mkUpdateOpenIDConnectProviderThumbprintResponse
    :: UpdateOpenIDConnectProviderThumbprintResponse
mkUpdateOpenIDConnectProviderThumbprintResponse
  = UpdateOpenIDConnectProviderThumbprintResponse'
