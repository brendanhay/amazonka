{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.DeleteSAMLProvider
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a SAML provider resource in IAM.
--
-- Deleting the provider resource from IAM does not update any roles that reference the SAML provider resource's ARN as a principal in their trust policies. Any attempt to assume a role that references a non-existent provider resource ARN fails.
module Network.AWS.IAM.DeleteSAMLProvider
    (
    -- * Creating a request
      DeleteSAMLProvider (..)
    , mkDeleteSAMLProvider
    -- ** Request lenses
    , dsamlpSAMLProviderArn

    -- * Destructuring the response
    , DeleteSAMLProviderResponse (..)
    , mkDeleteSAMLProviderResponse
    ) where

import qualified Network.AWS.IAM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteSAMLProvider' smart constructor.
newtype DeleteSAMLProvider = DeleteSAMLProvider'
  { sAMLProviderArn :: Types.SAMLProviderArn
    -- ^ The Amazon Resource Name (ARN) of the SAML provider to delete.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteSAMLProvider' value with any optional fields omitted.
mkDeleteSAMLProvider
    :: Types.SAMLProviderArn -- ^ 'sAMLProviderArn'
    -> DeleteSAMLProvider
mkDeleteSAMLProvider sAMLProviderArn
  = DeleteSAMLProvider'{sAMLProviderArn}

-- | The Amazon Resource Name (ARN) of the SAML provider to delete.
--
-- /Note:/ Consider using 'sAMLProviderArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsamlpSAMLProviderArn :: Lens.Lens' DeleteSAMLProvider Types.SAMLProviderArn
dsamlpSAMLProviderArn = Lens.field @"sAMLProviderArn"
{-# INLINEABLE dsamlpSAMLProviderArn #-}
{-# DEPRECATED sAMLProviderArn "Use generic-lens or generic-optics with 'sAMLProviderArn' instead"  #-}

instance Core.ToQuery DeleteSAMLProvider where
        toQuery DeleteSAMLProvider{..}
          = Core.toQueryPair "Action" ("DeleteSAMLProvider" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-05-08" :: Core.Text)
              Core.<> Core.toQueryPair "SAMLProviderArn" sAMLProviderArn

instance Core.ToHeaders DeleteSAMLProvider where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteSAMLProvider where
        type Rs DeleteSAMLProvider = DeleteSAMLProviderResponse
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
        parseResponse = Response.receiveNull DeleteSAMLProviderResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteSAMLProviderResponse' smart constructor.
data DeleteSAMLProviderResponse = DeleteSAMLProviderResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteSAMLProviderResponse' value with any optional fields omitted.
mkDeleteSAMLProviderResponse
    :: DeleteSAMLProviderResponse
mkDeleteSAMLProviderResponse = DeleteSAMLProviderResponse'
