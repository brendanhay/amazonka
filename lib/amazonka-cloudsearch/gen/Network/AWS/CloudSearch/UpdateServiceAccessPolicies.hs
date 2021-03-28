{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.UpdateServiceAccessPolicies
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Configures the access rules that control access to the domain's document and search endpoints. For more information, see <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/configuring-access.html Configuring Access for an Amazon CloudSearch Domain> .
module Network.AWS.CloudSearch.UpdateServiceAccessPolicies
    (
    -- * Creating a request
      UpdateServiceAccessPolicies (..)
    , mkUpdateServiceAccessPolicies
    -- ** Request lenses
    , usapDomainName
    , usapAccessPolicies

    -- * Destructuring the response
    , UpdateServiceAccessPoliciesResponse (..)
    , mkUpdateServiceAccessPoliciesResponse
    -- ** Response lenses
    , usaprrsAccessPolicies
    , usaprrsResponseStatus
    ) where

import qualified Network.AWS.CloudSearch.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Container for the parameters to the @'UpdateServiceAccessPolicies' @ operation. Specifies the name of the domain you want to update and the access rules you want to configure.
--
-- /See:/ 'mkUpdateServiceAccessPolicies' smart constructor.
data UpdateServiceAccessPolicies = UpdateServiceAccessPolicies'
  { domainName :: Types.DomainName
  , accessPolicies :: Types.PolicyDocument
    -- ^ The access rules you want to configure. These rules replace any existing rules. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateServiceAccessPolicies' value with any optional fields omitted.
mkUpdateServiceAccessPolicies
    :: Types.DomainName -- ^ 'domainName'
    -> Types.PolicyDocument -- ^ 'accessPolicies'
    -> UpdateServiceAccessPolicies
mkUpdateServiceAccessPolicies domainName accessPolicies
  = UpdateServiceAccessPolicies'{domainName, accessPolicies}

-- | Undocumented field.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usapDomainName :: Lens.Lens' UpdateServiceAccessPolicies Types.DomainName
usapDomainName = Lens.field @"domainName"
{-# INLINEABLE usapDomainName #-}
{-# DEPRECATED domainName "Use generic-lens or generic-optics with 'domainName' instead"  #-}

-- | The access rules you want to configure. These rules replace any existing rules. 
--
-- /Note:/ Consider using 'accessPolicies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usapAccessPolicies :: Lens.Lens' UpdateServiceAccessPolicies Types.PolicyDocument
usapAccessPolicies = Lens.field @"accessPolicies"
{-# INLINEABLE usapAccessPolicies #-}
{-# DEPRECATED accessPolicies "Use generic-lens or generic-optics with 'accessPolicies' instead"  #-}

instance Core.ToQuery UpdateServiceAccessPolicies where
        toQuery UpdateServiceAccessPolicies{..}
          = Core.toQueryPair "Action"
              ("UpdateServiceAccessPolicies" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2013-01-01" :: Core.Text)
              Core.<> Core.toQueryPair "DomainName" domainName
              Core.<> Core.toQueryPair "AccessPolicies" accessPolicies

instance Core.ToHeaders UpdateServiceAccessPolicies where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest UpdateServiceAccessPolicies where
        type Rs UpdateServiceAccessPolicies =
             UpdateServiceAccessPoliciesResponse
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
          = Response.receiveXMLWrapper "UpdateServiceAccessPoliciesResult"
              (\ s h x ->
                 UpdateServiceAccessPoliciesResponse' Core.<$>
                   (x Core..@ "AccessPolicies") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | The result of an @UpdateServiceAccessPolicies@ request. Contains the new access policies.
--
-- /See:/ 'mkUpdateServiceAccessPoliciesResponse' smart constructor.
data UpdateServiceAccessPoliciesResponse = UpdateServiceAccessPoliciesResponse'
  { accessPolicies :: Types.AccessPoliciesStatus
    -- ^ The access rules configured for the domain.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'UpdateServiceAccessPoliciesResponse' value with any optional fields omitted.
mkUpdateServiceAccessPoliciesResponse
    :: Types.AccessPoliciesStatus -- ^ 'accessPolicies'
    -> Core.Int -- ^ 'responseStatus'
    -> UpdateServiceAccessPoliciesResponse
mkUpdateServiceAccessPoliciesResponse accessPolicies responseStatus
  = UpdateServiceAccessPoliciesResponse'{accessPolicies,
                                         responseStatus}

-- | The access rules configured for the domain.
--
-- /Note:/ Consider using 'accessPolicies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usaprrsAccessPolicies :: Lens.Lens' UpdateServiceAccessPoliciesResponse Types.AccessPoliciesStatus
usaprrsAccessPolicies = Lens.field @"accessPolicies"
{-# INLINEABLE usaprrsAccessPolicies #-}
{-# DEPRECATED accessPolicies "Use generic-lens or generic-optics with 'accessPolicies' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usaprrsResponseStatus :: Lens.Lens' UpdateServiceAccessPoliciesResponse Core.Int
usaprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE usaprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
