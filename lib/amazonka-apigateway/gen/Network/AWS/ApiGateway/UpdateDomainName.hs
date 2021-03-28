{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.UpdateDomainName
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes information about the 'DomainName' resource.
module Network.AWS.ApiGateway.UpdateDomainName
    (
    -- * Creating a request
      UpdateDomainName (..)
    , mkUpdateDomainName
    -- ** Request lenses
    , udnDomainName
    , udnPatchOperations

     -- * Destructuring the response
    , Types.DomainName (..)
    , Types.mkDomainName
    -- ** Response lenses
    , Types.dnCertificateArn
    , Types.dnCertificateName
    , Types.dnCertificateUploadDate
    , Types.dnDistributionDomainName
    , Types.dnDistributionHostedZoneId
    , Types.dnDomainName
    , Types.dnDomainNameStatus
    , Types.dnDomainNameStatusMessage
    , Types.dnEndpointConfiguration
    , Types.dnMutualTlsAuthentication
    , Types.dnRegionalCertificateArn
    , Types.dnRegionalCertificateName
    , Types.dnRegionalDomainName
    , Types.dnRegionalHostedZoneId
    , Types.dnSecurityPolicy
    , Types.dnTags
    ) where

import qualified Network.AWS.ApiGateway.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | A request to change information about the 'DomainName' resource.
--
-- /See:/ 'mkUpdateDomainName' smart constructor.
data UpdateDomainName = UpdateDomainName'
  { domainName :: Core.Text
    -- ^ [Required] The name of the 'DomainName' resource to be changed.
  , patchOperations :: Core.Maybe [Types.PatchOperation]
    -- ^ A list of update operations to be applied to the specified resource and in the order specified in this list.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateDomainName' value with any optional fields omitted.
mkUpdateDomainName
    :: Core.Text -- ^ 'domainName'
    -> UpdateDomainName
mkUpdateDomainName domainName
  = UpdateDomainName'{domainName, patchOperations = Core.Nothing}

-- | [Required] The name of the 'DomainName' resource to be changed.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udnDomainName :: Lens.Lens' UpdateDomainName Core.Text
udnDomainName = Lens.field @"domainName"
{-# INLINEABLE udnDomainName #-}
{-# DEPRECATED domainName "Use generic-lens or generic-optics with 'domainName' instead"  #-}

-- | A list of update operations to be applied to the specified resource and in the order specified in this list.
--
-- /Note:/ Consider using 'patchOperations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udnPatchOperations :: Lens.Lens' UpdateDomainName (Core.Maybe [Types.PatchOperation])
udnPatchOperations = Lens.field @"patchOperations"
{-# INLINEABLE udnPatchOperations #-}
{-# DEPRECATED patchOperations "Use generic-lens or generic-optics with 'patchOperations' instead"  #-}

instance Core.ToQuery UpdateDomainName where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateDomainName where
        toHeaders UpdateDomainName{..}
          = Core.pure ("Accept", "application/json")

instance Core.FromJSON UpdateDomainName where
        toJSON UpdateDomainName{..}
          = Core.object
              (Core.catMaybes
                 [("patchOperations" Core..=) Core.<$> patchOperations])

instance Core.AWSRequest UpdateDomainName where
        type Rs UpdateDomainName = Types.DomainName
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PATCH,
                         Core._rqPath = "/domainnames/" Core.<> Core.toText domainName,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON (\ s h x -> Core.eitherParseJSON x)
        
        {-# INLINE parseResponse #-}
