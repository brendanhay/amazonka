{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.GetDomainName
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Represents a domain name that is contained in a simpler, more intuitive URL that can be called.
module Network.AWS.ApiGateway.GetDomainName
    (
    -- * Creating a request
      GetDomainName (..)
    , mkGetDomainName
    -- ** Request lenses
    , gdnDomainName

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

-- | Request to get the name of a 'DomainName' resource.
--
-- /See:/ 'mkGetDomainName' smart constructor.
newtype GetDomainName = GetDomainName'
  { domainName :: Core.Text
    -- ^ [Required] The name of the 'DomainName' resource.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetDomainName' value with any optional fields omitted.
mkGetDomainName
    :: Core.Text -- ^ 'domainName'
    -> GetDomainName
mkGetDomainName domainName = GetDomainName'{domainName}

-- | [Required] The name of the 'DomainName' resource.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdnDomainName :: Lens.Lens' GetDomainName Core.Text
gdnDomainName = Lens.field @"domainName"
{-# INLINEABLE gdnDomainName #-}
{-# DEPRECATED domainName "Use generic-lens or generic-optics with 'domainName' instead"  #-}

instance Core.ToQuery GetDomainName where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetDomainName where
        toHeaders GetDomainName{..}
          = Core.pure ("Accept", "application/json")

instance Core.AWSRequest GetDomainName where
        type Rs GetDomainName = Types.DomainName
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath = "/domainnames/" Core.<> Core.toText domainName,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON (\ s h x -> Core.eitherParseJSON x)
        
        {-# INLINE parseResponse #-}
