{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.GetClientCertificate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the current 'ClientCertificate' resource.
module Network.AWS.ApiGateway.GetClientCertificate
    (
    -- * Creating a request
      GetClientCertificate (..)
    , mkGetClientCertificate
    -- ** Request lenses
    , gccClientCertificateId

     -- * Destructuring the response
    , Types.ClientCertificate (..)
    , Types.mkClientCertificate
    -- ** Response lenses
    , Types.ccClientCertificateId
    , Types.ccCreatedDate
    , Types.ccDescription
    , Types.ccExpirationDate
    , Types.ccPemEncodedCertificate
    , Types.ccTags
    ) where

import qualified Network.AWS.ApiGateway.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | A request to get information about the current 'ClientCertificate' resource.
--
-- /See:/ 'mkGetClientCertificate' smart constructor.
newtype GetClientCertificate = GetClientCertificate'
  { clientCertificateId :: Core.Text
    -- ^ [Required] The identifier of the 'ClientCertificate' resource to be described.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetClientCertificate' value with any optional fields omitted.
mkGetClientCertificate
    :: Core.Text -- ^ 'clientCertificateId'
    -> GetClientCertificate
mkGetClientCertificate clientCertificateId
  = GetClientCertificate'{clientCertificateId}

-- | [Required] The identifier of the 'ClientCertificate' resource to be described.
--
-- /Note:/ Consider using 'clientCertificateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gccClientCertificateId :: Lens.Lens' GetClientCertificate Core.Text
gccClientCertificateId = Lens.field @"clientCertificateId"
{-# INLINEABLE gccClientCertificateId #-}
{-# DEPRECATED clientCertificateId "Use generic-lens or generic-optics with 'clientCertificateId' instead"  #-}

instance Core.ToQuery GetClientCertificate where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetClientCertificate where
        toHeaders GetClientCertificate{..}
          = Core.pure ("Accept", "application/json")

instance Core.AWSRequest GetClientCertificate where
        type Rs GetClientCertificate = Types.ClientCertificate
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/clientcertificates/" Core.<> Core.toText clientCertificateId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON (\ s h x -> Core.eitherParseJSON x)
        
        {-# INLINE parseResponse #-}
