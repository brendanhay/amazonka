{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.GenerateClientCertificate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Generates a 'ClientCertificate' resource.
module Network.AWS.ApiGateway.GenerateClientCertificate
    (
    -- * Creating a request
      GenerateClientCertificate (..)
    , mkGenerateClientCertificate
    -- ** Request lenses
    , gccDescription
    , gccTags

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

-- | A request to generate a 'ClientCertificate' resource.
--
-- /See:/ 'mkGenerateClientCertificate' smart constructor.
data GenerateClientCertificate = GenerateClientCertificate'
  { description :: Core.Maybe Core.Text
    -- ^ The description of the 'ClientCertificate' .
  , tags :: Core.Maybe (Core.HashMap Core.Text Core.Text)
    -- ^ The key-value map of strings. The valid character set is [a-zA-Z+-=._:/]. The tag key can be up to 128 characters and must not start with @aws:@ . The tag value can be up to 256 characters.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GenerateClientCertificate' value with any optional fields omitted.
mkGenerateClientCertificate
    :: GenerateClientCertificate
mkGenerateClientCertificate
  = GenerateClientCertificate'{description = Core.Nothing,
                               tags = Core.Nothing}

-- | The description of the 'ClientCertificate' .
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gccDescription :: Lens.Lens' GenerateClientCertificate (Core.Maybe Core.Text)
gccDescription = Lens.field @"description"
{-# INLINEABLE gccDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The key-value map of strings. The valid character set is [a-zA-Z+-=._:/]. The tag key can be up to 128 characters and must not start with @aws:@ . The tag value can be up to 256 characters.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gccTags :: Lens.Lens' GenerateClientCertificate (Core.Maybe (Core.HashMap Core.Text Core.Text))
gccTags = Lens.field @"tags"
{-# INLINEABLE gccTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery GenerateClientCertificate where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GenerateClientCertificate where
        toHeaders GenerateClientCertificate{..}
          = Core.pure ("Accept", "application/json")

instance Core.FromJSON GenerateClientCertificate where
        toJSON GenerateClientCertificate{..}
          = Core.object
              (Core.catMaybes
                 [("description" Core..=) Core.<$> description,
                  ("tags" Core..=) Core.<$> tags])

instance Core.AWSRequest GenerateClientCertificate where
        type Rs GenerateClientCertificate = Types.ClientCertificate
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath = "/clientcertificates",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON (\ s h x -> Core.eitherParseJSON x)
        
        {-# INLINE parseResponse #-}
