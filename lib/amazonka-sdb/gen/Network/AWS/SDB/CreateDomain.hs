{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SDB.CreateDomain
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @CreateDomain@ operation creates a new domain. The domain name should be unique among the domains associated with the Access Key ID provided in the request. The @CreateDomain@ operation may take 10 or more seconds to complete. 
--
-- The client can create up to 100 domains per account. 
-- If the client requires additional domains, go to <http://aws.amazon.com/contact-us/simpledb-limit-request/ http://aws.amazon.com/contact-us/simpledb-limit-request/> . 
module Network.AWS.SDB.CreateDomain
    (
    -- * Creating a request
      CreateDomain (..)
    , mkCreateDomain
    -- ** Request lenses
    , cdDomainName

    -- * Destructuring the response
    , CreateDomainResponse (..)
    , mkCreateDomainResponse
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SDB.Types as Types

-- | /See:/ 'mkCreateDomain' smart constructor.
newtype CreateDomain = CreateDomain'
  { domainName :: Core.Text
    -- ^ The name of the domain to create. The name can range between 3 and 255 characters and can contain the following characters: a-z, A-Z, 0-9, '_', '-', and '.'.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CreateDomain' value with any optional fields omitted.
mkCreateDomain
    :: Core.Text -- ^ 'domainName'
    -> CreateDomain
mkCreateDomain domainName = CreateDomain'{domainName}

-- | The name of the domain to create. The name can range between 3 and 255 characters and can contain the following characters: a-z, A-Z, 0-9, '_', '-', and '.'.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdDomainName :: Lens.Lens' CreateDomain Core.Text
cdDomainName = Lens.field @"domainName"
{-# INLINEABLE cdDomainName #-}
{-# DEPRECATED domainName "Use generic-lens or generic-optics with 'domainName' instead"  #-}

instance Core.ToQuery CreateDomain where
        toQuery CreateDomain{..}
          = Core.toQueryPair "Action" ("CreateDomain" :: Core.Text) Core.<>
              Core.toQueryPair "Version" ("2009-04-15" :: Core.Text)
              Core.<> Core.toQueryPair "DomainName" domainName

instance Core.ToHeaders CreateDomain where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest CreateDomain where
        type Rs CreateDomain = CreateDomainResponse
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
        parseResponse = Response.receiveNull CreateDomainResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateDomainResponse' smart constructor.
data CreateDomainResponse = CreateDomainResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateDomainResponse' value with any optional fields omitted.
mkCreateDomainResponse
    :: CreateDomainResponse
mkCreateDomainResponse = CreateDomainResponse'
