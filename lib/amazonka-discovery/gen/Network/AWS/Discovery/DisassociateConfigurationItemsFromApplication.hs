{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Discovery.DisassociateConfigurationItemsFromApplication
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates one or more configuration items from an application.
module Network.AWS.Discovery.DisassociateConfigurationItemsFromApplication
    (
    -- * Creating a request
      DisassociateConfigurationItemsFromApplication (..)
    , mkDisassociateConfigurationItemsFromApplication
    -- ** Request lenses
    , dcifaApplicationConfigurationId
    , dcifaConfigurationIds

    -- * Destructuring the response
    , DisassociateConfigurationItemsFromApplicationResponse (..)
    , mkDisassociateConfigurationItemsFromApplicationResponse
    -- ** Response lenses
    , dcifarrsResponseStatus
    ) where

import qualified Network.AWS.Discovery.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDisassociateConfigurationItemsFromApplication' smart constructor.
data DisassociateConfigurationItemsFromApplication = DisassociateConfigurationItemsFromApplication'
  { applicationConfigurationId :: Types.ApplicationConfigurationId
    -- ^ Configuration ID of an application from which each item is disassociated.
  , configurationIds :: [Types.ConfigurationId]
    -- ^ Configuration ID of each item to be disassociated from an application.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DisassociateConfigurationItemsFromApplication' value with any optional fields omitted.
mkDisassociateConfigurationItemsFromApplication
    :: Types.ApplicationConfigurationId -- ^ 'applicationConfigurationId'
    -> DisassociateConfigurationItemsFromApplication
mkDisassociateConfigurationItemsFromApplication
  applicationConfigurationId
  = DisassociateConfigurationItemsFromApplication'{applicationConfigurationId,
                                                   configurationIds = Core.mempty}

-- | Configuration ID of an application from which each item is disassociated.
--
-- /Note:/ Consider using 'applicationConfigurationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcifaApplicationConfigurationId :: Lens.Lens' DisassociateConfigurationItemsFromApplication Types.ApplicationConfigurationId
dcifaApplicationConfigurationId = Lens.field @"applicationConfigurationId"
{-# INLINEABLE dcifaApplicationConfigurationId #-}
{-# DEPRECATED applicationConfigurationId "Use generic-lens or generic-optics with 'applicationConfigurationId' instead"  #-}

-- | Configuration ID of each item to be disassociated from an application.
--
-- /Note:/ Consider using 'configurationIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcifaConfigurationIds :: Lens.Lens' DisassociateConfigurationItemsFromApplication [Types.ConfigurationId]
dcifaConfigurationIds = Lens.field @"configurationIds"
{-# INLINEABLE dcifaConfigurationIds #-}
{-# DEPRECATED configurationIds "Use generic-lens or generic-optics with 'configurationIds' instead"  #-}

instance Core.ToQuery DisassociateConfigurationItemsFromApplication
         where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders
           DisassociateConfigurationItemsFromApplication
         where
        toHeaders DisassociateConfigurationItemsFromApplication{..}
          = Core.pure
              ("X-Amz-Target",
               "AWSPoseidonService_V2015_11_01.DisassociateConfigurationItemsFromApplication")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON
           DisassociateConfigurationItemsFromApplication
         where
        toJSON DisassociateConfigurationItemsFromApplication{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just
                    ("applicationConfigurationId" Core..= applicationConfigurationId),
                  Core.Just ("configurationIds" Core..= configurationIds)])

instance Core.AWSRequest
           DisassociateConfigurationItemsFromApplication
         where
        type Rs DisassociateConfigurationItemsFromApplication =
             DisassociateConfigurationItemsFromApplicationResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DisassociateConfigurationItemsFromApplicationResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDisassociateConfigurationItemsFromApplicationResponse' smart constructor.
newtype DisassociateConfigurationItemsFromApplicationResponse = DisassociateConfigurationItemsFromApplicationResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DisassociateConfigurationItemsFromApplicationResponse' value with any optional fields omitted.
mkDisassociateConfigurationItemsFromApplicationResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DisassociateConfigurationItemsFromApplicationResponse
mkDisassociateConfigurationItemsFromApplicationResponse
  responseStatus
  = DisassociateConfigurationItemsFromApplicationResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcifarrsResponseStatus :: Lens.Lens' DisassociateConfigurationItemsFromApplicationResponse Core.Int
dcifarrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dcifarrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
