{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.CreateAuditSuppression
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a Device Defender audit suppression. 
module Network.AWS.IoT.CreateAuditSuppression
    (
    -- * Creating a request
      CreateAuditSuppression (..)
    , mkCreateAuditSuppression
    -- ** Request lenses
    , casCheckName
    , casResourceIdentifier
    , casClientRequestToken
    , casDescription
    , casExpirationDate
    , casSuppressIndefinitely

    -- * Destructuring the response
    , CreateAuditSuppressionResponse (..)
    , mkCreateAuditSuppressionResponse
    -- ** Response lenses
    , casrrsResponseStatus
    ) where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateAuditSuppression' smart constructor.
data CreateAuditSuppression = CreateAuditSuppression'
  { checkName :: Types.CheckName
  , resourceIdentifier :: Types.ResourceIdentifier
  , clientRequestToken :: Types.ClientRequestToken
    -- ^ The epoch timestamp in seconds at which this suppression expires. 
  , description :: Core.Maybe Types.Description
    -- ^ The description of the audit suppression. 
  , expirationDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The epoch timestamp in seconds at which this suppression expires. 
  , suppressIndefinitely :: Core.Maybe Core.Bool
    -- ^ Indicates whether a suppression should exist indefinitely or not. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'CreateAuditSuppression' value with any optional fields omitted.
mkCreateAuditSuppression
    :: Types.CheckName -- ^ 'checkName'
    -> Types.ResourceIdentifier -- ^ 'resourceIdentifier'
    -> Types.ClientRequestToken -- ^ 'clientRequestToken'
    -> CreateAuditSuppression
mkCreateAuditSuppression checkName resourceIdentifier
  clientRequestToken
  = CreateAuditSuppression'{checkName, resourceIdentifier,
                            clientRequestToken, description = Core.Nothing,
                            expirationDate = Core.Nothing, suppressIndefinitely = Core.Nothing}

-- | Undocumented field.
--
-- /Note:/ Consider using 'checkName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
casCheckName :: Lens.Lens' CreateAuditSuppression Types.CheckName
casCheckName = Lens.field @"checkName"
{-# INLINEABLE casCheckName #-}
{-# DEPRECATED checkName "Use generic-lens or generic-optics with 'checkName' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'resourceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
casResourceIdentifier :: Lens.Lens' CreateAuditSuppression Types.ResourceIdentifier
casResourceIdentifier = Lens.field @"resourceIdentifier"
{-# INLINEABLE casResourceIdentifier #-}
{-# DEPRECATED resourceIdentifier "Use generic-lens or generic-optics with 'resourceIdentifier' instead"  #-}

-- | The epoch timestamp in seconds at which this suppression expires. 
--
-- /Note:/ Consider using 'clientRequestToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
casClientRequestToken :: Lens.Lens' CreateAuditSuppression Types.ClientRequestToken
casClientRequestToken = Lens.field @"clientRequestToken"
{-# INLINEABLE casClientRequestToken #-}
{-# DEPRECATED clientRequestToken "Use generic-lens or generic-optics with 'clientRequestToken' instead"  #-}

-- | The description of the audit suppression. 
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
casDescription :: Lens.Lens' CreateAuditSuppression (Core.Maybe Types.Description)
casDescription = Lens.field @"description"
{-# INLINEABLE casDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The epoch timestamp in seconds at which this suppression expires. 
--
-- /Note:/ Consider using 'expirationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
casExpirationDate :: Lens.Lens' CreateAuditSuppression (Core.Maybe Core.NominalDiffTime)
casExpirationDate = Lens.field @"expirationDate"
{-# INLINEABLE casExpirationDate #-}
{-# DEPRECATED expirationDate "Use generic-lens or generic-optics with 'expirationDate' instead"  #-}

-- | Indicates whether a suppression should exist indefinitely or not. 
--
-- /Note:/ Consider using 'suppressIndefinitely' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
casSuppressIndefinitely :: Lens.Lens' CreateAuditSuppression (Core.Maybe Core.Bool)
casSuppressIndefinitely = Lens.field @"suppressIndefinitely"
{-# INLINEABLE casSuppressIndefinitely #-}
{-# DEPRECATED suppressIndefinitely "Use generic-lens or generic-optics with 'suppressIndefinitely' instead"  #-}

instance Core.ToQuery CreateAuditSuppression where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateAuditSuppression where
        toHeaders _ = Core.pure Core.mempty

instance Core.FromJSON CreateAuditSuppression where
        toJSON CreateAuditSuppression{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("checkName" Core..= checkName),
                  Core.Just ("resourceIdentifier" Core..= resourceIdentifier),
                  Core.Just ("clientRequestToken" Core..= clientRequestToken),
                  ("description" Core..=) Core.<$> description,
                  ("expirationDate" Core..=) Core.<$> expirationDate,
                  ("suppressIndefinitely" Core..=) Core.<$> suppressIndefinitely])

instance Core.AWSRequest CreateAuditSuppression where
        type Rs CreateAuditSuppression = CreateAuditSuppressionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath = "/audit/suppressions/create",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 CreateAuditSuppressionResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateAuditSuppressionResponse' smart constructor.
newtype CreateAuditSuppressionResponse = CreateAuditSuppressionResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CreateAuditSuppressionResponse' value with any optional fields omitted.
mkCreateAuditSuppressionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateAuditSuppressionResponse
mkCreateAuditSuppressionResponse responseStatus
  = CreateAuditSuppressionResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
casrrsResponseStatus :: Lens.Lens' CreateAuditSuppressionResponse Core.Int
casrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE casrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
