{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.DescribeCrossAccountAccessRole
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the IAM role that enables Amazon Inspector to access your AWS account.
module Network.AWS.Inspector.DescribeCrossAccountAccessRole
    (
    -- * Creating a request
      DescribeCrossAccountAccessRole (..)
    , mkDescribeCrossAccountAccessRole

    -- * Destructuring the response
    , DescribeCrossAccountAccessRoleResponse (..)
    , mkDescribeCrossAccountAccessRoleResponse
    -- ** Response lenses
    , dcaarrrsRoleArn
    , dcaarrrsValid
    , dcaarrrsRegisteredAt
    , dcaarrrsResponseStatus
    ) where

import qualified Network.AWS.Inspector.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeCrossAccountAccessRole' smart constructor.
data DescribeCrossAccountAccessRole = DescribeCrossAccountAccessRole'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeCrossAccountAccessRole' value with any optional fields omitted.
mkDescribeCrossAccountAccessRole
    :: DescribeCrossAccountAccessRole
mkDescribeCrossAccountAccessRole = DescribeCrossAccountAccessRole'

instance Core.ToQuery DescribeCrossAccountAccessRole where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeCrossAccountAccessRole where
        toHeaders DescribeCrossAccountAccessRole{..}
          = Core.pure
              ("X-Amz-Target", "InspectorService.DescribeCrossAccountAccessRole")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeCrossAccountAccessRole where
        toJSON _ = Core.Object Core.mempty

instance Core.AWSRequest DescribeCrossAccountAccessRole where
        type Rs DescribeCrossAccountAccessRole =
             DescribeCrossAccountAccessRoleResponse
        toRequest x@_
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeCrossAccountAccessRoleResponse' Core.<$>
                   (x Core..: "roleArn") Core.<*> x Core..: "valid" Core.<*>
                     x Core..: "registeredAt"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeCrossAccountAccessRoleResponse' smart constructor.
data DescribeCrossAccountAccessRoleResponse = DescribeCrossAccountAccessRoleResponse'
  { roleArn :: Types.Arn
    -- ^ The ARN that specifies the IAM role that Amazon Inspector uses to access your AWS account.
  , valid :: Core.Bool
    -- ^ A Boolean value that specifies whether the IAM role has the necessary policies attached to enable Amazon Inspector to access your AWS account.
  , registeredAt :: Core.NominalDiffTime
    -- ^ The date when the cross-account access role was registered.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeCrossAccountAccessRoleResponse' value with any optional fields omitted.
mkDescribeCrossAccountAccessRoleResponse
    :: Types.Arn -- ^ 'roleArn'
    -> Core.Bool -- ^ 'valid'
    -> Core.NominalDiffTime -- ^ 'registeredAt'
    -> Core.Int -- ^ 'responseStatus'
    -> DescribeCrossAccountAccessRoleResponse
mkDescribeCrossAccountAccessRoleResponse roleArn valid registeredAt
  responseStatus
  = DescribeCrossAccountAccessRoleResponse'{roleArn, valid,
                                            registeredAt, responseStatus}

-- | The ARN that specifies the IAM role that Amazon Inspector uses to access your AWS account.
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcaarrrsRoleArn :: Lens.Lens' DescribeCrossAccountAccessRoleResponse Types.Arn
dcaarrrsRoleArn = Lens.field @"roleArn"
{-# INLINEABLE dcaarrrsRoleArn #-}
{-# DEPRECATED roleArn "Use generic-lens or generic-optics with 'roleArn' instead"  #-}

-- | A Boolean value that specifies whether the IAM role has the necessary policies attached to enable Amazon Inspector to access your AWS account.
--
-- /Note:/ Consider using 'valid' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcaarrrsValid :: Lens.Lens' DescribeCrossAccountAccessRoleResponse Core.Bool
dcaarrrsValid = Lens.field @"valid"
{-# INLINEABLE dcaarrrsValid #-}
{-# DEPRECATED valid "Use generic-lens or generic-optics with 'valid' instead"  #-}

-- | The date when the cross-account access role was registered.
--
-- /Note:/ Consider using 'registeredAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcaarrrsRegisteredAt :: Lens.Lens' DescribeCrossAccountAccessRoleResponse Core.NominalDiffTime
dcaarrrsRegisteredAt = Lens.field @"registeredAt"
{-# INLINEABLE dcaarrrsRegisteredAt #-}
{-# DEPRECATED registeredAt "Use generic-lens or generic-optics with 'registeredAt' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcaarrrsResponseStatus :: Lens.Lens' DescribeCrossAccountAccessRoleResponse Core.Int
dcaarrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dcaarrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
