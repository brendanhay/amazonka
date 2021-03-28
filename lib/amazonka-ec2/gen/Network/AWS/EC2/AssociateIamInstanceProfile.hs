{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.AssociateIamInstanceProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates an IAM instance profile with a running or stopped instance. You cannot associate more than one IAM instance profile with an instance.
module Network.AWS.EC2.AssociateIamInstanceProfile
    (
    -- * Creating a request
      AssociateIamInstanceProfile (..)
    , mkAssociateIamInstanceProfile
    -- ** Request lenses
    , aiipIamInstanceProfile
    , aiipInstanceId

    -- * Destructuring the response
    , AssociateIamInstanceProfileResponse (..)
    , mkAssociateIamInstanceProfileResponse
    -- ** Response lenses
    , aiiprrsIamInstanceProfileAssociation
    , aiiprrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkAssociateIamInstanceProfile' smart constructor.
data AssociateIamInstanceProfile = AssociateIamInstanceProfile'
  { iamInstanceProfile :: Types.IamInstanceProfileSpecification
    -- ^ The IAM instance profile.
  , instanceId :: Types.InstanceId
    -- ^ The ID of the instance.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AssociateIamInstanceProfile' value with any optional fields omitted.
mkAssociateIamInstanceProfile
    :: Types.IamInstanceProfileSpecification -- ^ 'iamInstanceProfile'
    -> Types.InstanceId -- ^ 'instanceId'
    -> AssociateIamInstanceProfile
mkAssociateIamInstanceProfile iamInstanceProfile instanceId
  = AssociateIamInstanceProfile'{iamInstanceProfile, instanceId}

-- | The IAM instance profile.
--
-- /Note:/ Consider using 'iamInstanceProfile' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aiipIamInstanceProfile :: Lens.Lens' AssociateIamInstanceProfile Types.IamInstanceProfileSpecification
aiipIamInstanceProfile = Lens.field @"iamInstanceProfile"
{-# INLINEABLE aiipIamInstanceProfile #-}
{-# DEPRECATED iamInstanceProfile "Use generic-lens or generic-optics with 'iamInstanceProfile' instead"  #-}

-- | The ID of the instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aiipInstanceId :: Lens.Lens' AssociateIamInstanceProfile Types.InstanceId
aiipInstanceId = Lens.field @"instanceId"
{-# INLINEABLE aiipInstanceId #-}
{-# DEPRECATED instanceId "Use generic-lens or generic-optics with 'instanceId' instead"  #-}

instance Core.ToQuery AssociateIamInstanceProfile where
        toQuery AssociateIamInstanceProfile{..}
          = Core.toQueryPair "Action"
              ("AssociateIamInstanceProfile" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "IamInstanceProfile" iamInstanceProfile
              Core.<> Core.toQueryPair "InstanceId" instanceId

instance Core.ToHeaders AssociateIamInstanceProfile where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest AssociateIamInstanceProfile where
        type Rs AssociateIamInstanceProfile =
             AssociateIamInstanceProfileResponse
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
          = Response.receiveXML
              (\ s h x ->
                 AssociateIamInstanceProfileResponse' Core.<$>
                   (x Core..@? "iamInstanceProfileAssociation") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkAssociateIamInstanceProfileResponse' smart constructor.
data AssociateIamInstanceProfileResponse = AssociateIamInstanceProfileResponse'
  { iamInstanceProfileAssociation :: Core.Maybe Types.IamInstanceProfileAssociation
    -- ^ Information about the IAM instance profile association.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'AssociateIamInstanceProfileResponse' value with any optional fields omitted.
mkAssociateIamInstanceProfileResponse
    :: Core.Int -- ^ 'responseStatus'
    -> AssociateIamInstanceProfileResponse
mkAssociateIamInstanceProfileResponse responseStatus
  = AssociateIamInstanceProfileResponse'{iamInstanceProfileAssociation
                                           = Core.Nothing,
                                         responseStatus}

-- | Information about the IAM instance profile association.
--
-- /Note:/ Consider using 'iamInstanceProfileAssociation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aiiprrsIamInstanceProfileAssociation :: Lens.Lens' AssociateIamInstanceProfileResponse (Core.Maybe Types.IamInstanceProfileAssociation)
aiiprrsIamInstanceProfileAssociation = Lens.field @"iamInstanceProfileAssociation"
{-# INLINEABLE aiiprrsIamInstanceProfileAssociation #-}
{-# DEPRECATED iamInstanceProfileAssociation "Use generic-lens or generic-optics with 'iamInstanceProfileAssociation' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aiiprrsResponseStatus :: Lens.Lens' AssociateIamInstanceProfileResponse Core.Int
aiiprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE aiiprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
