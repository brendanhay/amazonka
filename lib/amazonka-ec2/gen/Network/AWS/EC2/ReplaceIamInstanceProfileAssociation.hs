{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ReplaceIamInstanceProfileAssociation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Replaces an IAM instance profile for the specified running instance. You can use this action to change the IAM instance profile that's associated with an instance without having to disassociate the existing IAM instance profile first.
--
-- Use 'DescribeIamInstanceProfileAssociations' to get the association ID.
module Network.AWS.EC2.ReplaceIamInstanceProfileAssociation
    (
    -- * Creating a request
      ReplaceIamInstanceProfileAssociation (..)
    , mkReplaceIamInstanceProfileAssociation
    -- ** Request lenses
    , riipaIamInstanceProfile
    , riipaAssociationId

    -- * Destructuring the response
    , ReplaceIamInstanceProfileAssociationResponse (..)
    , mkReplaceIamInstanceProfileAssociationResponse
    -- ** Response lenses
    , riiparrsIamInstanceProfileAssociation
    , riiparrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkReplaceIamInstanceProfileAssociation' smart constructor.
data ReplaceIamInstanceProfileAssociation = ReplaceIamInstanceProfileAssociation'
  { iamInstanceProfile :: Types.IamInstanceProfileSpecification
    -- ^ The IAM instance profile.
  , associationId :: Types.IamInstanceProfileAssociationId
    -- ^ The ID of the existing IAM instance profile association.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ReplaceIamInstanceProfileAssociation' value with any optional fields omitted.
mkReplaceIamInstanceProfileAssociation
    :: Types.IamInstanceProfileSpecification -- ^ 'iamInstanceProfile'
    -> Types.IamInstanceProfileAssociationId -- ^ 'associationId'
    -> ReplaceIamInstanceProfileAssociation
mkReplaceIamInstanceProfileAssociation iamInstanceProfile
  associationId
  = ReplaceIamInstanceProfileAssociation'{iamInstanceProfile,
                                          associationId}

-- | The IAM instance profile.
--
-- /Note:/ Consider using 'iamInstanceProfile' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riipaIamInstanceProfile :: Lens.Lens' ReplaceIamInstanceProfileAssociation Types.IamInstanceProfileSpecification
riipaIamInstanceProfile = Lens.field @"iamInstanceProfile"
{-# INLINEABLE riipaIamInstanceProfile #-}
{-# DEPRECATED iamInstanceProfile "Use generic-lens or generic-optics with 'iamInstanceProfile' instead"  #-}

-- | The ID of the existing IAM instance profile association.
--
-- /Note:/ Consider using 'associationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riipaAssociationId :: Lens.Lens' ReplaceIamInstanceProfileAssociation Types.IamInstanceProfileAssociationId
riipaAssociationId = Lens.field @"associationId"
{-# INLINEABLE riipaAssociationId #-}
{-# DEPRECATED associationId "Use generic-lens or generic-optics with 'associationId' instead"  #-}

instance Core.ToQuery ReplaceIamInstanceProfileAssociation where
        toQuery ReplaceIamInstanceProfileAssociation{..}
          = Core.toQueryPair "Action"
              ("ReplaceIamInstanceProfileAssociation" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "IamInstanceProfile" iamInstanceProfile
              Core.<> Core.toQueryPair "AssociationId" associationId

instance Core.ToHeaders ReplaceIamInstanceProfileAssociation where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ReplaceIamInstanceProfileAssociation where
        type Rs ReplaceIamInstanceProfileAssociation =
             ReplaceIamInstanceProfileAssociationResponse
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
                 ReplaceIamInstanceProfileAssociationResponse' Core.<$>
                   (x Core..@? "iamInstanceProfileAssociation") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkReplaceIamInstanceProfileAssociationResponse' smart constructor.
data ReplaceIamInstanceProfileAssociationResponse = ReplaceIamInstanceProfileAssociationResponse'
  { iamInstanceProfileAssociation :: Core.Maybe Types.IamInstanceProfileAssociation
    -- ^ Information about the IAM instance profile association.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ReplaceIamInstanceProfileAssociationResponse' value with any optional fields omitted.
mkReplaceIamInstanceProfileAssociationResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ReplaceIamInstanceProfileAssociationResponse
mkReplaceIamInstanceProfileAssociationResponse responseStatus
  = ReplaceIamInstanceProfileAssociationResponse'{iamInstanceProfileAssociation
                                                    = Core.Nothing,
                                                  responseStatus}

-- | Information about the IAM instance profile association.
--
-- /Note:/ Consider using 'iamInstanceProfileAssociation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riiparrsIamInstanceProfileAssociation :: Lens.Lens' ReplaceIamInstanceProfileAssociationResponse (Core.Maybe Types.IamInstanceProfileAssociation)
riiparrsIamInstanceProfileAssociation = Lens.field @"iamInstanceProfileAssociation"
{-# INLINEABLE riiparrsIamInstanceProfileAssociation #-}
{-# DEPRECATED iamInstanceProfileAssociation "Use generic-lens or generic-optics with 'iamInstanceProfileAssociation' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riiparrsResponseStatus :: Lens.Lens' ReplaceIamInstanceProfileAssociationResponse Core.Int
riiparrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE riiparrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
