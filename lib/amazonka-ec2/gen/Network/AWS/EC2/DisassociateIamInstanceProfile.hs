{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DisassociateIamInstanceProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates an IAM instance profile from a running or stopped instance.
--
-- Use 'DescribeIamInstanceProfileAssociations' to get the association ID.
module Network.AWS.EC2.DisassociateIamInstanceProfile
    (
    -- * Creating a request
      DisassociateIamInstanceProfile (..)
    , mkDisassociateIamInstanceProfile
    -- ** Request lenses
    , diipAssociationId

    -- * Destructuring the response
    , DisassociateIamInstanceProfileResponse (..)
    , mkDisassociateIamInstanceProfileResponse
    -- ** Response lenses
    , diiprrsIamInstanceProfileAssociation
    , diiprrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDisassociateIamInstanceProfile' smart constructor.
newtype DisassociateIamInstanceProfile = DisassociateIamInstanceProfile'
  { associationId :: Types.IamInstanceProfileAssociationId
    -- ^ The ID of the IAM instance profile association.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DisassociateIamInstanceProfile' value with any optional fields omitted.
mkDisassociateIamInstanceProfile
    :: Types.IamInstanceProfileAssociationId -- ^ 'associationId'
    -> DisassociateIamInstanceProfile
mkDisassociateIamInstanceProfile associationId
  = DisassociateIamInstanceProfile'{associationId}

-- | The ID of the IAM instance profile association.
--
-- /Note:/ Consider using 'associationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diipAssociationId :: Lens.Lens' DisassociateIamInstanceProfile Types.IamInstanceProfileAssociationId
diipAssociationId = Lens.field @"associationId"
{-# INLINEABLE diipAssociationId #-}
{-# DEPRECATED associationId "Use generic-lens or generic-optics with 'associationId' instead"  #-}

instance Core.ToQuery DisassociateIamInstanceProfile where
        toQuery DisassociateIamInstanceProfile{..}
          = Core.toQueryPair "Action"
              ("DisassociateIamInstanceProfile" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "AssociationId" associationId

instance Core.ToHeaders DisassociateIamInstanceProfile where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DisassociateIamInstanceProfile where
        type Rs DisassociateIamInstanceProfile =
             DisassociateIamInstanceProfileResponse
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
                 DisassociateIamInstanceProfileResponse' Core.<$>
                   (x Core..@? "iamInstanceProfileAssociation") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDisassociateIamInstanceProfileResponse' smart constructor.
data DisassociateIamInstanceProfileResponse = DisassociateIamInstanceProfileResponse'
  { iamInstanceProfileAssociation :: Core.Maybe Types.IamInstanceProfileAssociation
    -- ^ Information about the IAM instance profile association.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DisassociateIamInstanceProfileResponse' value with any optional fields omitted.
mkDisassociateIamInstanceProfileResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DisassociateIamInstanceProfileResponse
mkDisassociateIamInstanceProfileResponse responseStatus
  = DisassociateIamInstanceProfileResponse'{iamInstanceProfileAssociation
                                              = Core.Nothing,
                                            responseStatus}

-- | Information about the IAM instance profile association.
--
-- /Note:/ Consider using 'iamInstanceProfileAssociation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diiprrsIamInstanceProfileAssociation :: Lens.Lens' DisassociateIamInstanceProfileResponse (Core.Maybe Types.IamInstanceProfileAssociation)
diiprrsIamInstanceProfileAssociation = Lens.field @"iamInstanceProfileAssociation"
{-# INLINEABLE diiprrsIamInstanceProfileAssociation #-}
{-# DEPRECATED iamInstanceProfileAssociation "Use generic-lens or generic-optics with 'iamInstanceProfileAssociation' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diiprrsResponseStatus :: Lens.Lens' DisassociateIamInstanceProfileResponse Core.Int
diiprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE diiprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
