{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.DescribeInputSecurityGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Produces a summary of an Input Security Group
module Network.AWS.MediaLive.DescribeInputSecurityGroup
    (
    -- * Creating a request
      DescribeInputSecurityGroup (..)
    , mkDescribeInputSecurityGroup
    -- ** Request lenses
    , disgInputSecurityGroupId

    -- * Destructuring the response
    , DescribeInputSecurityGroupResponse (..)
    , mkDescribeInputSecurityGroupResponse
    -- ** Response lenses
    , disgrfrsArn
    , disgrfrsId
    , disgrfrsInputs
    , disgrfrsState
    , disgrfrsTags
    , disgrfrsWhitelistRules
    , disgrfrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Placeholder documentation for DescribeInputSecurityGroupRequest
--
-- /See:/ 'mkDescribeInputSecurityGroup' smart constructor.
newtype DescribeInputSecurityGroup = DescribeInputSecurityGroup'
  { inputSecurityGroupId :: Core.Text
    -- ^ The id of the Input Security Group to describe
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeInputSecurityGroup' value with any optional fields omitted.
mkDescribeInputSecurityGroup
    :: Core.Text -- ^ 'inputSecurityGroupId'
    -> DescribeInputSecurityGroup
mkDescribeInputSecurityGroup inputSecurityGroupId
  = DescribeInputSecurityGroup'{inputSecurityGroupId}

-- | The id of the Input Security Group to describe
--
-- /Note:/ Consider using 'inputSecurityGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
disgInputSecurityGroupId :: Lens.Lens' DescribeInputSecurityGroup Core.Text
disgInputSecurityGroupId = Lens.field @"inputSecurityGroupId"
{-# INLINEABLE disgInputSecurityGroupId #-}
{-# DEPRECATED inputSecurityGroupId "Use generic-lens or generic-optics with 'inputSecurityGroupId' instead"  #-}

instance Core.ToQuery DescribeInputSecurityGroup where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeInputSecurityGroup where
        toHeaders DescribeInputSecurityGroup{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest DescribeInputSecurityGroup where
        type Rs DescribeInputSecurityGroup =
             DescribeInputSecurityGroupResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/prod/inputSecurityGroups/" Core.<>
                             Core.toText inputSecurityGroupId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeInputSecurityGroupResponse' Core.<$>
                   (x Core..:? "arn") Core.<*> x Core..:? "id" Core.<*>
                     x Core..:? "inputs"
                     Core.<*> x Core..:? "state"
                     Core.<*> x Core..:? "tags"
                     Core.<*> x Core..:? "whitelistRules"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Placeholder documentation for DescribeInputSecurityGroupResponse
--
-- /See:/ 'mkDescribeInputSecurityGroupResponse' smart constructor.
data DescribeInputSecurityGroupResponse = DescribeInputSecurityGroupResponse'
  { arn :: Core.Maybe Core.Text
    -- ^ Unique ARN of Input Security Group
  , id :: Core.Maybe Core.Text
    -- ^ The Id of the Input Security Group
  , inputs :: Core.Maybe [Core.Text]
    -- ^ The list of inputs currently using this Input Security Group.
  , state :: Core.Maybe Types.InputSecurityGroupState
    -- ^ The current state of the Input Security Group.
  , tags :: Core.Maybe (Core.HashMap Core.Text Core.Text)
    -- ^ A collection of key-value pairs.
  , whitelistRules :: Core.Maybe [Types.InputWhitelistRule]
    -- ^ Whitelist rules and their sync status
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeInputSecurityGroupResponse' value with any optional fields omitted.
mkDescribeInputSecurityGroupResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeInputSecurityGroupResponse
mkDescribeInputSecurityGroupResponse responseStatus
  = DescribeInputSecurityGroupResponse'{arn = Core.Nothing,
                                        id = Core.Nothing, inputs = Core.Nothing,
                                        state = Core.Nothing, tags = Core.Nothing,
                                        whitelistRules = Core.Nothing, responseStatus}

-- | Unique ARN of Input Security Group
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
disgrfrsArn :: Lens.Lens' DescribeInputSecurityGroupResponse (Core.Maybe Core.Text)
disgrfrsArn = Lens.field @"arn"
{-# INLINEABLE disgrfrsArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | The Id of the Input Security Group
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
disgrfrsId :: Lens.Lens' DescribeInputSecurityGroupResponse (Core.Maybe Core.Text)
disgrfrsId = Lens.field @"id"
{-# INLINEABLE disgrfrsId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The list of inputs currently using this Input Security Group.
--
-- /Note:/ Consider using 'inputs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
disgrfrsInputs :: Lens.Lens' DescribeInputSecurityGroupResponse (Core.Maybe [Core.Text])
disgrfrsInputs = Lens.field @"inputs"
{-# INLINEABLE disgrfrsInputs #-}
{-# DEPRECATED inputs "Use generic-lens or generic-optics with 'inputs' instead"  #-}

-- | The current state of the Input Security Group.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
disgrfrsState :: Lens.Lens' DescribeInputSecurityGroupResponse (Core.Maybe Types.InputSecurityGroupState)
disgrfrsState = Lens.field @"state"
{-# INLINEABLE disgrfrsState #-}
{-# DEPRECATED state "Use generic-lens or generic-optics with 'state' instead"  #-}

-- | A collection of key-value pairs.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
disgrfrsTags :: Lens.Lens' DescribeInputSecurityGroupResponse (Core.Maybe (Core.HashMap Core.Text Core.Text))
disgrfrsTags = Lens.field @"tags"
{-# INLINEABLE disgrfrsTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | Whitelist rules and their sync status
--
-- /Note:/ Consider using 'whitelistRules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
disgrfrsWhitelistRules :: Lens.Lens' DescribeInputSecurityGroupResponse (Core.Maybe [Types.InputWhitelistRule])
disgrfrsWhitelistRules = Lens.field @"whitelistRules"
{-# INLINEABLE disgrfrsWhitelistRules #-}
{-# DEPRECATED whitelistRules "Use generic-lens or generic-optics with 'whitelistRules' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
disgrfrsResponseStatus :: Lens.Lens' DescribeInputSecurityGroupResponse Core.Int
disgrfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE disgrfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
