{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.GetStackPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the stack policy for a specified stack. If a stack doesn't have a policy, a null value is returned.
module Network.AWS.CloudFormation.GetStackPolicy
    (
    -- * Creating a request
      GetStackPolicy (..)
    , mkGetStackPolicy
    -- ** Request lenses
    , gspStackName

    -- * Destructuring the response
    , GetStackPolicyResponse (..)
    , mkGetStackPolicyResponse
    -- ** Response lenses
    , gsprrsStackPolicyBody
    , gsprrsResponseStatus
    ) where

import qualified Network.AWS.CloudFormation.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the 'GetStackPolicy' action.
--
-- /See:/ 'mkGetStackPolicy' smart constructor.
newtype GetStackPolicy = GetStackPolicy'
  { stackName :: Types.StackName
    -- ^ The name or unique stack ID that is associated with the stack whose policy you want to get.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetStackPolicy' value with any optional fields omitted.
mkGetStackPolicy
    :: Types.StackName -- ^ 'stackName'
    -> GetStackPolicy
mkGetStackPolicy stackName = GetStackPolicy'{stackName}

-- | The name or unique stack ID that is associated with the stack whose policy you want to get.
--
-- /Note:/ Consider using 'stackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gspStackName :: Lens.Lens' GetStackPolicy Types.StackName
gspStackName = Lens.field @"stackName"
{-# INLINEABLE gspStackName #-}
{-# DEPRECATED stackName "Use generic-lens or generic-optics with 'stackName' instead"  #-}

instance Core.ToQuery GetStackPolicy where
        toQuery GetStackPolicy{..}
          = Core.toQueryPair "Action" ("GetStackPolicy" :: Core.Text) Core.<>
              Core.toQueryPair "Version" ("2010-05-15" :: Core.Text)
              Core.<> Core.toQueryPair "StackName" stackName

instance Core.ToHeaders GetStackPolicy where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest GetStackPolicy where
        type Rs GetStackPolicy = GetStackPolicyResponse
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
          = Response.receiveXMLWrapper "GetStackPolicyResult"
              (\ s h x ->
                 GetStackPolicyResponse' Core.<$>
                   (x Core..@? "StackPolicyBody") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | The output for the 'GetStackPolicy' action.
--
-- /See:/ 'mkGetStackPolicyResponse' smart constructor.
data GetStackPolicyResponse = GetStackPolicyResponse'
  { stackPolicyBody :: Core.Maybe Types.StackPolicyBody
    -- ^ Structure containing the stack policy body. (For more information, go to <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/protect-stack-resources.html Prevent Updates to Stack Resources> in the AWS CloudFormation User Guide.)
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetStackPolicyResponse' value with any optional fields omitted.
mkGetStackPolicyResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetStackPolicyResponse
mkGetStackPolicyResponse responseStatus
  = GetStackPolicyResponse'{stackPolicyBody = Core.Nothing,
                            responseStatus}

-- | Structure containing the stack policy body. (For more information, go to <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/protect-stack-resources.html Prevent Updates to Stack Resources> in the AWS CloudFormation User Guide.)
--
-- /Note:/ Consider using 'stackPolicyBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsprrsStackPolicyBody :: Lens.Lens' GetStackPolicyResponse (Core.Maybe Types.StackPolicyBody)
gsprrsStackPolicyBody = Lens.field @"stackPolicyBody"
{-# INLINEABLE gsprrsStackPolicyBody #-}
{-# DEPRECATED stackPolicyBody "Use generic-lens or generic-optics with 'stackPolicyBody' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsprrsResponseStatus :: Lens.Lens' GetStackPolicyResponse Core.Int
gsprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gsprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
