{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.SetStackPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets a stack policy for a specified stack.
module Network.AWS.CloudFormation.SetStackPolicy
    (
    -- * Creating a request
      SetStackPolicy (..)
    , mkSetStackPolicy
    -- ** Request lenses
    , sspStackName
    , sspStackPolicyBody
    , sspStackPolicyURL

    -- * Destructuring the response
    , SetStackPolicyResponse (..)
    , mkSetStackPolicyResponse
    ) where

import qualified Network.AWS.CloudFormation.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the 'SetStackPolicy' action.
--
-- /See:/ 'mkSetStackPolicy' smart constructor.
data SetStackPolicy = SetStackPolicy'
  { stackName :: Types.StackName
    -- ^ The name or unique stack ID that you want to associate a policy with.
  , stackPolicyBody :: Core.Maybe Types.StackPolicyBody
    -- ^ Structure containing the stack policy body. For more information, go to <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/protect-stack-resources.html Prevent Updates to Stack Resources> in the AWS CloudFormation User Guide. You can specify either the @StackPolicyBody@ or the @StackPolicyURL@ parameter, but not both.
  , stackPolicyURL :: Core.Maybe Types.StackPolicyURL
    -- ^ Location of a file containing the stack policy. The URL must point to a policy (maximum size: 16 KB) located in an S3 bucket in the same Region as the stack. You can specify either the @StackPolicyBody@ or the @StackPolicyURL@ parameter, but not both.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SetStackPolicy' value with any optional fields omitted.
mkSetStackPolicy
    :: Types.StackName -- ^ 'stackName'
    -> SetStackPolicy
mkSetStackPolicy stackName
  = SetStackPolicy'{stackName, stackPolicyBody = Core.Nothing,
                    stackPolicyURL = Core.Nothing}

-- | The name or unique stack ID that you want to associate a policy with.
--
-- /Note:/ Consider using 'stackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sspStackName :: Lens.Lens' SetStackPolicy Types.StackName
sspStackName = Lens.field @"stackName"
{-# INLINEABLE sspStackName #-}
{-# DEPRECATED stackName "Use generic-lens or generic-optics with 'stackName' instead"  #-}

-- | Structure containing the stack policy body. For more information, go to <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/protect-stack-resources.html Prevent Updates to Stack Resources> in the AWS CloudFormation User Guide. You can specify either the @StackPolicyBody@ or the @StackPolicyURL@ parameter, but not both.
--
-- /Note:/ Consider using 'stackPolicyBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sspStackPolicyBody :: Lens.Lens' SetStackPolicy (Core.Maybe Types.StackPolicyBody)
sspStackPolicyBody = Lens.field @"stackPolicyBody"
{-# INLINEABLE sspStackPolicyBody #-}
{-# DEPRECATED stackPolicyBody "Use generic-lens or generic-optics with 'stackPolicyBody' instead"  #-}

-- | Location of a file containing the stack policy. The URL must point to a policy (maximum size: 16 KB) located in an S3 bucket in the same Region as the stack. You can specify either the @StackPolicyBody@ or the @StackPolicyURL@ parameter, but not both.
--
-- /Note:/ Consider using 'stackPolicyURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sspStackPolicyURL :: Lens.Lens' SetStackPolicy (Core.Maybe Types.StackPolicyURL)
sspStackPolicyURL = Lens.field @"stackPolicyURL"
{-# INLINEABLE sspStackPolicyURL #-}
{-# DEPRECATED stackPolicyURL "Use generic-lens or generic-optics with 'stackPolicyURL' instead"  #-}

instance Core.ToQuery SetStackPolicy where
        toQuery SetStackPolicy{..}
          = Core.toQueryPair "Action" ("SetStackPolicy" :: Core.Text) Core.<>
              Core.toQueryPair "Version" ("2010-05-15" :: Core.Text)
              Core.<> Core.toQueryPair "StackName" stackName
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "StackPolicyBody")
                stackPolicyBody
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "StackPolicyURL")
                stackPolicyURL

instance Core.ToHeaders SetStackPolicy where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest SetStackPolicy where
        type Rs SetStackPolicy = SetStackPolicyResponse
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
        parseResponse = Response.receiveNull SetStackPolicyResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkSetStackPolicyResponse' smart constructor.
data SetStackPolicyResponse = SetStackPolicyResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SetStackPolicyResponse' value with any optional fields omitted.
mkSetStackPolicyResponse
    :: SetStackPolicyResponse
mkSetStackPolicyResponse = SetStackPolicyResponse'
