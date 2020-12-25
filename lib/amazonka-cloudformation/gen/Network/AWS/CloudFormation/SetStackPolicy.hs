{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    SetStackPolicy (..),
    mkSetStackPolicy,

    -- ** Request lenses
    sspStackName,
    sspStackPolicyBody,
    sspStackPolicyURL,

    -- * Destructuring the response
    SetStackPolicyResponse (..),
    mkSetStackPolicyResponse,
  )
where

import qualified Network.AWS.CloudFormation.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the 'SetStackPolicy' action.
--
-- /See:/ 'mkSetStackPolicy' smart constructor.
data SetStackPolicy = SetStackPolicy'
  { -- | The name or unique stack ID that you want to associate a policy with.
    stackName :: Types.StackName,
    -- | Structure containing the stack policy body. For more information, go to <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/protect-stack-resources.html Prevent Updates to Stack Resources> in the AWS CloudFormation User Guide. You can specify either the @StackPolicyBody@ or the @StackPolicyURL@ parameter, but not both.
    stackPolicyBody :: Core.Maybe Types.StackPolicyBody,
    -- | Location of a file containing the stack policy. The URL must point to a policy (maximum size: 16 KB) located in an S3 bucket in the same Region as the stack. You can specify either the @StackPolicyBody@ or the @StackPolicyURL@ parameter, but not both.
    stackPolicyURL :: Core.Maybe Types.StackPolicyURL
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SetStackPolicy' value with any optional fields omitted.
mkSetStackPolicy ::
  -- | 'stackName'
  Types.StackName ->
  SetStackPolicy
mkSetStackPolicy stackName =
  SetStackPolicy'
    { stackName,
      stackPolicyBody = Core.Nothing,
      stackPolicyURL = Core.Nothing
    }

-- | The name or unique stack ID that you want to associate a policy with.
--
-- /Note:/ Consider using 'stackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sspStackName :: Lens.Lens' SetStackPolicy Types.StackName
sspStackName = Lens.field @"stackName"
{-# DEPRECATED sspStackName "Use generic-lens or generic-optics with 'stackName' instead." #-}

-- | Structure containing the stack policy body. For more information, go to <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/protect-stack-resources.html Prevent Updates to Stack Resources> in the AWS CloudFormation User Guide. You can specify either the @StackPolicyBody@ or the @StackPolicyURL@ parameter, but not both.
--
-- /Note:/ Consider using 'stackPolicyBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sspStackPolicyBody :: Lens.Lens' SetStackPolicy (Core.Maybe Types.StackPolicyBody)
sspStackPolicyBody = Lens.field @"stackPolicyBody"
{-# DEPRECATED sspStackPolicyBody "Use generic-lens or generic-optics with 'stackPolicyBody' instead." #-}

-- | Location of a file containing the stack policy. The URL must point to a policy (maximum size: 16 KB) located in an S3 bucket in the same Region as the stack. You can specify either the @StackPolicyBody@ or the @StackPolicyURL@ parameter, but not both.
--
-- /Note:/ Consider using 'stackPolicyURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sspStackPolicyURL :: Lens.Lens' SetStackPolicy (Core.Maybe Types.StackPolicyURL)
sspStackPolicyURL = Lens.field @"stackPolicyURL"
{-# DEPRECATED sspStackPolicyURL "Use generic-lens or generic-optics with 'stackPolicyURL' instead." #-}

instance Core.AWSRequest SetStackPolicy where
  type Rs SetStackPolicy = SetStackPolicyResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "SetStackPolicy")
                Core.<> (Core.pure ("Version", "2010-05-15"))
                Core.<> (Core.toQueryValue "StackName" stackName)
                Core.<> (Core.toQueryValue "StackPolicyBody" Core.<$> stackPolicyBody)
                Core.<> (Core.toQueryValue "StackPolicyURL" Core.<$> stackPolicyURL)
            )
      }
  response = Response.receiveNull SetStackPolicyResponse'

-- | /See:/ 'mkSetStackPolicyResponse' smart constructor.
data SetStackPolicyResponse = SetStackPolicyResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SetStackPolicyResponse' value with any optional fields omitted.
mkSetStackPolicyResponse ::
  SetStackPolicyResponse
mkSetStackPolicyResponse = SetStackPolicyResponse'
