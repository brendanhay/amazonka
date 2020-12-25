{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.CreatePolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an AWS IoT policy.
--
-- The created policy is the default version for the policy. This operation creates a policy version with a version identifier of __1__ and sets __1__ as the policy's default version.
module Network.AWS.IoT.CreatePolicy
  ( -- * Creating a request
    CreatePolicy (..),
    mkCreatePolicy,

    -- ** Request lenses
    cpPolicyName,
    cpPolicyDocument,
    cpTags,

    -- * Destructuring the response
    CreatePolicyResponse (..),
    mkCreatePolicyResponse,

    -- ** Response lenses
    cprrsPolicyArn,
    cprrsPolicyDocument,
    cprrsPolicyName,
    cprrsPolicyVersionId,
    cprrsResponseStatus,
  )
where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the CreatePolicy operation.
--
-- /See:/ 'mkCreatePolicy' smart constructor.
data CreatePolicy = CreatePolicy'
  { -- | The policy name.
    policyName :: Types.PolicyName,
    -- | The JSON document that describes the policy. __policyDocument__ must have a minimum length of 1, with a maximum length of 2048, excluding whitespace.
    policyDocument :: Types.PolicyDocument,
    -- | Metadata which can be used to manage the policy.
    tags :: Core.Maybe [Types.Tag]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreatePolicy' value with any optional fields omitted.
mkCreatePolicy ::
  -- | 'policyName'
  Types.PolicyName ->
  -- | 'policyDocument'
  Types.PolicyDocument ->
  CreatePolicy
mkCreatePolicy policyName policyDocument =
  CreatePolicy' {policyName, policyDocument, tags = Core.Nothing}

-- | The policy name.
--
-- /Note:/ Consider using 'policyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpPolicyName :: Lens.Lens' CreatePolicy Types.PolicyName
cpPolicyName = Lens.field @"policyName"
{-# DEPRECATED cpPolicyName "Use generic-lens or generic-optics with 'policyName' instead." #-}

-- | The JSON document that describes the policy. __policyDocument__ must have a minimum length of 1, with a maximum length of 2048, excluding whitespace.
--
-- /Note:/ Consider using 'policyDocument' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpPolicyDocument :: Lens.Lens' CreatePolicy Types.PolicyDocument
cpPolicyDocument = Lens.field @"policyDocument"
{-# DEPRECATED cpPolicyDocument "Use generic-lens or generic-optics with 'policyDocument' instead." #-}

-- | Metadata which can be used to manage the policy.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpTags :: Lens.Lens' CreatePolicy (Core.Maybe [Types.Tag])
cpTags = Lens.field @"tags"
{-# DEPRECATED cpTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromJSON CreatePolicy where
  toJSON CreatePolicy {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("policyDocument" Core..= policyDocument),
            ("tags" Core..=) Core.<$> tags
          ]
      )

instance Core.AWSRequest CreatePolicy where
  type Rs CreatePolicy = CreatePolicyResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath =
          Core.rawPath ("/policies/" Core.<> (Core.toText policyName)),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreatePolicyResponse'
            Core.<$> (x Core..:? "policyArn")
            Core.<*> (x Core..:? "policyDocument")
            Core.<*> (x Core..:? "policyName")
            Core.<*> (x Core..:? "policyVersionId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | The output from the CreatePolicy operation.
--
-- /See:/ 'mkCreatePolicyResponse' smart constructor.
data CreatePolicyResponse = CreatePolicyResponse'
  { -- | The policy ARN.
    policyArn :: Core.Maybe Types.PolicyArn,
    -- | The JSON document that describes the policy.
    policyDocument :: Core.Maybe Types.PolicyDocument,
    -- | The policy name.
    policyName :: Core.Maybe Types.PolicyName,
    -- | The policy version ID.
    policyVersionId :: Core.Maybe Types.PolicyVersionId,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreatePolicyResponse' value with any optional fields omitted.
mkCreatePolicyResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreatePolicyResponse
mkCreatePolicyResponse responseStatus =
  CreatePolicyResponse'
    { policyArn = Core.Nothing,
      policyDocument = Core.Nothing,
      policyName = Core.Nothing,
      policyVersionId = Core.Nothing,
      responseStatus
    }

-- | The policy ARN.
--
-- /Note:/ Consider using 'policyArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprrsPolicyArn :: Lens.Lens' CreatePolicyResponse (Core.Maybe Types.PolicyArn)
cprrsPolicyArn = Lens.field @"policyArn"
{-# DEPRECATED cprrsPolicyArn "Use generic-lens or generic-optics with 'policyArn' instead." #-}

-- | The JSON document that describes the policy.
--
-- /Note:/ Consider using 'policyDocument' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprrsPolicyDocument :: Lens.Lens' CreatePolicyResponse (Core.Maybe Types.PolicyDocument)
cprrsPolicyDocument = Lens.field @"policyDocument"
{-# DEPRECATED cprrsPolicyDocument "Use generic-lens or generic-optics with 'policyDocument' instead." #-}

-- | The policy name.
--
-- /Note:/ Consider using 'policyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprrsPolicyName :: Lens.Lens' CreatePolicyResponse (Core.Maybe Types.PolicyName)
cprrsPolicyName = Lens.field @"policyName"
{-# DEPRECATED cprrsPolicyName "Use generic-lens or generic-optics with 'policyName' instead." #-}

-- | The policy version ID.
--
-- /Note:/ Consider using 'policyVersionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprrsPolicyVersionId :: Lens.Lens' CreatePolicyResponse (Core.Maybe Types.PolicyVersionId)
cprrsPolicyVersionId = Lens.field @"policyVersionId"
{-# DEPRECATED cprrsPolicyVersionId "Use generic-lens or generic-optics with 'policyVersionId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprrsResponseStatus :: Lens.Lens' CreatePolicyResponse Core.Int
cprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
