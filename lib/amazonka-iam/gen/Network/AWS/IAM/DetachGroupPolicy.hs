{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.DetachGroupPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the specified managed policy from the specified IAM group.
--
-- A group can also have inline policies embedded with it. To delete an inline policy, use the 'DeleteGroupPolicy' API. For information about policies, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies> in the /IAM User Guide/ .
module Network.AWS.IAM.DetachGroupPolicy
  ( -- * Creating a request
    DetachGroupPolicy (..),
    mkDetachGroupPolicy,

    -- ** Request lenses
    dgpGroupName,
    dgpPolicyArn,

    -- * Destructuring the response
    DetachGroupPolicyResponse (..),
    mkDetachGroupPolicyResponse,
  )
where

import qualified Network.AWS.IAM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDetachGroupPolicy' smart constructor.
data DetachGroupPolicy = DetachGroupPolicy'
  { -- | The name (friendly name, not ARN) of the IAM group to detach the policy from.
    --
    -- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
    groupName :: Types.GroupNameType,
    -- | The Amazon Resource Name (ARN) of the IAM policy you want to detach.
    --
    -- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
    policyArn :: Types.ArnType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DetachGroupPolicy' value with any optional fields omitted.
mkDetachGroupPolicy ::
  -- | 'groupName'
  Types.GroupNameType ->
  -- | 'policyArn'
  Types.ArnType ->
  DetachGroupPolicy
mkDetachGroupPolicy groupName policyArn =
  DetachGroupPolicy' {groupName, policyArn}

-- | The name (friendly name, not ARN) of the IAM group to detach the policy from.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgpGroupName :: Lens.Lens' DetachGroupPolicy Types.GroupNameType
dgpGroupName = Lens.field @"groupName"
{-# DEPRECATED dgpGroupName "Use generic-lens or generic-optics with 'groupName' instead." #-}

-- | The Amazon Resource Name (ARN) of the IAM policy you want to detach.
--
-- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
--
-- /Note:/ Consider using 'policyArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgpPolicyArn :: Lens.Lens' DetachGroupPolicy Types.ArnType
dgpPolicyArn = Lens.field @"policyArn"
{-# DEPRECATED dgpPolicyArn "Use generic-lens or generic-optics with 'policyArn' instead." #-}

instance Core.AWSRequest DetachGroupPolicy where
  type Rs DetachGroupPolicy = DetachGroupPolicyResponse
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
            ( Core.pure ("Action", "DetachGroupPolicy")
                Core.<> (Core.pure ("Version", "2010-05-08"))
                Core.<> (Core.toQueryValue "GroupName" groupName)
                Core.<> (Core.toQueryValue "PolicyArn" policyArn)
            )
      }
  response = Response.receiveNull DetachGroupPolicyResponse'

-- | /See:/ 'mkDetachGroupPolicyResponse' smart constructor.
data DetachGroupPolicyResponse = DetachGroupPolicyResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DetachGroupPolicyResponse' value with any optional fields omitted.
mkDetachGroupPolicyResponse ::
  DetachGroupPolicyResponse
mkDetachGroupPolicyResponse = DetachGroupPolicyResponse'
