{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.AttachUserPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attaches the specified managed policy to the specified user.
--
-- You use this API to attach a /managed/ policy to a user. To embed an inline policy in a user, use 'PutUserPolicy' .
-- For more information about policies, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies> in the /IAM User Guide/ .
module Network.AWS.IAM.AttachUserPolicy
  ( -- * Creating a request
    AttachUserPolicy (..),
    mkAttachUserPolicy,

    -- ** Request lenses
    aupUserName,
    aupPolicyArn,

    -- * Destructuring the response
    AttachUserPolicyResponse (..),
    mkAttachUserPolicyResponse,
  )
where

import qualified Network.AWS.IAM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkAttachUserPolicy' smart constructor.
data AttachUserPolicy = AttachUserPolicy'
  { -- | The name (friendly name, not ARN) of the IAM user to attach the policy to.
    --
    -- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
    userName :: Types.UserName,
    -- | The Amazon Resource Name (ARN) of the IAM policy you want to attach.
    --
    -- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
    policyArn :: Types.PolicyArn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AttachUserPolicy' value with any optional fields omitted.
mkAttachUserPolicy ::
  -- | 'userName'
  Types.UserName ->
  -- | 'policyArn'
  Types.PolicyArn ->
  AttachUserPolicy
mkAttachUserPolicy userName policyArn =
  AttachUserPolicy' {userName, policyArn}

-- | The name (friendly name, not ARN) of the IAM user to attach the policy to.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aupUserName :: Lens.Lens' AttachUserPolicy Types.UserName
aupUserName = Lens.field @"userName"
{-# DEPRECATED aupUserName "Use generic-lens or generic-optics with 'userName' instead." #-}

-- | The Amazon Resource Name (ARN) of the IAM policy you want to attach.
--
-- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
--
-- /Note:/ Consider using 'policyArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aupPolicyArn :: Lens.Lens' AttachUserPolicy Types.PolicyArn
aupPolicyArn = Lens.field @"policyArn"
{-# DEPRECATED aupPolicyArn "Use generic-lens or generic-optics with 'policyArn' instead." #-}

instance Core.AWSRequest AttachUserPolicy where
  type Rs AttachUserPolicy = AttachUserPolicyResponse
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
            ( Core.pure ("Action", "AttachUserPolicy")
                Core.<> (Core.pure ("Version", "2010-05-08"))
                Core.<> (Core.toQueryValue "UserName" userName)
                Core.<> (Core.toQueryValue "PolicyArn" policyArn)
            )
      }
  response = Response.receiveNull AttachUserPolicyResponse'

-- | /See:/ 'mkAttachUserPolicyResponse' smart constructor.
data AttachUserPolicyResponse = AttachUserPolicyResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AttachUserPolicyResponse' value with any optional fields omitted.
mkAttachUserPolicyResponse ::
  AttachUserPolicyResponse
mkAttachUserPolicyResponse = AttachUserPolicyResponse'
