{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.GetAccountPasswordPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the password policy for the AWS account. For more information about using a password policy, go to <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_ManagingPasswordPolicies.html Managing an IAM Password Policy> .
module Network.AWS.IAM.GetAccountPasswordPolicy
  ( -- * Creating a request
    GetAccountPasswordPolicy (..),
    mkGetAccountPasswordPolicy,

    -- * Destructuring the response
    GetAccountPasswordPolicyResponse (..),
    mkGetAccountPasswordPolicyResponse,

    -- ** Response lenses
    gapprrsPasswordPolicy,
    gapprrsResponseStatus,
  )
where

import qualified Network.AWS.IAM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetAccountPasswordPolicy' smart constructor.
data GetAccountPasswordPolicy = GetAccountPasswordPolicy'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetAccountPasswordPolicy' value with any optional fields omitted.
mkGetAccountPasswordPolicy ::
  GetAccountPasswordPolicy
mkGetAccountPasswordPolicy = GetAccountPasswordPolicy'

instance Core.AWSRequest GetAccountPasswordPolicy where
  type Rs GetAccountPasswordPolicy = GetAccountPasswordPolicyResponse
  request x@_ =
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
            ( Core.pure ("Action", "GetAccountPasswordPolicy")
                Core.<> (Core.pure ("Version", "2010-05-08"))
            )
      }
  response =
    Response.receiveXMLWrapper
      "GetAccountPasswordPolicyResult"
      ( \s h x ->
          GetAccountPasswordPolicyResponse'
            Core.<$> (x Core..@ "PasswordPolicy")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Contains the response to a successful 'GetAccountPasswordPolicy' request.
--
-- /See:/ 'mkGetAccountPasswordPolicyResponse' smart constructor.
data GetAccountPasswordPolicyResponse = GetAccountPasswordPolicyResponse'
  { -- | A structure that contains details about the account's password policy.
    passwordPolicy :: Types.PasswordPolicy,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetAccountPasswordPolicyResponse' value with any optional fields omitted.
mkGetAccountPasswordPolicyResponse ::
  -- | 'passwordPolicy'
  Types.PasswordPolicy ->
  -- | 'responseStatus'
  Core.Int ->
  GetAccountPasswordPolicyResponse
mkGetAccountPasswordPolicyResponse passwordPolicy responseStatus =
  GetAccountPasswordPolicyResponse' {passwordPolicy, responseStatus}

-- | A structure that contains details about the account's password policy.
--
-- /Note:/ Consider using 'passwordPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gapprrsPasswordPolicy :: Lens.Lens' GetAccountPasswordPolicyResponse Types.PasswordPolicy
gapprrsPasswordPolicy = Lens.field @"passwordPolicy"
{-# DEPRECATED gapprrsPasswordPolicy "Use generic-lens or generic-optics with 'passwordPolicy' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gapprrsResponseStatus :: Lens.Lens' GetAccountPasswordPolicyResponse Core.Int
gapprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gapprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
