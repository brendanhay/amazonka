{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.DeleteAccountPasswordPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the password policy for the AWS account. There are no parameters.
module Network.AWS.IAM.DeleteAccountPasswordPolicy
  ( -- * Creating a request
    DeleteAccountPasswordPolicy (..),
    mkDeleteAccountPasswordPolicy,

    -- * Destructuring the response
    DeleteAccountPasswordPolicyResponse (..),
    mkDeleteAccountPasswordPolicyResponse,
  )
where

import qualified Network.AWS.IAM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteAccountPasswordPolicy' smart constructor.
data DeleteAccountPasswordPolicy = DeleteAccountPasswordPolicy'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteAccountPasswordPolicy' value with any optional fields omitted.
mkDeleteAccountPasswordPolicy ::
  DeleteAccountPasswordPolicy
mkDeleteAccountPasswordPolicy = DeleteAccountPasswordPolicy'

instance Core.AWSRequest DeleteAccountPasswordPolicy where
  type
    Rs DeleteAccountPasswordPolicy =
      DeleteAccountPasswordPolicyResponse
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
            ( Core.pure ("Action", "DeleteAccountPasswordPolicy")
                Core.<> (Core.pure ("Version", "2010-05-08"))
            )
      }
  response =
    Response.receiveNull DeleteAccountPasswordPolicyResponse'

-- | /See:/ 'mkDeleteAccountPasswordPolicyResponse' smart constructor.
data DeleteAccountPasswordPolicyResponse = DeleteAccountPasswordPolicyResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteAccountPasswordPolicyResponse' value with any optional fields omitted.
mkDeleteAccountPasswordPolicyResponse ::
  DeleteAccountPasswordPolicyResponse
mkDeleteAccountPasswordPolicyResponse =
  DeleteAccountPasswordPolicyResponse'
