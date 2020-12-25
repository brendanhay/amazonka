{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.DisassociateAdminAccount
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates the account that has been set as the AWS Firewall Manager administrator account. To set a different account as the administrator account, you must submit an @AssociateAdminAccount@ request.
module Network.AWS.FMS.DisassociateAdminAccount
  ( -- * Creating a request
    DisassociateAdminAccount (..),
    mkDisassociateAdminAccount,

    -- * Destructuring the response
    DisassociateAdminAccountResponse (..),
    mkDisassociateAdminAccountResponse,
  )
where

import qualified Network.AWS.FMS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDisassociateAdminAccount' smart constructor.
data DisassociateAdminAccount = DisassociateAdminAccount'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DisassociateAdminAccount' value with any optional fields omitted.
mkDisassociateAdminAccount ::
  DisassociateAdminAccount
mkDisassociateAdminAccount = DisassociateAdminAccount'

instance Core.FromJSON DisassociateAdminAccount where
  toJSON _ = Core.Object Core.mempty

instance Core.AWSRequest DisassociateAdminAccount where
  type Rs DisassociateAdminAccount = DisassociateAdminAccountResponse
  request x@_ =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AWSFMS_20180101.DisassociateAdminAccount")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveNull DisassociateAdminAccountResponse'

-- | /See:/ 'mkDisassociateAdminAccountResponse' smart constructor.
data DisassociateAdminAccountResponse = DisassociateAdminAccountResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DisassociateAdminAccountResponse' value with any optional fields omitted.
mkDisassociateAdminAccountResponse ::
  DisassociateAdminAccountResponse
mkDisassociateAdminAccountResponse =
  DisassociateAdminAccountResponse'
