{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AWSHealth.DisableHealthServiceAccessForOrganization
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables AWS Health from working with AWS Organizations. To call this operation, you must sign in as an AWS Identity and Access Management (IAM) user, assume an IAM role, or sign in as the root user (not recommended) in the organization's master AWS account. For more information, see <https://docs.aws.amazon.com/health/latest/ug/aggregate-events.html Aggregating AWS Health events> in the /AWS Health User Guide/ .
--
-- This operation doesn't remove the service-linked role (SLR) from the AWS master account in your organization. You must use the IAM console, API, or AWS Command Line Interface (AWS CLI) to remove the SLR. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/using-service-linked-roles.html#delete-service-linked-role Deleting a Service-Linked Role> in the /IAM User Guide/ .
module Network.AWS.AWSHealth.DisableHealthServiceAccessForOrganization
  ( -- * Creating a request
    DisableHealthServiceAccessForOrganization (..),
    mkDisableHealthServiceAccessForOrganization,

    -- * Destructuring the response
    DisableHealthServiceAccessForOrganizationResponse (..),
    mkDisableHealthServiceAccessForOrganizationResponse,
  )
where

import qualified Network.AWS.AWSHealth.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDisableHealthServiceAccessForOrganization' smart constructor.
data DisableHealthServiceAccessForOrganization = DisableHealthServiceAccessForOrganization'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DisableHealthServiceAccessForOrganization' value with any optional fields omitted.
mkDisableHealthServiceAccessForOrganization ::
  DisableHealthServiceAccessForOrganization
mkDisableHealthServiceAccessForOrganization =
  DisableHealthServiceAccessForOrganization'

instance Core.FromJSON DisableHealthServiceAccessForOrganization where
  toJSON _ = Core.Object Core.mempty

instance Core.AWSRequest DisableHealthServiceAccessForOrganization where
  type
    Rs DisableHealthServiceAccessForOrganization =
      DisableHealthServiceAccessForOrganizationResponse
  request x@_ =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWSHealth_20160804.DisableHealthServiceAccessForOrganization"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveNull
      DisableHealthServiceAccessForOrganizationResponse'

-- | /See:/ 'mkDisableHealthServiceAccessForOrganizationResponse' smart constructor.
data DisableHealthServiceAccessForOrganizationResponse = DisableHealthServiceAccessForOrganizationResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DisableHealthServiceAccessForOrganizationResponse' value with any optional fields omitted.
mkDisableHealthServiceAccessForOrganizationResponse ::
  DisableHealthServiceAccessForOrganizationResponse
mkDisableHealthServiceAccessForOrganizationResponse =
  DisableHealthServiceAccessForOrganizationResponse'
