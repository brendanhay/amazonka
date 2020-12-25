{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AWSHealth.EnableHealthServiceAccessForOrganization
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Calling this operation enables AWS Health to work with AWS Organizations. This applies a service-linked role (SLR) to the master account in the organization. To call this operation, you must sign in as an IAM user, assume an IAM role, or sign in as the root user (not recommended) in the organization's master account.
--
-- For more information, see <https://docs.aws.amazon.com/health/latest/ug/aggregate-events.html Aggregating AWS Health events> in the /AWS Health User Guide/ .
module Network.AWS.AWSHealth.EnableHealthServiceAccessForOrganization
  ( -- * Creating a request
    EnableHealthServiceAccessForOrganization (..),
    mkEnableHealthServiceAccessForOrganization,

    -- * Destructuring the response
    EnableHealthServiceAccessForOrganizationResponse (..),
    mkEnableHealthServiceAccessForOrganizationResponse,
  )
where

import qualified Network.AWS.AWSHealth.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkEnableHealthServiceAccessForOrganization' smart constructor.
data EnableHealthServiceAccessForOrganization = EnableHealthServiceAccessForOrganization'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EnableHealthServiceAccessForOrganization' value with any optional fields omitted.
mkEnableHealthServiceAccessForOrganization ::
  EnableHealthServiceAccessForOrganization
mkEnableHealthServiceAccessForOrganization =
  EnableHealthServiceAccessForOrganization'

instance Core.FromJSON EnableHealthServiceAccessForOrganization where
  toJSON _ = Core.Object Core.mempty

instance Core.AWSRequest EnableHealthServiceAccessForOrganization where
  type
    Rs EnableHealthServiceAccessForOrganization =
      EnableHealthServiceAccessForOrganizationResponse
  request x@_ =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWSHealth_20160804.EnableHealthServiceAccessForOrganization"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveNull
      EnableHealthServiceAccessForOrganizationResponse'

-- | /See:/ 'mkEnableHealthServiceAccessForOrganizationResponse' smart constructor.
data EnableHealthServiceAccessForOrganizationResponse = EnableHealthServiceAccessForOrganizationResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EnableHealthServiceAccessForOrganizationResponse' value with any optional fields omitted.
mkEnableHealthServiceAccessForOrganizationResponse ::
  EnableHealthServiceAccessForOrganizationResponse
mkEnableHealthServiceAccessForOrganizationResponse =
  EnableHealthServiceAccessForOrganizationResponse'
