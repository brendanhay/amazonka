{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.PutDestinationPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates an access policy associated with an existing destination. An access policy is an <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies_overview.html IAM policy document> that is used to authorize claims to register a subscription filter against a given destination.
module Network.AWS.CloudWatchLogs.PutDestinationPolicy
  ( -- * Creating a request
    PutDestinationPolicy (..),
    mkPutDestinationPolicy,

    -- ** Request lenses
    pdpDestinationName,
    pdpAccessPolicy,

    -- * Destructuring the response
    PutDestinationPolicyResponse (..),
    mkPutDestinationPolicyResponse,
  )
where

import qualified Network.AWS.CloudWatchLogs.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkPutDestinationPolicy' smart constructor.
data PutDestinationPolicy = PutDestinationPolicy'
  { -- | A name for an existing destination.
    destinationName :: Types.DestinationName,
    -- | An IAM policy document that authorizes cross-account users to deliver their log events to the associated destination. This can be up to 5120 bytes.
    accessPolicy :: Types.AccessPolicy
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutDestinationPolicy' value with any optional fields omitted.
mkPutDestinationPolicy ::
  -- | 'destinationName'
  Types.DestinationName ->
  -- | 'accessPolicy'
  Types.AccessPolicy ->
  PutDestinationPolicy
mkPutDestinationPolicy destinationName accessPolicy =
  PutDestinationPolicy' {destinationName, accessPolicy}

-- | A name for an existing destination.
--
-- /Note:/ Consider using 'destinationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdpDestinationName :: Lens.Lens' PutDestinationPolicy Types.DestinationName
pdpDestinationName = Lens.field @"destinationName"
{-# DEPRECATED pdpDestinationName "Use generic-lens or generic-optics with 'destinationName' instead." #-}

-- | An IAM policy document that authorizes cross-account users to deliver their log events to the associated destination. This can be up to 5120 bytes.
--
-- /Note:/ Consider using 'accessPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdpAccessPolicy :: Lens.Lens' PutDestinationPolicy Types.AccessPolicy
pdpAccessPolicy = Lens.field @"accessPolicy"
{-# DEPRECATED pdpAccessPolicy "Use generic-lens or generic-optics with 'accessPolicy' instead." #-}

instance Core.FromJSON PutDestinationPolicy where
  toJSON PutDestinationPolicy {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("destinationName" Core..= destinationName),
            Core.Just ("accessPolicy" Core..= accessPolicy)
          ]
      )

instance Core.AWSRequest PutDestinationPolicy where
  type Rs PutDestinationPolicy = PutDestinationPolicyResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "Logs_20140328.PutDestinationPolicy")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveNull PutDestinationPolicyResponse'

-- | /See:/ 'mkPutDestinationPolicyResponse' smart constructor.
data PutDestinationPolicyResponse = PutDestinationPolicyResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutDestinationPolicyResponse' value with any optional fields omitted.
mkPutDestinationPolicyResponse ::
  PutDestinationPolicyResponse
mkPutDestinationPolicyResponse = PutDestinationPolicyResponse'
