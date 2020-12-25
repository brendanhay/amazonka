{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.ListProvisionedCapacity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation lists the provisioned capacity units for the specified AWS account.
module Network.AWS.Glacier.ListProvisionedCapacity
  ( -- * Creating a request
    ListProvisionedCapacity (..),
    mkListProvisionedCapacity,

    -- ** Request lenses
    lpcAccountId,

    -- * Destructuring the response
    ListProvisionedCapacityResponse (..),
    mkListProvisionedCapacityResponse,

    -- ** Response lenses
    lpcrrsProvisionedCapacityList,
    lpcrrsResponseStatus,
  )
where

import qualified Network.AWS.Glacier.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListProvisionedCapacity' smart constructor.
newtype ListProvisionedCapacity = ListProvisionedCapacity'
  { -- | The AWS account ID of the account that owns the vault. You can either specify an AWS account ID or optionally a single '-' (hyphen), in which case Amazon S3 Glacier uses the AWS account ID associated with the credentials used to sign the request. If you use an account ID, don't include any hyphens ('-') in the ID.
    accountId :: Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ListProvisionedCapacity' value with any optional fields omitted.
mkListProvisionedCapacity ::
  -- | 'accountId'
  Types.String ->
  ListProvisionedCapacity
mkListProvisionedCapacity accountId =
  ListProvisionedCapacity' {accountId}

-- | The AWS account ID of the account that owns the vault. You can either specify an AWS account ID or optionally a single '-' (hyphen), in which case Amazon S3 Glacier uses the AWS account ID associated with the credentials used to sign the request. If you use an account ID, don't include any hyphens ('-') in the ID.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpcAccountId :: Lens.Lens' ListProvisionedCapacity Types.String
lpcAccountId = Lens.field @"accountId"
{-# DEPRECATED lpcAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

instance Core.AWSRequest ListProvisionedCapacity where
  type Rs ListProvisionedCapacity = ListProvisionedCapacityResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ( "/" Core.<> (Core.toText accountId)
                Core.<> ("/provisioned-capacity")
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListProvisionedCapacityResponse'
            Core.<$> (x Core..:? "ProvisionedCapacityList")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkListProvisionedCapacityResponse' smart constructor.
data ListProvisionedCapacityResponse = ListProvisionedCapacityResponse'
  { -- | The response body contains the following JSON fields.
    provisionedCapacityList :: Core.Maybe [Types.ProvisionedCapacityDescription],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListProvisionedCapacityResponse' value with any optional fields omitted.
mkListProvisionedCapacityResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListProvisionedCapacityResponse
mkListProvisionedCapacityResponse responseStatus =
  ListProvisionedCapacityResponse'
    { provisionedCapacityList =
        Core.Nothing,
      responseStatus
    }

-- | The response body contains the following JSON fields.
--
-- /Note:/ Consider using 'provisionedCapacityList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpcrrsProvisionedCapacityList :: Lens.Lens' ListProvisionedCapacityResponse (Core.Maybe [Types.ProvisionedCapacityDescription])
lpcrrsProvisionedCapacityList = Lens.field @"provisionedCapacityList"
{-# DEPRECATED lpcrrsProvisionedCapacityList "Use generic-lens or generic-optics with 'provisionedCapacityList' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpcrrsResponseStatus :: Lens.Lens' ListProvisionedCapacityResponse Core.Int
lpcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lpcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
