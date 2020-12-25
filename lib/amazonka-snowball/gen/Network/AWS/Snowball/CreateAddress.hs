{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Snowball.CreateAddress
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an address for a Snow device to be shipped to. In most regions, addresses are validated at the time of creation. The address you provide must be located within the serviceable area of your region. If the address is invalid or unsupported, then an exception is thrown.
module Network.AWS.Snowball.CreateAddress
  ( -- * Creating a request
    CreateAddress (..),
    mkCreateAddress,

    -- ** Request lenses
    caAddress,

    -- * Destructuring the response
    CreateAddressResponse (..),
    mkCreateAddressResponse,

    -- ** Response lenses
    carrsAddressId,
    carrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Snowball.Types as Types

-- | /See:/ 'mkCreateAddress' smart constructor.
newtype CreateAddress = CreateAddress'
  { -- | The address that you want the Snow device shipped to.
    address :: Types.Address
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CreateAddress' value with any optional fields omitted.
mkCreateAddress ::
  -- | 'address'
  Types.Address ->
  CreateAddress
mkCreateAddress address = CreateAddress' {address}

-- | The address that you want the Snow device shipped to.
--
-- /Note:/ Consider using 'address' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caAddress :: Lens.Lens' CreateAddress Types.Address
caAddress = Lens.field @"address"
{-# DEPRECATED caAddress "Use generic-lens or generic-optics with 'address' instead." #-}

instance Core.FromJSON CreateAddress where
  toJSON CreateAddress {..} =
    Core.object
      (Core.catMaybes [Core.Just ("Address" Core..= address)])

instance Core.AWSRequest CreateAddress where
  type Rs CreateAddress = CreateAddressResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AWSIESnowballJobManagementService.CreateAddress")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateAddressResponse'
            Core.<$> (x Core..:? "AddressId") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateAddressResponse' smart constructor.
data CreateAddressResponse = CreateAddressResponse'
  { -- | The automatically generated ID for a specific address. You'll use this ID when you create a job to specify which address you want the Snow device for that job shipped to.
    addressId :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateAddressResponse' value with any optional fields omitted.
mkCreateAddressResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateAddressResponse
mkCreateAddressResponse responseStatus =
  CreateAddressResponse' {addressId = Core.Nothing, responseStatus}

-- | The automatically generated ID for a specific address. You'll use this ID when you create a job to specify which address you want the Snow device for that job shipped to.
--
-- /Note:/ Consider using 'addressId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
carrsAddressId :: Lens.Lens' CreateAddressResponse (Core.Maybe Types.String)
carrsAddressId = Lens.field @"addressId"
{-# DEPRECATED carrsAddressId "Use generic-lens or generic-optics with 'addressId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
carrsResponseStatus :: Lens.Lens' CreateAddressResponse Core.Int
carrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED carrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
