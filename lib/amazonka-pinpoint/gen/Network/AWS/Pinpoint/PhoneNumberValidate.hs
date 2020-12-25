{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.PhoneNumberValidate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a phone number.
module Network.AWS.Pinpoint.PhoneNumberValidate
  ( -- * Creating a request
    PhoneNumberValidate (..),
    mkPhoneNumberValidate,

    -- ** Request lenses
    pnvNumberValidateRequest,

    -- * Destructuring the response
    PhoneNumberValidateResponse (..),
    mkPhoneNumberValidateResponse,

    -- ** Response lenses
    pnvrrsNumberValidateResponse,
    pnvrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkPhoneNumberValidate' smart constructor.
newtype PhoneNumberValidate = PhoneNumberValidate'
  { numberValidateRequest :: Types.NumberValidateRequest
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'PhoneNumberValidate' value with any optional fields omitted.
mkPhoneNumberValidate ::
  -- | 'numberValidateRequest'
  Types.NumberValidateRequest ->
  PhoneNumberValidate
mkPhoneNumberValidate numberValidateRequest =
  PhoneNumberValidate' {numberValidateRequest}

-- | Undocumented field.
--
-- /Note:/ Consider using 'numberValidateRequest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pnvNumberValidateRequest :: Lens.Lens' PhoneNumberValidate Types.NumberValidateRequest
pnvNumberValidateRequest = Lens.field @"numberValidateRequest"
{-# DEPRECATED pnvNumberValidateRequest "Use generic-lens or generic-optics with 'numberValidateRequest' instead." #-}

instance Core.FromJSON PhoneNumberValidate where
  toJSON PhoneNumberValidate {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("NumberValidateRequest" Core..= numberValidateRequest)
          ]
      )

instance Core.AWSRequest PhoneNumberValidate where
  type Rs PhoneNumberValidate = PhoneNumberValidateResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/v1/phone/number/validate",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          PhoneNumberValidateResponse'
            Core.<$> (Core.eitherParseJSON x) Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkPhoneNumberValidateResponse' smart constructor.
data PhoneNumberValidateResponse = PhoneNumberValidateResponse'
  { numberValidateResponse :: Types.NumberValidateResponse,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PhoneNumberValidateResponse' value with any optional fields omitted.
mkPhoneNumberValidateResponse ::
  -- | 'numberValidateResponse'
  Types.NumberValidateResponse ->
  -- | 'responseStatus'
  Core.Int ->
  PhoneNumberValidateResponse
mkPhoneNumberValidateResponse numberValidateResponse responseStatus =
  PhoneNumberValidateResponse'
    { numberValidateResponse,
      responseStatus
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'numberValidateResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pnvrrsNumberValidateResponse :: Lens.Lens' PhoneNumberValidateResponse Types.NumberValidateResponse
pnvrrsNumberValidateResponse = Lens.field @"numberValidateResponse"
{-# DEPRECATED pnvrrsNumberValidateResponse "Use generic-lens or generic-optics with 'numberValidateResponse' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pnvrrsResponseStatus :: Lens.Lens' PhoneNumberValidateResponse Core.Int
pnvrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED pnvrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
