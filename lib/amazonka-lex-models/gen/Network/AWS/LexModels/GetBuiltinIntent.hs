{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.GetBuiltinIntent
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a built-in intent.
--
-- This operation requires permission for the @lex:GetBuiltinIntent@ action.
module Network.AWS.LexModels.GetBuiltinIntent
  ( -- * Creating a request
    GetBuiltinIntent (..),
    mkGetBuiltinIntent,

    -- ** Request lenses
    gbiSignature,

    -- * Destructuring the response
    GetBuiltinIntentResponse (..),
    mkGetBuiltinIntentResponse,

    -- ** Response lenses
    gbirrsSignature,
    gbirrsSlots,
    gbirrsSupportedLocales,
    gbirrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.LexModels.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetBuiltinIntent' smart constructor.
newtype GetBuiltinIntent = GetBuiltinIntent'
  { -- | The unique identifier for a built-in intent. To find the signature for an intent, see <https://developer.amazon.com/public/solutions/alexa/alexa-skills-kit/docs/built-in-intent-ref/standard-intents Standard Built-in Intents> in the /Alexa Skills Kit/ .
    signature :: Types.Signature
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetBuiltinIntent' value with any optional fields omitted.
mkGetBuiltinIntent ::
  -- | 'signature'
  Types.Signature ->
  GetBuiltinIntent
mkGetBuiltinIntent signature = GetBuiltinIntent' {signature}

-- | The unique identifier for a built-in intent. To find the signature for an intent, see <https://developer.amazon.com/public/solutions/alexa/alexa-skills-kit/docs/built-in-intent-ref/standard-intents Standard Built-in Intents> in the /Alexa Skills Kit/ .
--
-- /Note:/ Consider using 'signature' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbiSignature :: Lens.Lens' GetBuiltinIntent Types.Signature
gbiSignature = Lens.field @"signature"
{-# DEPRECATED gbiSignature "Use generic-lens or generic-optics with 'signature' instead." #-}

instance Core.AWSRequest GetBuiltinIntent where
  type Rs GetBuiltinIntent = GetBuiltinIntentResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ("/builtins/intents/" Core.<> (Core.toText signature)),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetBuiltinIntentResponse'
            Core.<$> (x Core..:? "signature")
            Core.<*> (x Core..:? "slots")
            Core.<*> (x Core..:? "supportedLocales")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetBuiltinIntentResponse' smart constructor.
data GetBuiltinIntentResponse = GetBuiltinIntentResponse'
  { -- | The unique identifier for a built-in intent.
    signature :: Core.Maybe Types.Signature,
    -- | An array of @BuiltinIntentSlot@ objects, one entry for each slot type in the intent.
    slots :: Core.Maybe [Types.BuiltinIntentSlot],
    -- | A list of locales that the intent supports.
    supportedLocales :: Core.Maybe [Types.Locale],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetBuiltinIntentResponse' value with any optional fields omitted.
mkGetBuiltinIntentResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetBuiltinIntentResponse
mkGetBuiltinIntentResponse responseStatus =
  GetBuiltinIntentResponse'
    { signature = Core.Nothing,
      slots = Core.Nothing,
      supportedLocales = Core.Nothing,
      responseStatus
    }

-- | The unique identifier for a built-in intent.
--
-- /Note:/ Consider using 'signature' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbirrsSignature :: Lens.Lens' GetBuiltinIntentResponse (Core.Maybe Types.Signature)
gbirrsSignature = Lens.field @"signature"
{-# DEPRECATED gbirrsSignature "Use generic-lens or generic-optics with 'signature' instead." #-}

-- | An array of @BuiltinIntentSlot@ objects, one entry for each slot type in the intent.
--
-- /Note:/ Consider using 'slots' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbirrsSlots :: Lens.Lens' GetBuiltinIntentResponse (Core.Maybe [Types.BuiltinIntentSlot])
gbirrsSlots = Lens.field @"slots"
{-# DEPRECATED gbirrsSlots "Use generic-lens or generic-optics with 'slots' instead." #-}

-- | A list of locales that the intent supports.
--
-- /Note:/ Consider using 'supportedLocales' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbirrsSupportedLocales :: Lens.Lens' GetBuiltinIntentResponse (Core.Maybe [Types.Locale])
gbirrsSupportedLocales = Lens.field @"supportedLocales"
{-# DEPRECATED gbirrsSupportedLocales "Use generic-lens or generic-optics with 'supportedLocales' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbirrsResponseStatus :: Lens.Lens' GetBuiltinIntentResponse Core.Int
gbirrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gbirrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
