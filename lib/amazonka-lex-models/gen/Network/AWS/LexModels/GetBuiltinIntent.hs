{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    gbirsSignature,
    gbirsSlots,
    gbirsSupportedLocales,
    gbirsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.LexModels.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetBuiltinIntent' smart constructor.
newtype GetBuiltinIntent = GetBuiltinIntent'
  { signature ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetBuiltinIntent' with the minimum fields required to make a request.
--
-- * 'signature' - The unique identifier for a built-in intent. To find the signature for an intent, see <https://developer.amazon.com/public/solutions/alexa/alexa-skills-kit/docs/built-in-intent-ref/standard-intents Standard Built-in Intents> in the /Alexa Skills Kit/ .
mkGetBuiltinIntent ::
  -- | 'signature'
  Lude.Text ->
  GetBuiltinIntent
mkGetBuiltinIntent pSignature_ =
  GetBuiltinIntent' {signature = pSignature_}

-- | The unique identifier for a built-in intent. To find the signature for an intent, see <https://developer.amazon.com/public/solutions/alexa/alexa-skills-kit/docs/built-in-intent-ref/standard-intents Standard Built-in Intents> in the /Alexa Skills Kit/ .
--
-- /Note:/ Consider using 'signature' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbiSignature :: Lens.Lens' GetBuiltinIntent Lude.Text
gbiSignature = Lens.lens (signature :: GetBuiltinIntent -> Lude.Text) (\s a -> s {signature = a} :: GetBuiltinIntent)
{-# DEPRECATED gbiSignature "Use generic-lens or generic-optics with 'signature' instead." #-}

instance Lude.AWSRequest GetBuiltinIntent where
  type Rs GetBuiltinIntent = GetBuiltinIntentResponse
  request = Req.get lexModelsService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetBuiltinIntentResponse'
            Lude.<$> (x Lude..?> "signature")
            Lude.<*> (x Lude..?> "slots" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "supportedLocales" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetBuiltinIntent where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath GetBuiltinIntent where
  toPath GetBuiltinIntent' {..} =
    Lude.mconcat ["/builtins/intents/", Lude.toBS signature]

instance Lude.ToQuery GetBuiltinIntent where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetBuiltinIntentResponse' smart constructor.
data GetBuiltinIntentResponse = GetBuiltinIntentResponse'
  { signature ::
      Lude.Maybe Lude.Text,
    slots :: Lude.Maybe [BuiltinIntentSlot],
    supportedLocales :: Lude.Maybe [Locale],
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetBuiltinIntentResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'signature' - The unique identifier for a built-in intent.
-- * 'slots' - An array of @BuiltinIntentSlot@ objects, one entry for each slot type in the intent.
-- * 'supportedLocales' - A list of locales that the intent supports.
mkGetBuiltinIntentResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetBuiltinIntentResponse
mkGetBuiltinIntentResponse pResponseStatus_ =
  GetBuiltinIntentResponse'
    { signature = Lude.Nothing,
      slots = Lude.Nothing,
      supportedLocales = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The unique identifier for a built-in intent.
--
-- /Note:/ Consider using 'signature' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbirsSignature :: Lens.Lens' GetBuiltinIntentResponse (Lude.Maybe Lude.Text)
gbirsSignature = Lens.lens (signature :: GetBuiltinIntentResponse -> Lude.Maybe Lude.Text) (\s a -> s {signature = a} :: GetBuiltinIntentResponse)
{-# DEPRECATED gbirsSignature "Use generic-lens or generic-optics with 'signature' instead." #-}

-- | An array of @BuiltinIntentSlot@ objects, one entry for each slot type in the intent.
--
-- /Note:/ Consider using 'slots' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbirsSlots :: Lens.Lens' GetBuiltinIntentResponse (Lude.Maybe [BuiltinIntentSlot])
gbirsSlots = Lens.lens (slots :: GetBuiltinIntentResponse -> Lude.Maybe [BuiltinIntentSlot]) (\s a -> s {slots = a} :: GetBuiltinIntentResponse)
{-# DEPRECATED gbirsSlots "Use generic-lens or generic-optics with 'slots' instead." #-}

-- | A list of locales that the intent supports.
--
-- /Note:/ Consider using 'supportedLocales' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbirsSupportedLocales :: Lens.Lens' GetBuiltinIntentResponse (Lude.Maybe [Locale])
gbirsSupportedLocales = Lens.lens (supportedLocales :: GetBuiltinIntentResponse -> Lude.Maybe [Locale]) (\s a -> s {supportedLocales = a} :: GetBuiltinIntentResponse)
{-# DEPRECATED gbirsSupportedLocales "Use generic-lens or generic-optics with 'supportedLocales' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbirsResponseStatus :: Lens.Lens' GetBuiltinIntentResponse Lude.Int
gbirsResponseStatus = Lens.lens (responseStatus :: GetBuiltinIntentResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetBuiltinIntentResponse)
{-# DEPRECATED gbirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
