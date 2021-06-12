{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.GetBuiltinIntent
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a built-in intent.
--
-- This operation requires permission for the @lex:GetBuiltinIntent@
-- action.
module Network.AWS.LexModels.GetBuiltinIntent
  ( -- * Creating a Request
    GetBuiltinIntent (..),
    newGetBuiltinIntent,

    -- * Request Lenses
    getBuiltinIntent_signature,

    -- * Destructuring the Response
    GetBuiltinIntentResponse (..),
    newGetBuiltinIntentResponse,

    -- * Response Lenses
    getBuiltinIntentResponse_slots,
    getBuiltinIntentResponse_signature,
    getBuiltinIntentResponse_supportedLocales,
    getBuiltinIntentResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.LexModels.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetBuiltinIntent' smart constructor.
data GetBuiltinIntent = GetBuiltinIntent'
  { -- | The unique identifier for a built-in intent. To find the signature for
    -- an intent, see
    -- <https://developer.amazon.com/public/solutions/alexa/alexa-skills-kit/docs/built-in-intent-ref/standard-intents Standard Built-in Intents>
    -- in the /Alexa Skills Kit/.
    signature :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetBuiltinIntent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'signature', 'getBuiltinIntent_signature' - The unique identifier for a built-in intent. To find the signature for
-- an intent, see
-- <https://developer.amazon.com/public/solutions/alexa/alexa-skills-kit/docs/built-in-intent-ref/standard-intents Standard Built-in Intents>
-- in the /Alexa Skills Kit/.
newGetBuiltinIntent ::
  -- | 'signature'
  Core.Text ->
  GetBuiltinIntent
newGetBuiltinIntent pSignature_ =
  GetBuiltinIntent' {signature = pSignature_}

-- | The unique identifier for a built-in intent. To find the signature for
-- an intent, see
-- <https://developer.amazon.com/public/solutions/alexa/alexa-skills-kit/docs/built-in-intent-ref/standard-intents Standard Built-in Intents>
-- in the /Alexa Skills Kit/.
getBuiltinIntent_signature :: Lens.Lens' GetBuiltinIntent Core.Text
getBuiltinIntent_signature = Lens.lens (\GetBuiltinIntent' {signature} -> signature) (\s@GetBuiltinIntent' {} a -> s {signature = a} :: GetBuiltinIntent)

instance Core.AWSRequest GetBuiltinIntent where
  type
    AWSResponse GetBuiltinIntent =
      GetBuiltinIntentResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetBuiltinIntentResponse'
            Core.<$> (x Core..?> "slots" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "signature")
            Core.<*> (x Core..?> "supportedLocales" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetBuiltinIntent

instance Core.NFData GetBuiltinIntent

instance Core.ToHeaders GetBuiltinIntent where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath GetBuiltinIntent where
  toPath GetBuiltinIntent' {..} =
    Core.mconcat
      ["/builtins/intents/", Core.toBS signature]

instance Core.ToQuery GetBuiltinIntent where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetBuiltinIntentResponse' smart constructor.
data GetBuiltinIntentResponse = GetBuiltinIntentResponse'
  { -- | An array of @BuiltinIntentSlot@ objects, one entry for each slot type in
    -- the intent.
    slots :: Core.Maybe [BuiltinIntentSlot],
    -- | The unique identifier for a built-in intent.
    signature :: Core.Maybe Core.Text,
    -- | A list of locales that the intent supports.
    supportedLocales :: Core.Maybe [Locale],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetBuiltinIntentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'slots', 'getBuiltinIntentResponse_slots' - An array of @BuiltinIntentSlot@ objects, one entry for each slot type in
-- the intent.
--
-- 'signature', 'getBuiltinIntentResponse_signature' - The unique identifier for a built-in intent.
--
-- 'supportedLocales', 'getBuiltinIntentResponse_supportedLocales' - A list of locales that the intent supports.
--
-- 'httpStatus', 'getBuiltinIntentResponse_httpStatus' - The response's http status code.
newGetBuiltinIntentResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetBuiltinIntentResponse
newGetBuiltinIntentResponse pHttpStatus_ =
  GetBuiltinIntentResponse'
    { slots = Core.Nothing,
      signature = Core.Nothing,
      supportedLocales = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of @BuiltinIntentSlot@ objects, one entry for each slot type in
-- the intent.
getBuiltinIntentResponse_slots :: Lens.Lens' GetBuiltinIntentResponse (Core.Maybe [BuiltinIntentSlot])
getBuiltinIntentResponse_slots = Lens.lens (\GetBuiltinIntentResponse' {slots} -> slots) (\s@GetBuiltinIntentResponse' {} a -> s {slots = a} :: GetBuiltinIntentResponse) Core.. Lens.mapping Lens._Coerce

-- | The unique identifier for a built-in intent.
getBuiltinIntentResponse_signature :: Lens.Lens' GetBuiltinIntentResponse (Core.Maybe Core.Text)
getBuiltinIntentResponse_signature = Lens.lens (\GetBuiltinIntentResponse' {signature} -> signature) (\s@GetBuiltinIntentResponse' {} a -> s {signature = a} :: GetBuiltinIntentResponse)

-- | A list of locales that the intent supports.
getBuiltinIntentResponse_supportedLocales :: Lens.Lens' GetBuiltinIntentResponse (Core.Maybe [Locale])
getBuiltinIntentResponse_supportedLocales = Lens.lens (\GetBuiltinIntentResponse' {supportedLocales} -> supportedLocales) (\s@GetBuiltinIntentResponse' {} a -> s {supportedLocales = a} :: GetBuiltinIntentResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getBuiltinIntentResponse_httpStatus :: Lens.Lens' GetBuiltinIntentResponse Core.Int
getBuiltinIntentResponse_httpStatus = Lens.lens (\GetBuiltinIntentResponse' {httpStatus} -> httpStatus) (\s@GetBuiltinIntentResponse' {} a -> s {httpStatus = a} :: GetBuiltinIntentResponse)

instance Core.NFData GetBuiltinIntentResponse
