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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetBuiltinIntent' smart constructor.
data GetBuiltinIntent = GetBuiltinIntent'
  { -- | The unique identifier for a built-in intent. To find the signature for
    -- an intent, see
    -- <https://developer.amazon.com/public/solutions/alexa/alexa-skills-kit/docs/built-in-intent-ref/standard-intents Standard Built-in Intents>
    -- in the /Alexa Skills Kit/.
    signature :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  GetBuiltinIntent
newGetBuiltinIntent pSignature_ =
  GetBuiltinIntent' {signature = pSignature_}

-- | The unique identifier for a built-in intent. To find the signature for
-- an intent, see
-- <https://developer.amazon.com/public/solutions/alexa/alexa-skills-kit/docs/built-in-intent-ref/standard-intents Standard Built-in Intents>
-- in the /Alexa Skills Kit/.
getBuiltinIntent_signature :: Lens.Lens' GetBuiltinIntent Prelude.Text
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
            Prelude.<$> (x Core..?> "slots" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "signature")
            Prelude.<*> ( x Core..?> "supportedLocales"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetBuiltinIntent

instance Prelude.NFData GetBuiltinIntent

instance Core.ToHeaders GetBuiltinIntent where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetBuiltinIntent where
  toPath GetBuiltinIntent' {..} =
    Prelude.mconcat
      ["/builtins/intents/", Core.toBS signature]

instance Core.ToQuery GetBuiltinIntent where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetBuiltinIntentResponse' smart constructor.
data GetBuiltinIntentResponse = GetBuiltinIntentResponse'
  { -- | An array of @BuiltinIntentSlot@ objects, one entry for each slot type in
    -- the intent.
    slots :: Prelude.Maybe [BuiltinIntentSlot],
    -- | The unique identifier for a built-in intent.
    signature :: Prelude.Maybe Prelude.Text,
    -- | A list of locales that the intent supports.
    supportedLocales :: Prelude.Maybe [Locale],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  GetBuiltinIntentResponse
newGetBuiltinIntentResponse pHttpStatus_ =
  GetBuiltinIntentResponse'
    { slots = Prelude.Nothing,
      signature = Prelude.Nothing,
      supportedLocales = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of @BuiltinIntentSlot@ objects, one entry for each slot type in
-- the intent.
getBuiltinIntentResponse_slots :: Lens.Lens' GetBuiltinIntentResponse (Prelude.Maybe [BuiltinIntentSlot])
getBuiltinIntentResponse_slots = Lens.lens (\GetBuiltinIntentResponse' {slots} -> slots) (\s@GetBuiltinIntentResponse' {} a -> s {slots = a} :: GetBuiltinIntentResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The unique identifier for a built-in intent.
getBuiltinIntentResponse_signature :: Lens.Lens' GetBuiltinIntentResponse (Prelude.Maybe Prelude.Text)
getBuiltinIntentResponse_signature = Lens.lens (\GetBuiltinIntentResponse' {signature} -> signature) (\s@GetBuiltinIntentResponse' {} a -> s {signature = a} :: GetBuiltinIntentResponse)

-- | A list of locales that the intent supports.
getBuiltinIntentResponse_supportedLocales :: Lens.Lens' GetBuiltinIntentResponse (Prelude.Maybe [Locale])
getBuiltinIntentResponse_supportedLocales = Lens.lens (\GetBuiltinIntentResponse' {supportedLocales} -> supportedLocales) (\s@GetBuiltinIntentResponse' {} a -> s {supportedLocales = a} :: GetBuiltinIntentResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getBuiltinIntentResponse_httpStatus :: Lens.Lens' GetBuiltinIntentResponse Prelude.Int
getBuiltinIntentResponse_httpStatus = Lens.lens (\GetBuiltinIntentResponse' {httpStatus} -> httpStatus) (\s@GetBuiltinIntentResponse' {} a -> s {httpStatus = a} :: GetBuiltinIntentResponse)

instance Prelude.NFData GetBuiltinIntentResponse
