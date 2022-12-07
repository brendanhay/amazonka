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
-- Module      : Amazonka.LexModels.GetBuiltinIntent
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a built-in intent.
--
-- This operation requires permission for the @lex:GetBuiltinIntent@
-- action.
module Amazonka.LexModels.GetBuiltinIntent
  ( -- * Creating a Request
    GetBuiltinIntent (..),
    newGetBuiltinIntent,

    -- * Request Lenses
    getBuiltinIntent_signature,

    -- * Destructuring the Response
    GetBuiltinIntentResponse (..),
    newGetBuiltinIntentResponse,

    -- * Response Lenses
    getBuiltinIntentResponse_supportedLocales,
    getBuiltinIntentResponse_signature,
    getBuiltinIntentResponse_slots,
    getBuiltinIntentResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexModels.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetBuiltinIntentResponse'
            Prelude.<$> ( x Data..?> "supportedLocales"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "signature")
            Prelude.<*> (x Data..?> "slots" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetBuiltinIntent where
  hashWithSalt _salt GetBuiltinIntent' {..} =
    _salt `Prelude.hashWithSalt` signature

instance Prelude.NFData GetBuiltinIntent where
  rnf GetBuiltinIntent' {..} = Prelude.rnf signature

instance Data.ToHeaders GetBuiltinIntent where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetBuiltinIntent where
  toPath GetBuiltinIntent' {..} =
    Prelude.mconcat
      ["/builtins/intents/", Data.toBS signature]

instance Data.ToQuery GetBuiltinIntent where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetBuiltinIntentResponse' smart constructor.
data GetBuiltinIntentResponse = GetBuiltinIntentResponse'
  { -- | A list of locales that the intent supports.
    supportedLocales :: Prelude.Maybe [Locale],
    -- | The unique identifier for a built-in intent.
    signature :: Prelude.Maybe Prelude.Text,
    -- | An array of @BuiltinIntentSlot@ objects, one entry for each slot type in
    -- the intent.
    slots :: Prelude.Maybe [BuiltinIntentSlot],
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
-- 'supportedLocales', 'getBuiltinIntentResponse_supportedLocales' - A list of locales that the intent supports.
--
-- 'signature', 'getBuiltinIntentResponse_signature' - The unique identifier for a built-in intent.
--
-- 'slots', 'getBuiltinIntentResponse_slots' - An array of @BuiltinIntentSlot@ objects, one entry for each slot type in
-- the intent.
--
-- 'httpStatus', 'getBuiltinIntentResponse_httpStatus' - The response's http status code.
newGetBuiltinIntentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetBuiltinIntentResponse
newGetBuiltinIntentResponse pHttpStatus_ =
  GetBuiltinIntentResponse'
    { supportedLocales =
        Prelude.Nothing,
      signature = Prelude.Nothing,
      slots = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of locales that the intent supports.
getBuiltinIntentResponse_supportedLocales :: Lens.Lens' GetBuiltinIntentResponse (Prelude.Maybe [Locale])
getBuiltinIntentResponse_supportedLocales = Lens.lens (\GetBuiltinIntentResponse' {supportedLocales} -> supportedLocales) (\s@GetBuiltinIntentResponse' {} a -> s {supportedLocales = a} :: GetBuiltinIntentResponse) Prelude.. Lens.mapping Lens.coerced

-- | The unique identifier for a built-in intent.
getBuiltinIntentResponse_signature :: Lens.Lens' GetBuiltinIntentResponse (Prelude.Maybe Prelude.Text)
getBuiltinIntentResponse_signature = Lens.lens (\GetBuiltinIntentResponse' {signature} -> signature) (\s@GetBuiltinIntentResponse' {} a -> s {signature = a} :: GetBuiltinIntentResponse)

-- | An array of @BuiltinIntentSlot@ objects, one entry for each slot type in
-- the intent.
getBuiltinIntentResponse_slots :: Lens.Lens' GetBuiltinIntentResponse (Prelude.Maybe [BuiltinIntentSlot])
getBuiltinIntentResponse_slots = Lens.lens (\GetBuiltinIntentResponse' {slots} -> slots) (\s@GetBuiltinIntentResponse' {} a -> s {slots = a} :: GetBuiltinIntentResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getBuiltinIntentResponse_httpStatus :: Lens.Lens' GetBuiltinIntentResponse Prelude.Int
getBuiltinIntentResponse_httpStatus = Lens.lens (\GetBuiltinIntentResponse' {httpStatus} -> httpStatus) (\s@GetBuiltinIntentResponse' {} a -> s {httpStatus = a} :: GetBuiltinIntentResponse)

instance Prelude.NFData GetBuiltinIntentResponse where
  rnf GetBuiltinIntentResponse' {..} =
    Prelude.rnf supportedLocales
      `Prelude.seq` Prelude.rnf signature
      `Prelude.seq` Prelude.rnf slots
      `Prelude.seq` Prelude.rnf httpStatus
