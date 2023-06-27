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
-- Module      : Amazonka.ChimeSdkVoice.CreateSipMediaApplicationCall
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an outbound call to a phone number from the phone number
-- specified in the request, and it invokes the endpoint of the specified
-- @sipMediaApplicationId@.
module Amazonka.ChimeSdkVoice.CreateSipMediaApplicationCall
  ( -- * Creating a Request
    CreateSipMediaApplicationCall (..),
    newCreateSipMediaApplicationCall,

    -- * Request Lenses
    createSipMediaApplicationCall_argumentsMap,
    createSipMediaApplicationCall_sipHeaders,
    createSipMediaApplicationCall_fromPhoneNumber,
    createSipMediaApplicationCall_toPhoneNumber,
    createSipMediaApplicationCall_sipMediaApplicationId,

    -- * Destructuring the Response
    CreateSipMediaApplicationCallResponse (..),
    newCreateSipMediaApplicationCallResponse,

    -- * Response Lenses
    createSipMediaApplicationCallResponse_sipMediaApplicationCall,
    createSipMediaApplicationCallResponse_httpStatus,
  )
where

import Amazonka.ChimeSdkVoice.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateSipMediaApplicationCall' smart constructor.
data CreateSipMediaApplicationCall = CreateSipMediaApplicationCall'
  { -- | Context passed to a CreateSipMediaApplication API call. For example, you
    -- could pass key-value pairs such as:
    -- @\"FirstName\": \"John\", \"LastName\": \"Doe\"@
    argumentsMap :: Prelude.Maybe (Prelude.HashMap Prelude.Text (Data.Sensitive Prelude.Text)),
    -- | The SIP headers added to an outbound call leg.
    sipHeaders :: Prelude.Maybe (Prelude.HashMap Prelude.Text (Data.Sensitive Prelude.Text)),
    -- | The phone number that a user calls from. This is a phone number in your
    -- Amazon Chime SDK phone number inventory.
    fromPhoneNumber :: Data.Sensitive Prelude.Text,
    -- | The phone number that the service should call.
    toPhoneNumber :: Data.Sensitive Prelude.Text,
    -- | The ID of the SIP media application.
    sipMediaApplicationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateSipMediaApplicationCall' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'argumentsMap', 'createSipMediaApplicationCall_argumentsMap' - Context passed to a CreateSipMediaApplication API call. For example, you
-- could pass key-value pairs such as:
-- @\"FirstName\": \"John\", \"LastName\": \"Doe\"@
--
-- 'sipHeaders', 'createSipMediaApplicationCall_sipHeaders' - The SIP headers added to an outbound call leg.
--
-- 'fromPhoneNumber', 'createSipMediaApplicationCall_fromPhoneNumber' - The phone number that a user calls from. This is a phone number in your
-- Amazon Chime SDK phone number inventory.
--
-- 'toPhoneNumber', 'createSipMediaApplicationCall_toPhoneNumber' - The phone number that the service should call.
--
-- 'sipMediaApplicationId', 'createSipMediaApplicationCall_sipMediaApplicationId' - The ID of the SIP media application.
newCreateSipMediaApplicationCall ::
  -- | 'fromPhoneNumber'
  Prelude.Text ->
  -- | 'toPhoneNumber'
  Prelude.Text ->
  -- | 'sipMediaApplicationId'
  Prelude.Text ->
  CreateSipMediaApplicationCall
newCreateSipMediaApplicationCall
  pFromPhoneNumber_
  pToPhoneNumber_
  pSipMediaApplicationId_ =
    CreateSipMediaApplicationCall'
      { argumentsMap =
          Prelude.Nothing,
        sipHeaders = Prelude.Nothing,
        fromPhoneNumber =
          Data._Sensitive Lens.# pFromPhoneNumber_,
        toPhoneNumber =
          Data._Sensitive Lens.# pToPhoneNumber_,
        sipMediaApplicationId =
          pSipMediaApplicationId_
      }

-- | Context passed to a CreateSipMediaApplication API call. For example, you
-- could pass key-value pairs such as:
-- @\"FirstName\": \"John\", \"LastName\": \"Doe\"@
createSipMediaApplicationCall_argumentsMap :: Lens.Lens' CreateSipMediaApplicationCall (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createSipMediaApplicationCall_argumentsMap = Lens.lens (\CreateSipMediaApplicationCall' {argumentsMap} -> argumentsMap) (\s@CreateSipMediaApplicationCall' {} a -> s {argumentsMap = a} :: CreateSipMediaApplicationCall) Prelude.. Lens.mapping Lens.coerced

-- | The SIP headers added to an outbound call leg.
createSipMediaApplicationCall_sipHeaders :: Lens.Lens' CreateSipMediaApplicationCall (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createSipMediaApplicationCall_sipHeaders = Lens.lens (\CreateSipMediaApplicationCall' {sipHeaders} -> sipHeaders) (\s@CreateSipMediaApplicationCall' {} a -> s {sipHeaders = a} :: CreateSipMediaApplicationCall) Prelude.. Lens.mapping Lens.coerced

-- | The phone number that a user calls from. This is a phone number in your
-- Amazon Chime SDK phone number inventory.
createSipMediaApplicationCall_fromPhoneNumber :: Lens.Lens' CreateSipMediaApplicationCall Prelude.Text
createSipMediaApplicationCall_fromPhoneNumber = Lens.lens (\CreateSipMediaApplicationCall' {fromPhoneNumber} -> fromPhoneNumber) (\s@CreateSipMediaApplicationCall' {} a -> s {fromPhoneNumber = a} :: CreateSipMediaApplicationCall) Prelude.. Data._Sensitive

-- | The phone number that the service should call.
createSipMediaApplicationCall_toPhoneNumber :: Lens.Lens' CreateSipMediaApplicationCall Prelude.Text
createSipMediaApplicationCall_toPhoneNumber = Lens.lens (\CreateSipMediaApplicationCall' {toPhoneNumber} -> toPhoneNumber) (\s@CreateSipMediaApplicationCall' {} a -> s {toPhoneNumber = a} :: CreateSipMediaApplicationCall) Prelude.. Data._Sensitive

-- | The ID of the SIP media application.
createSipMediaApplicationCall_sipMediaApplicationId :: Lens.Lens' CreateSipMediaApplicationCall Prelude.Text
createSipMediaApplicationCall_sipMediaApplicationId = Lens.lens (\CreateSipMediaApplicationCall' {sipMediaApplicationId} -> sipMediaApplicationId) (\s@CreateSipMediaApplicationCall' {} a -> s {sipMediaApplicationId = a} :: CreateSipMediaApplicationCall)

instance
  Core.AWSRequest
    CreateSipMediaApplicationCall
  where
  type
    AWSResponse CreateSipMediaApplicationCall =
      CreateSipMediaApplicationCallResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateSipMediaApplicationCallResponse'
            Prelude.<$> (x Data..?> "SipMediaApplicationCall")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    CreateSipMediaApplicationCall
  where
  hashWithSalt _salt CreateSipMediaApplicationCall' {..} =
    _salt
      `Prelude.hashWithSalt` argumentsMap
      `Prelude.hashWithSalt` sipHeaders
      `Prelude.hashWithSalt` fromPhoneNumber
      `Prelude.hashWithSalt` toPhoneNumber
      `Prelude.hashWithSalt` sipMediaApplicationId

instance Prelude.NFData CreateSipMediaApplicationCall where
  rnf CreateSipMediaApplicationCall' {..} =
    Prelude.rnf argumentsMap
      `Prelude.seq` Prelude.rnf sipHeaders
      `Prelude.seq` Prelude.rnf fromPhoneNumber
      `Prelude.seq` Prelude.rnf toPhoneNumber
      `Prelude.seq` Prelude.rnf sipMediaApplicationId

instance Data.ToHeaders CreateSipMediaApplicationCall where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON CreateSipMediaApplicationCall where
  toJSON CreateSipMediaApplicationCall' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ArgumentsMap" Data..=) Prelude.<$> argumentsMap,
            ("SipHeaders" Data..=) Prelude.<$> sipHeaders,
            Prelude.Just
              ("FromPhoneNumber" Data..= fromPhoneNumber),
            Prelude.Just
              ("ToPhoneNumber" Data..= toPhoneNumber)
          ]
      )

instance Data.ToPath CreateSipMediaApplicationCall where
  toPath CreateSipMediaApplicationCall' {..} =
    Prelude.mconcat
      [ "/sip-media-applications/",
        Data.toBS sipMediaApplicationId,
        "/calls"
      ]

instance Data.ToQuery CreateSipMediaApplicationCall where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateSipMediaApplicationCallResponse' smart constructor.
data CreateSipMediaApplicationCallResponse = CreateSipMediaApplicationCallResponse'
  { -- | The actual call.
    sipMediaApplicationCall :: Prelude.Maybe SipMediaApplicationCall,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateSipMediaApplicationCallResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sipMediaApplicationCall', 'createSipMediaApplicationCallResponse_sipMediaApplicationCall' - The actual call.
--
-- 'httpStatus', 'createSipMediaApplicationCallResponse_httpStatus' - The response's http status code.
newCreateSipMediaApplicationCallResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateSipMediaApplicationCallResponse
newCreateSipMediaApplicationCallResponse pHttpStatus_ =
  CreateSipMediaApplicationCallResponse'
    { sipMediaApplicationCall =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The actual call.
createSipMediaApplicationCallResponse_sipMediaApplicationCall :: Lens.Lens' CreateSipMediaApplicationCallResponse (Prelude.Maybe SipMediaApplicationCall)
createSipMediaApplicationCallResponse_sipMediaApplicationCall = Lens.lens (\CreateSipMediaApplicationCallResponse' {sipMediaApplicationCall} -> sipMediaApplicationCall) (\s@CreateSipMediaApplicationCallResponse' {} a -> s {sipMediaApplicationCall = a} :: CreateSipMediaApplicationCallResponse)

-- | The response's http status code.
createSipMediaApplicationCallResponse_httpStatus :: Lens.Lens' CreateSipMediaApplicationCallResponse Prelude.Int
createSipMediaApplicationCallResponse_httpStatus = Lens.lens (\CreateSipMediaApplicationCallResponse' {httpStatus} -> httpStatus) (\s@CreateSipMediaApplicationCallResponse' {} a -> s {httpStatus = a} :: CreateSipMediaApplicationCallResponse)

instance
  Prelude.NFData
    CreateSipMediaApplicationCallResponse
  where
  rnf CreateSipMediaApplicationCallResponse' {..} =
    Prelude.rnf sipMediaApplicationCall
      `Prelude.seq` Prelude.rnf httpStatus
