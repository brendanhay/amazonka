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
-- Module      : Amazonka.Chime.DisassociatePhoneNumbersFromVoiceConnector
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates the specified phone numbers from the specified Amazon
-- Chime Voice Connector.
module Amazonka.Chime.DisassociatePhoneNumbersFromVoiceConnector
  ( -- * Creating a Request
    DisassociatePhoneNumbersFromVoiceConnector (..),
    newDisassociatePhoneNumbersFromVoiceConnector,

    -- * Request Lenses
    disassociatePhoneNumbersFromVoiceConnector_voiceConnectorId,
    disassociatePhoneNumbersFromVoiceConnector_e164PhoneNumbers,

    -- * Destructuring the Response
    DisassociatePhoneNumbersFromVoiceConnectorResponse (..),
    newDisassociatePhoneNumbersFromVoiceConnectorResponse,

    -- * Response Lenses
    disassociatePhoneNumbersFromVoiceConnectorResponse_phoneNumberErrors,
    disassociatePhoneNumbersFromVoiceConnectorResponse_httpStatus,
  )
where

import Amazonka.Chime.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDisassociatePhoneNumbersFromVoiceConnector' smart constructor.
data DisassociatePhoneNumbersFromVoiceConnector = DisassociatePhoneNumbersFromVoiceConnector'
  { -- | The Amazon Chime Voice Connector ID.
    voiceConnectorId :: Prelude.Text,
    -- | List of phone numbers, in E.164 format.
    e164PhoneNumbers :: [Data.Sensitive Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociatePhoneNumbersFromVoiceConnector' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'voiceConnectorId', 'disassociatePhoneNumbersFromVoiceConnector_voiceConnectorId' - The Amazon Chime Voice Connector ID.
--
-- 'e164PhoneNumbers', 'disassociatePhoneNumbersFromVoiceConnector_e164PhoneNumbers' - List of phone numbers, in E.164 format.
newDisassociatePhoneNumbersFromVoiceConnector ::
  -- | 'voiceConnectorId'
  Prelude.Text ->
  DisassociatePhoneNumbersFromVoiceConnector
newDisassociatePhoneNumbersFromVoiceConnector
  pVoiceConnectorId_ =
    DisassociatePhoneNumbersFromVoiceConnector'
      { voiceConnectorId =
          pVoiceConnectorId_,
        e164PhoneNumbers =
          Prelude.mempty
      }

-- | The Amazon Chime Voice Connector ID.
disassociatePhoneNumbersFromVoiceConnector_voiceConnectorId :: Lens.Lens' DisassociatePhoneNumbersFromVoiceConnector Prelude.Text
disassociatePhoneNumbersFromVoiceConnector_voiceConnectorId = Lens.lens (\DisassociatePhoneNumbersFromVoiceConnector' {voiceConnectorId} -> voiceConnectorId) (\s@DisassociatePhoneNumbersFromVoiceConnector' {} a -> s {voiceConnectorId = a} :: DisassociatePhoneNumbersFromVoiceConnector)

-- | List of phone numbers, in E.164 format.
disassociatePhoneNumbersFromVoiceConnector_e164PhoneNumbers :: Lens.Lens' DisassociatePhoneNumbersFromVoiceConnector [Prelude.Text]
disassociatePhoneNumbersFromVoiceConnector_e164PhoneNumbers = Lens.lens (\DisassociatePhoneNumbersFromVoiceConnector' {e164PhoneNumbers} -> e164PhoneNumbers) (\s@DisassociatePhoneNumbersFromVoiceConnector' {} a -> s {e164PhoneNumbers = a} :: DisassociatePhoneNumbersFromVoiceConnector) Prelude.. Lens.coerced

instance
  Core.AWSRequest
    DisassociatePhoneNumbersFromVoiceConnector
  where
  type
    AWSResponse
      DisassociatePhoneNumbersFromVoiceConnector =
      DisassociatePhoneNumbersFromVoiceConnectorResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DisassociatePhoneNumbersFromVoiceConnectorResponse'
            Prelude.<$> ( x
                            Data..?> "PhoneNumberErrors"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DisassociatePhoneNumbersFromVoiceConnector
  where
  hashWithSalt
    _salt
    DisassociatePhoneNumbersFromVoiceConnector' {..} =
      _salt
        `Prelude.hashWithSalt` voiceConnectorId
        `Prelude.hashWithSalt` e164PhoneNumbers

instance
  Prelude.NFData
    DisassociatePhoneNumbersFromVoiceConnector
  where
  rnf DisassociatePhoneNumbersFromVoiceConnector' {..} =
    Prelude.rnf voiceConnectorId
      `Prelude.seq` Prelude.rnf e164PhoneNumbers

instance
  Data.ToHeaders
    DisassociatePhoneNumbersFromVoiceConnector
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToJSON
    DisassociatePhoneNumbersFromVoiceConnector
  where
  toJSON
    DisassociatePhoneNumbersFromVoiceConnector' {..} =
      Data.object
        ( Prelude.catMaybes
            [ Prelude.Just
                ("E164PhoneNumbers" Data..= e164PhoneNumbers)
            ]
        )

instance
  Data.ToPath
    DisassociatePhoneNumbersFromVoiceConnector
  where
  toPath
    DisassociatePhoneNumbersFromVoiceConnector' {..} =
      Prelude.mconcat
        ["/voice-connectors/", Data.toBS voiceConnectorId]

instance
  Data.ToQuery
    DisassociatePhoneNumbersFromVoiceConnector
  where
  toQuery =
    Prelude.const
      ( Prelude.mconcat
          ["operation=disassociate-phone-numbers"]
      )

-- | /See:/ 'newDisassociatePhoneNumbersFromVoiceConnectorResponse' smart constructor.
data DisassociatePhoneNumbersFromVoiceConnectorResponse = DisassociatePhoneNumbersFromVoiceConnectorResponse'
  { -- | If the action fails for one or more of the phone numbers in the request,
    -- a list of the phone numbers is returned, along with error codes and
    -- error messages.
    phoneNumberErrors :: Prelude.Maybe [PhoneNumberError],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociatePhoneNumbersFromVoiceConnectorResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'phoneNumberErrors', 'disassociatePhoneNumbersFromVoiceConnectorResponse_phoneNumberErrors' - If the action fails for one or more of the phone numbers in the request,
-- a list of the phone numbers is returned, along with error codes and
-- error messages.
--
-- 'httpStatus', 'disassociatePhoneNumbersFromVoiceConnectorResponse_httpStatus' - The response's http status code.
newDisassociatePhoneNumbersFromVoiceConnectorResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DisassociatePhoneNumbersFromVoiceConnectorResponse
newDisassociatePhoneNumbersFromVoiceConnectorResponse
  pHttpStatus_ =
    DisassociatePhoneNumbersFromVoiceConnectorResponse'
      { phoneNumberErrors =
          Prelude.Nothing,
        httpStatus =
          pHttpStatus_
      }

-- | If the action fails for one or more of the phone numbers in the request,
-- a list of the phone numbers is returned, along with error codes and
-- error messages.
disassociatePhoneNumbersFromVoiceConnectorResponse_phoneNumberErrors :: Lens.Lens' DisassociatePhoneNumbersFromVoiceConnectorResponse (Prelude.Maybe [PhoneNumberError])
disassociatePhoneNumbersFromVoiceConnectorResponse_phoneNumberErrors = Lens.lens (\DisassociatePhoneNumbersFromVoiceConnectorResponse' {phoneNumberErrors} -> phoneNumberErrors) (\s@DisassociatePhoneNumbersFromVoiceConnectorResponse' {} a -> s {phoneNumberErrors = a} :: DisassociatePhoneNumbersFromVoiceConnectorResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
disassociatePhoneNumbersFromVoiceConnectorResponse_httpStatus :: Lens.Lens' DisassociatePhoneNumbersFromVoiceConnectorResponse Prelude.Int
disassociatePhoneNumbersFromVoiceConnectorResponse_httpStatus = Lens.lens (\DisassociatePhoneNumbersFromVoiceConnectorResponse' {httpStatus} -> httpStatus) (\s@DisassociatePhoneNumbersFromVoiceConnectorResponse' {} a -> s {httpStatus = a} :: DisassociatePhoneNumbersFromVoiceConnectorResponse)

instance
  Prelude.NFData
    DisassociatePhoneNumbersFromVoiceConnectorResponse
  where
  rnf
    DisassociatePhoneNumbersFromVoiceConnectorResponse' {..} =
      Prelude.rnf phoneNumberErrors
        `Prelude.seq` Prelude.rnf httpStatus
