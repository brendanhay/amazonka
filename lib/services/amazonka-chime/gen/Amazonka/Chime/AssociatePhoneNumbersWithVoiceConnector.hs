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
-- Module      : Amazonka.Chime.AssociatePhoneNumbersWithVoiceConnector
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates phone numbers with the specified Amazon Chime Voice
-- Connector.
module Amazonka.Chime.AssociatePhoneNumbersWithVoiceConnector
  ( -- * Creating a Request
    AssociatePhoneNumbersWithVoiceConnector (..),
    newAssociatePhoneNumbersWithVoiceConnector,

    -- * Request Lenses
    associatePhoneNumbersWithVoiceConnector_forceAssociate,
    associatePhoneNumbersWithVoiceConnector_voiceConnectorId,
    associatePhoneNumbersWithVoiceConnector_e164PhoneNumbers,

    -- * Destructuring the Response
    AssociatePhoneNumbersWithVoiceConnectorResponse (..),
    newAssociatePhoneNumbersWithVoiceConnectorResponse,

    -- * Response Lenses
    associatePhoneNumbersWithVoiceConnectorResponse_phoneNumberErrors,
    associatePhoneNumbersWithVoiceConnectorResponse_httpStatus,
  )
where

import Amazonka.Chime.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAssociatePhoneNumbersWithVoiceConnector' smart constructor.
data AssociatePhoneNumbersWithVoiceConnector = AssociatePhoneNumbersWithVoiceConnector'
  { -- | If true, associates the provided phone numbers with the provided Amazon
    -- Chime Voice Connector and removes any previously existing associations.
    -- If false, does not associate any phone numbers that have previously
    -- existing associations.
    forceAssociate :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon Chime Voice Connector ID.
    voiceConnectorId :: Prelude.Text,
    -- | List of phone numbers, in E.164 format.
    e164PhoneNumbers :: [Data.Sensitive Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociatePhoneNumbersWithVoiceConnector' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'forceAssociate', 'associatePhoneNumbersWithVoiceConnector_forceAssociate' - If true, associates the provided phone numbers with the provided Amazon
-- Chime Voice Connector and removes any previously existing associations.
-- If false, does not associate any phone numbers that have previously
-- existing associations.
--
-- 'voiceConnectorId', 'associatePhoneNumbersWithVoiceConnector_voiceConnectorId' - The Amazon Chime Voice Connector ID.
--
-- 'e164PhoneNumbers', 'associatePhoneNumbersWithVoiceConnector_e164PhoneNumbers' - List of phone numbers, in E.164 format.
newAssociatePhoneNumbersWithVoiceConnector ::
  -- | 'voiceConnectorId'
  Prelude.Text ->
  AssociatePhoneNumbersWithVoiceConnector
newAssociatePhoneNumbersWithVoiceConnector
  pVoiceConnectorId_ =
    AssociatePhoneNumbersWithVoiceConnector'
      { forceAssociate =
          Prelude.Nothing,
        voiceConnectorId =
          pVoiceConnectorId_,
        e164PhoneNumbers = Prelude.mempty
      }

-- | If true, associates the provided phone numbers with the provided Amazon
-- Chime Voice Connector and removes any previously existing associations.
-- If false, does not associate any phone numbers that have previously
-- existing associations.
associatePhoneNumbersWithVoiceConnector_forceAssociate :: Lens.Lens' AssociatePhoneNumbersWithVoiceConnector (Prelude.Maybe Prelude.Bool)
associatePhoneNumbersWithVoiceConnector_forceAssociate = Lens.lens (\AssociatePhoneNumbersWithVoiceConnector' {forceAssociate} -> forceAssociate) (\s@AssociatePhoneNumbersWithVoiceConnector' {} a -> s {forceAssociate = a} :: AssociatePhoneNumbersWithVoiceConnector)

-- | The Amazon Chime Voice Connector ID.
associatePhoneNumbersWithVoiceConnector_voiceConnectorId :: Lens.Lens' AssociatePhoneNumbersWithVoiceConnector Prelude.Text
associatePhoneNumbersWithVoiceConnector_voiceConnectorId = Lens.lens (\AssociatePhoneNumbersWithVoiceConnector' {voiceConnectorId} -> voiceConnectorId) (\s@AssociatePhoneNumbersWithVoiceConnector' {} a -> s {voiceConnectorId = a} :: AssociatePhoneNumbersWithVoiceConnector)

-- | List of phone numbers, in E.164 format.
associatePhoneNumbersWithVoiceConnector_e164PhoneNumbers :: Lens.Lens' AssociatePhoneNumbersWithVoiceConnector [Prelude.Text]
associatePhoneNumbersWithVoiceConnector_e164PhoneNumbers = Lens.lens (\AssociatePhoneNumbersWithVoiceConnector' {e164PhoneNumbers} -> e164PhoneNumbers) (\s@AssociatePhoneNumbersWithVoiceConnector' {} a -> s {e164PhoneNumbers = a} :: AssociatePhoneNumbersWithVoiceConnector) Prelude.. Lens.coerced

instance
  Core.AWSRequest
    AssociatePhoneNumbersWithVoiceConnector
  where
  type
    AWSResponse
      AssociatePhoneNumbersWithVoiceConnector =
      AssociatePhoneNumbersWithVoiceConnectorResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          AssociatePhoneNumbersWithVoiceConnectorResponse'
            Prelude.<$> ( x
                            Data..?> "PhoneNumberErrors"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    AssociatePhoneNumbersWithVoiceConnector
  where
  hashWithSalt
    _salt
    AssociatePhoneNumbersWithVoiceConnector' {..} =
      _salt
        `Prelude.hashWithSalt` forceAssociate
        `Prelude.hashWithSalt` voiceConnectorId
        `Prelude.hashWithSalt` e164PhoneNumbers

instance
  Prelude.NFData
    AssociatePhoneNumbersWithVoiceConnector
  where
  rnf AssociatePhoneNumbersWithVoiceConnector' {..} =
    Prelude.rnf forceAssociate
      `Prelude.seq` Prelude.rnf voiceConnectorId
      `Prelude.seq` Prelude.rnf e164PhoneNumbers

instance
  Data.ToHeaders
    AssociatePhoneNumbersWithVoiceConnector
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToJSON
    AssociatePhoneNumbersWithVoiceConnector
  where
  toJSON AssociatePhoneNumbersWithVoiceConnector' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ForceAssociate" Data..=)
              Prelude.<$> forceAssociate,
            Prelude.Just
              ("E164PhoneNumbers" Data..= e164PhoneNumbers)
          ]
      )

instance
  Data.ToPath
    AssociatePhoneNumbersWithVoiceConnector
  where
  toPath AssociatePhoneNumbersWithVoiceConnector' {..} =
    Prelude.mconcat
      ["/voice-connectors/", Data.toBS voiceConnectorId]

instance
  Data.ToQuery
    AssociatePhoneNumbersWithVoiceConnector
  where
  toQuery =
    Prelude.const
      ( Prelude.mconcat
          ["operation=associate-phone-numbers"]
      )

-- | /See:/ 'newAssociatePhoneNumbersWithVoiceConnectorResponse' smart constructor.
data AssociatePhoneNumbersWithVoiceConnectorResponse = AssociatePhoneNumbersWithVoiceConnectorResponse'
  { -- | If the action fails for one or more of the phone numbers in the request,
    -- a list of the phone numbers is returned, along with error codes and
    -- error messages.
    phoneNumberErrors :: Prelude.Maybe [PhoneNumberError],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociatePhoneNumbersWithVoiceConnectorResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'phoneNumberErrors', 'associatePhoneNumbersWithVoiceConnectorResponse_phoneNumberErrors' - If the action fails for one or more of the phone numbers in the request,
-- a list of the phone numbers is returned, along with error codes and
-- error messages.
--
-- 'httpStatus', 'associatePhoneNumbersWithVoiceConnectorResponse_httpStatus' - The response's http status code.
newAssociatePhoneNumbersWithVoiceConnectorResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AssociatePhoneNumbersWithVoiceConnectorResponse
newAssociatePhoneNumbersWithVoiceConnectorResponse
  pHttpStatus_ =
    AssociatePhoneNumbersWithVoiceConnectorResponse'
      { phoneNumberErrors =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | If the action fails for one or more of the phone numbers in the request,
-- a list of the phone numbers is returned, along with error codes and
-- error messages.
associatePhoneNumbersWithVoiceConnectorResponse_phoneNumberErrors :: Lens.Lens' AssociatePhoneNumbersWithVoiceConnectorResponse (Prelude.Maybe [PhoneNumberError])
associatePhoneNumbersWithVoiceConnectorResponse_phoneNumberErrors = Lens.lens (\AssociatePhoneNumbersWithVoiceConnectorResponse' {phoneNumberErrors} -> phoneNumberErrors) (\s@AssociatePhoneNumbersWithVoiceConnectorResponse' {} a -> s {phoneNumberErrors = a} :: AssociatePhoneNumbersWithVoiceConnectorResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
associatePhoneNumbersWithVoiceConnectorResponse_httpStatus :: Lens.Lens' AssociatePhoneNumbersWithVoiceConnectorResponse Prelude.Int
associatePhoneNumbersWithVoiceConnectorResponse_httpStatus = Lens.lens (\AssociatePhoneNumbersWithVoiceConnectorResponse' {httpStatus} -> httpStatus) (\s@AssociatePhoneNumbersWithVoiceConnectorResponse' {} a -> s {httpStatus = a} :: AssociatePhoneNumbersWithVoiceConnectorResponse)

instance
  Prelude.NFData
    AssociatePhoneNumbersWithVoiceConnectorResponse
  where
  rnf
    AssociatePhoneNumbersWithVoiceConnectorResponse' {..} =
      Prelude.rnf phoneNumberErrors
        `Prelude.seq` Prelude.rnf httpStatus
