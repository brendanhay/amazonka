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
-- Module      : Amazonka.Chime.DisassociatePhoneNumbersFromVoiceConnectorGroup
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates the specified phone numbers from the specified Amazon
-- Chime Voice Connector group.
module Amazonka.Chime.DisassociatePhoneNumbersFromVoiceConnectorGroup
  ( -- * Creating a Request
    DisassociatePhoneNumbersFromVoiceConnectorGroup (..),
    newDisassociatePhoneNumbersFromVoiceConnectorGroup,

    -- * Request Lenses
    disassociatePhoneNumbersFromVoiceConnectorGroup_voiceConnectorGroupId,
    disassociatePhoneNumbersFromVoiceConnectorGroup_e164PhoneNumbers,

    -- * Destructuring the Response
    DisassociatePhoneNumbersFromVoiceConnectorGroupResponse (..),
    newDisassociatePhoneNumbersFromVoiceConnectorGroupResponse,

    -- * Response Lenses
    disassociatePhoneNumbersFromVoiceConnectorGroupResponse_phoneNumberErrors,
    disassociatePhoneNumbersFromVoiceConnectorGroupResponse_httpStatus,
  )
where

import Amazonka.Chime.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDisassociatePhoneNumbersFromVoiceConnectorGroup' smart constructor.
data DisassociatePhoneNumbersFromVoiceConnectorGroup = DisassociatePhoneNumbersFromVoiceConnectorGroup'
  { -- | The Amazon Chime Voice Connector group ID.
    voiceConnectorGroupId :: Prelude.Text,
    -- | List of phone numbers, in E.164 format.
    e164PhoneNumbers :: [Data.Sensitive Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociatePhoneNumbersFromVoiceConnectorGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'voiceConnectorGroupId', 'disassociatePhoneNumbersFromVoiceConnectorGroup_voiceConnectorGroupId' - The Amazon Chime Voice Connector group ID.
--
-- 'e164PhoneNumbers', 'disassociatePhoneNumbersFromVoiceConnectorGroup_e164PhoneNumbers' - List of phone numbers, in E.164 format.
newDisassociatePhoneNumbersFromVoiceConnectorGroup ::
  -- | 'voiceConnectorGroupId'
  Prelude.Text ->
  DisassociatePhoneNumbersFromVoiceConnectorGroup
newDisassociatePhoneNumbersFromVoiceConnectorGroup
  pVoiceConnectorGroupId_ =
    DisassociatePhoneNumbersFromVoiceConnectorGroup'
      { voiceConnectorGroupId =
          pVoiceConnectorGroupId_,
        e164PhoneNumbers =
          Prelude.mempty
      }

-- | The Amazon Chime Voice Connector group ID.
disassociatePhoneNumbersFromVoiceConnectorGroup_voiceConnectorGroupId :: Lens.Lens' DisassociatePhoneNumbersFromVoiceConnectorGroup Prelude.Text
disassociatePhoneNumbersFromVoiceConnectorGroup_voiceConnectorGroupId = Lens.lens (\DisassociatePhoneNumbersFromVoiceConnectorGroup' {voiceConnectorGroupId} -> voiceConnectorGroupId) (\s@DisassociatePhoneNumbersFromVoiceConnectorGroup' {} a -> s {voiceConnectorGroupId = a} :: DisassociatePhoneNumbersFromVoiceConnectorGroup)

-- | List of phone numbers, in E.164 format.
disassociatePhoneNumbersFromVoiceConnectorGroup_e164PhoneNumbers :: Lens.Lens' DisassociatePhoneNumbersFromVoiceConnectorGroup [Prelude.Text]
disassociatePhoneNumbersFromVoiceConnectorGroup_e164PhoneNumbers = Lens.lens (\DisassociatePhoneNumbersFromVoiceConnectorGroup' {e164PhoneNumbers} -> e164PhoneNumbers) (\s@DisassociatePhoneNumbersFromVoiceConnectorGroup' {} a -> s {e164PhoneNumbers = a} :: DisassociatePhoneNumbersFromVoiceConnectorGroup) Prelude.. Lens.coerced

instance
  Core.AWSRequest
    DisassociatePhoneNumbersFromVoiceConnectorGroup
  where
  type
    AWSResponse
      DisassociatePhoneNumbersFromVoiceConnectorGroup =
      DisassociatePhoneNumbersFromVoiceConnectorGroupResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DisassociatePhoneNumbersFromVoiceConnectorGroupResponse'
            Prelude.<$> ( x Data..?> "PhoneNumberErrors"
                            Core..!@ Prelude.mempty
                        )
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DisassociatePhoneNumbersFromVoiceConnectorGroup
  where
  hashWithSalt
    _salt
    DisassociatePhoneNumbersFromVoiceConnectorGroup' {..} =
      _salt `Prelude.hashWithSalt` voiceConnectorGroupId
        `Prelude.hashWithSalt` e164PhoneNumbers

instance
  Prelude.NFData
    DisassociatePhoneNumbersFromVoiceConnectorGroup
  where
  rnf
    DisassociatePhoneNumbersFromVoiceConnectorGroup' {..} =
      Prelude.rnf voiceConnectorGroupId
        `Prelude.seq` Prelude.rnf e164PhoneNumbers

instance
  Data.ToHeaders
    DisassociatePhoneNumbersFromVoiceConnectorGroup
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToJSON
    DisassociatePhoneNumbersFromVoiceConnectorGroup
  where
  toJSON
    DisassociatePhoneNumbersFromVoiceConnectorGroup' {..} =
      Data.object
        ( Prelude.catMaybes
            [ Prelude.Just
                ("E164PhoneNumbers" Data..= e164PhoneNumbers)
            ]
        )

instance
  Data.ToPath
    DisassociatePhoneNumbersFromVoiceConnectorGroup
  where
  toPath
    DisassociatePhoneNumbersFromVoiceConnectorGroup' {..} =
      Prelude.mconcat
        [ "/voice-connector-groups/",
          Data.toBS voiceConnectorGroupId
        ]

instance
  Data.ToQuery
    DisassociatePhoneNumbersFromVoiceConnectorGroup
  where
  toQuery =
    Prelude.const
      ( Prelude.mconcat
          ["operation=disassociate-phone-numbers"]
      )

-- | /See:/ 'newDisassociatePhoneNumbersFromVoiceConnectorGroupResponse' smart constructor.
data DisassociatePhoneNumbersFromVoiceConnectorGroupResponse = DisassociatePhoneNumbersFromVoiceConnectorGroupResponse'
  { -- | If the action fails for one or more of the phone numbers in the request,
    -- a list of the phone numbers is returned, along with error codes and
    -- error messages.
    phoneNumberErrors :: Prelude.Maybe [PhoneNumberError],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociatePhoneNumbersFromVoiceConnectorGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'phoneNumberErrors', 'disassociatePhoneNumbersFromVoiceConnectorGroupResponse_phoneNumberErrors' - If the action fails for one or more of the phone numbers in the request,
-- a list of the phone numbers is returned, along with error codes and
-- error messages.
--
-- 'httpStatus', 'disassociatePhoneNumbersFromVoiceConnectorGroupResponse_httpStatus' - The response's http status code.
newDisassociatePhoneNumbersFromVoiceConnectorGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DisassociatePhoneNumbersFromVoiceConnectorGroupResponse
newDisassociatePhoneNumbersFromVoiceConnectorGroupResponse
  pHttpStatus_ =
    DisassociatePhoneNumbersFromVoiceConnectorGroupResponse'
      { phoneNumberErrors =
          Prelude.Nothing,
        httpStatus =
          pHttpStatus_
      }

-- | If the action fails for one or more of the phone numbers in the request,
-- a list of the phone numbers is returned, along with error codes and
-- error messages.
disassociatePhoneNumbersFromVoiceConnectorGroupResponse_phoneNumberErrors :: Lens.Lens' DisassociatePhoneNumbersFromVoiceConnectorGroupResponse (Prelude.Maybe [PhoneNumberError])
disassociatePhoneNumbersFromVoiceConnectorGroupResponse_phoneNumberErrors = Lens.lens (\DisassociatePhoneNumbersFromVoiceConnectorGroupResponse' {phoneNumberErrors} -> phoneNumberErrors) (\s@DisassociatePhoneNumbersFromVoiceConnectorGroupResponse' {} a -> s {phoneNumberErrors = a} :: DisassociatePhoneNumbersFromVoiceConnectorGroupResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
disassociatePhoneNumbersFromVoiceConnectorGroupResponse_httpStatus :: Lens.Lens' DisassociatePhoneNumbersFromVoiceConnectorGroupResponse Prelude.Int
disassociatePhoneNumbersFromVoiceConnectorGroupResponse_httpStatus = Lens.lens (\DisassociatePhoneNumbersFromVoiceConnectorGroupResponse' {httpStatus} -> httpStatus) (\s@DisassociatePhoneNumbersFromVoiceConnectorGroupResponse' {} a -> s {httpStatus = a} :: DisassociatePhoneNumbersFromVoiceConnectorGroupResponse)

instance
  Prelude.NFData
    DisassociatePhoneNumbersFromVoiceConnectorGroupResponse
  where
  rnf
    DisassociatePhoneNumbersFromVoiceConnectorGroupResponse' {..} =
      Prelude.rnf phoneNumberErrors
        `Prelude.seq` Prelude.rnf httpStatus
