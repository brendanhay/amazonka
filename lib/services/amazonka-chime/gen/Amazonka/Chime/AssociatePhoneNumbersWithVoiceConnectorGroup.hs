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
-- Module      : Amazonka.Chime.AssociatePhoneNumbersWithVoiceConnectorGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates phone numbers with the specified Amazon Chime Voice Connector
-- group.
module Amazonka.Chime.AssociatePhoneNumbersWithVoiceConnectorGroup
  ( -- * Creating a Request
    AssociatePhoneNumbersWithVoiceConnectorGroup (..),
    newAssociatePhoneNumbersWithVoiceConnectorGroup,

    -- * Request Lenses
    associatePhoneNumbersWithVoiceConnectorGroup_forceAssociate,
    associatePhoneNumbersWithVoiceConnectorGroup_voiceConnectorGroupId,
    associatePhoneNumbersWithVoiceConnectorGroup_e164PhoneNumbers,

    -- * Destructuring the Response
    AssociatePhoneNumbersWithVoiceConnectorGroupResponse (..),
    newAssociatePhoneNumbersWithVoiceConnectorGroupResponse,

    -- * Response Lenses
    associatePhoneNumbersWithVoiceConnectorGroupResponse_phoneNumberErrors,
    associatePhoneNumbersWithVoiceConnectorGroupResponse_httpStatus,
  )
where

import Amazonka.Chime.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAssociatePhoneNumbersWithVoiceConnectorGroup' smart constructor.
data AssociatePhoneNumbersWithVoiceConnectorGroup = AssociatePhoneNumbersWithVoiceConnectorGroup'
  { -- | If true, associates the provided phone numbers with the provided Amazon
    -- Chime Voice Connector Group and removes any previously existing
    -- associations. If false, does not associate any phone numbers that have
    -- previously existing associations.
    forceAssociate :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon Chime Voice Connector group ID.
    voiceConnectorGroupId :: Prelude.Text,
    -- | List of phone numbers, in E.164 format.
    e164PhoneNumbers :: [Data.Sensitive Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociatePhoneNumbersWithVoiceConnectorGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'forceAssociate', 'associatePhoneNumbersWithVoiceConnectorGroup_forceAssociate' - If true, associates the provided phone numbers with the provided Amazon
-- Chime Voice Connector Group and removes any previously existing
-- associations. If false, does not associate any phone numbers that have
-- previously existing associations.
--
-- 'voiceConnectorGroupId', 'associatePhoneNumbersWithVoiceConnectorGroup_voiceConnectorGroupId' - The Amazon Chime Voice Connector group ID.
--
-- 'e164PhoneNumbers', 'associatePhoneNumbersWithVoiceConnectorGroup_e164PhoneNumbers' - List of phone numbers, in E.164 format.
newAssociatePhoneNumbersWithVoiceConnectorGroup ::
  -- | 'voiceConnectorGroupId'
  Prelude.Text ->
  AssociatePhoneNumbersWithVoiceConnectorGroup
newAssociatePhoneNumbersWithVoiceConnectorGroup
  pVoiceConnectorGroupId_ =
    AssociatePhoneNumbersWithVoiceConnectorGroup'
      { forceAssociate =
          Prelude.Nothing,
        voiceConnectorGroupId =
          pVoiceConnectorGroupId_,
        e164PhoneNumbers =
          Prelude.mempty
      }

-- | If true, associates the provided phone numbers with the provided Amazon
-- Chime Voice Connector Group and removes any previously existing
-- associations. If false, does not associate any phone numbers that have
-- previously existing associations.
associatePhoneNumbersWithVoiceConnectorGroup_forceAssociate :: Lens.Lens' AssociatePhoneNumbersWithVoiceConnectorGroup (Prelude.Maybe Prelude.Bool)
associatePhoneNumbersWithVoiceConnectorGroup_forceAssociate = Lens.lens (\AssociatePhoneNumbersWithVoiceConnectorGroup' {forceAssociate} -> forceAssociate) (\s@AssociatePhoneNumbersWithVoiceConnectorGroup' {} a -> s {forceAssociate = a} :: AssociatePhoneNumbersWithVoiceConnectorGroup)

-- | The Amazon Chime Voice Connector group ID.
associatePhoneNumbersWithVoiceConnectorGroup_voiceConnectorGroupId :: Lens.Lens' AssociatePhoneNumbersWithVoiceConnectorGroup Prelude.Text
associatePhoneNumbersWithVoiceConnectorGroup_voiceConnectorGroupId = Lens.lens (\AssociatePhoneNumbersWithVoiceConnectorGroup' {voiceConnectorGroupId} -> voiceConnectorGroupId) (\s@AssociatePhoneNumbersWithVoiceConnectorGroup' {} a -> s {voiceConnectorGroupId = a} :: AssociatePhoneNumbersWithVoiceConnectorGroup)

-- | List of phone numbers, in E.164 format.
associatePhoneNumbersWithVoiceConnectorGroup_e164PhoneNumbers :: Lens.Lens' AssociatePhoneNumbersWithVoiceConnectorGroup [Prelude.Text]
associatePhoneNumbersWithVoiceConnectorGroup_e164PhoneNumbers = Lens.lens (\AssociatePhoneNumbersWithVoiceConnectorGroup' {e164PhoneNumbers} -> e164PhoneNumbers) (\s@AssociatePhoneNumbersWithVoiceConnectorGroup' {} a -> s {e164PhoneNumbers = a} :: AssociatePhoneNumbersWithVoiceConnectorGroup) Prelude.. Lens.coerced

instance
  Core.AWSRequest
    AssociatePhoneNumbersWithVoiceConnectorGroup
  where
  type
    AWSResponse
      AssociatePhoneNumbersWithVoiceConnectorGroup =
      AssociatePhoneNumbersWithVoiceConnectorGroupResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          AssociatePhoneNumbersWithVoiceConnectorGroupResponse'
            Prelude.<$> ( x
                            Data..?> "PhoneNumberErrors"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    AssociatePhoneNumbersWithVoiceConnectorGroup
  where
  hashWithSalt
    _salt
    AssociatePhoneNumbersWithVoiceConnectorGroup' {..} =
      _salt
        `Prelude.hashWithSalt` forceAssociate
        `Prelude.hashWithSalt` voiceConnectorGroupId
        `Prelude.hashWithSalt` e164PhoneNumbers

instance
  Prelude.NFData
    AssociatePhoneNumbersWithVoiceConnectorGroup
  where
  rnf AssociatePhoneNumbersWithVoiceConnectorGroup' {..} =
    Prelude.rnf forceAssociate
      `Prelude.seq` Prelude.rnf voiceConnectorGroupId
      `Prelude.seq` Prelude.rnf e164PhoneNumbers

instance
  Data.ToHeaders
    AssociatePhoneNumbersWithVoiceConnectorGroup
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToJSON
    AssociatePhoneNumbersWithVoiceConnectorGroup
  where
  toJSON
    AssociatePhoneNumbersWithVoiceConnectorGroup' {..} =
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
    AssociatePhoneNumbersWithVoiceConnectorGroup
  where
  toPath
    AssociatePhoneNumbersWithVoiceConnectorGroup' {..} =
      Prelude.mconcat
        [ "/voice-connector-groups/",
          Data.toBS voiceConnectorGroupId
        ]

instance
  Data.ToQuery
    AssociatePhoneNumbersWithVoiceConnectorGroup
  where
  toQuery =
    Prelude.const
      ( Prelude.mconcat
          ["operation=associate-phone-numbers"]
      )

-- | /See:/ 'newAssociatePhoneNumbersWithVoiceConnectorGroupResponse' smart constructor.
data AssociatePhoneNumbersWithVoiceConnectorGroupResponse = AssociatePhoneNumbersWithVoiceConnectorGroupResponse'
  { -- | If the action fails for one or more of the phone numbers in the request,
    -- a list of the phone numbers is returned, along with error codes and
    -- error messages.
    phoneNumberErrors :: Prelude.Maybe [PhoneNumberError],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociatePhoneNumbersWithVoiceConnectorGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'phoneNumberErrors', 'associatePhoneNumbersWithVoiceConnectorGroupResponse_phoneNumberErrors' - If the action fails for one or more of the phone numbers in the request,
-- a list of the phone numbers is returned, along with error codes and
-- error messages.
--
-- 'httpStatus', 'associatePhoneNumbersWithVoiceConnectorGroupResponse_httpStatus' - The response's http status code.
newAssociatePhoneNumbersWithVoiceConnectorGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AssociatePhoneNumbersWithVoiceConnectorGroupResponse
newAssociatePhoneNumbersWithVoiceConnectorGroupResponse
  pHttpStatus_ =
    AssociatePhoneNumbersWithVoiceConnectorGroupResponse'
      { phoneNumberErrors =
          Prelude.Nothing,
        httpStatus =
          pHttpStatus_
      }

-- | If the action fails for one or more of the phone numbers in the request,
-- a list of the phone numbers is returned, along with error codes and
-- error messages.
associatePhoneNumbersWithVoiceConnectorGroupResponse_phoneNumberErrors :: Lens.Lens' AssociatePhoneNumbersWithVoiceConnectorGroupResponse (Prelude.Maybe [PhoneNumberError])
associatePhoneNumbersWithVoiceConnectorGroupResponse_phoneNumberErrors = Lens.lens (\AssociatePhoneNumbersWithVoiceConnectorGroupResponse' {phoneNumberErrors} -> phoneNumberErrors) (\s@AssociatePhoneNumbersWithVoiceConnectorGroupResponse' {} a -> s {phoneNumberErrors = a} :: AssociatePhoneNumbersWithVoiceConnectorGroupResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
associatePhoneNumbersWithVoiceConnectorGroupResponse_httpStatus :: Lens.Lens' AssociatePhoneNumbersWithVoiceConnectorGroupResponse Prelude.Int
associatePhoneNumbersWithVoiceConnectorGroupResponse_httpStatus = Lens.lens (\AssociatePhoneNumbersWithVoiceConnectorGroupResponse' {httpStatus} -> httpStatus) (\s@AssociatePhoneNumbersWithVoiceConnectorGroupResponse' {} a -> s {httpStatus = a} :: AssociatePhoneNumbersWithVoiceConnectorGroupResponse)

instance
  Prelude.NFData
    AssociatePhoneNumbersWithVoiceConnectorGroupResponse
  where
  rnf
    AssociatePhoneNumbersWithVoiceConnectorGroupResponse' {..} =
      Prelude.rnf phoneNumberErrors
        `Prelude.seq` Prelude.rnf httpStatus
