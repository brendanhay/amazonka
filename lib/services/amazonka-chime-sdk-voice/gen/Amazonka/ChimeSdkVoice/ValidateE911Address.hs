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
-- Module      : Amazonka.ChimeSdkVoice.ValidateE911Address
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- -- | Undocumented operation.
module Amazonka.ChimeSdkVoice.ValidateE911Address
  ( -- * Creating a Request
    ValidateE911Address (..),
    newValidateE911Address,

    -- * Request Lenses
    validateE911Address_awsAccountId,
    validateE911Address_streetNumber,
    validateE911Address_streetInfo,
    validateE911Address_city,
    validateE911Address_state,
    validateE911Address_country,
    validateE911Address_postalCode,

    -- * Destructuring the Response
    ValidateE911AddressResponse (..),
    newValidateE911AddressResponse,

    -- * Response Lenses
    validateE911AddressResponse_address,
    validateE911AddressResponse_addressExternalId,
    validateE911AddressResponse_candidateAddressList,
    validateE911AddressResponse_validationResult,
    validateE911AddressResponse_httpStatus,
  )
where

import Amazonka.ChimeSdkVoice.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newValidateE911Address' smart constructor.
data ValidateE911Address = ValidateE911Address'
  { awsAccountId :: Prelude.Text,
    streetNumber :: Data.Sensitive Prelude.Text,
    streetInfo :: Data.Sensitive Prelude.Text,
    city :: Data.Sensitive Prelude.Text,
    state :: Data.Sensitive Prelude.Text,
    country :: Data.Sensitive Prelude.Text,
    postalCode :: Data.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ValidateE911Address' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'awsAccountId', 'validateE911Address_awsAccountId' - Undocumented member.
--
-- 'streetNumber', 'validateE911Address_streetNumber' - Undocumented member.
--
-- 'streetInfo', 'validateE911Address_streetInfo' - Undocumented member.
--
-- 'city', 'validateE911Address_city' - Undocumented member.
--
-- 'state', 'validateE911Address_state' - Undocumented member.
--
-- 'country', 'validateE911Address_country' - Undocumented member.
--
-- 'postalCode', 'validateE911Address_postalCode' - Undocumented member.
newValidateE911Address ::
  -- | 'awsAccountId'
  Prelude.Text ->
  -- | 'streetNumber'
  Prelude.Text ->
  -- | 'streetInfo'
  Prelude.Text ->
  -- | 'city'
  Prelude.Text ->
  -- | 'state'
  Prelude.Text ->
  -- | 'country'
  Prelude.Text ->
  -- | 'postalCode'
  Prelude.Text ->
  ValidateE911Address
newValidateE911Address
  pAwsAccountId_
  pStreetNumber_
  pStreetInfo_
  pCity_
  pState_
  pCountry_
  pPostalCode_ =
    ValidateE911Address'
      { awsAccountId = pAwsAccountId_,
        streetNumber = Data._Sensitive Lens.# pStreetNumber_,
        streetInfo = Data._Sensitive Lens.# pStreetInfo_,
        city = Data._Sensitive Lens.# pCity_,
        state = Data._Sensitive Lens.# pState_,
        country = Data._Sensitive Lens.# pCountry_,
        postalCode = Data._Sensitive Lens.# pPostalCode_
      }

-- | Undocumented member.
validateE911Address_awsAccountId :: Lens.Lens' ValidateE911Address Prelude.Text
validateE911Address_awsAccountId = Lens.lens (\ValidateE911Address' {awsAccountId} -> awsAccountId) (\s@ValidateE911Address' {} a -> s {awsAccountId = a} :: ValidateE911Address)

-- | Undocumented member.
validateE911Address_streetNumber :: Lens.Lens' ValidateE911Address Prelude.Text
validateE911Address_streetNumber = Lens.lens (\ValidateE911Address' {streetNumber} -> streetNumber) (\s@ValidateE911Address' {} a -> s {streetNumber = a} :: ValidateE911Address) Prelude.. Data._Sensitive

-- | Undocumented member.
validateE911Address_streetInfo :: Lens.Lens' ValidateE911Address Prelude.Text
validateE911Address_streetInfo = Lens.lens (\ValidateE911Address' {streetInfo} -> streetInfo) (\s@ValidateE911Address' {} a -> s {streetInfo = a} :: ValidateE911Address) Prelude.. Data._Sensitive

-- | Undocumented member.
validateE911Address_city :: Lens.Lens' ValidateE911Address Prelude.Text
validateE911Address_city = Lens.lens (\ValidateE911Address' {city} -> city) (\s@ValidateE911Address' {} a -> s {city = a} :: ValidateE911Address) Prelude.. Data._Sensitive

-- | Undocumented member.
validateE911Address_state :: Lens.Lens' ValidateE911Address Prelude.Text
validateE911Address_state = Lens.lens (\ValidateE911Address' {state} -> state) (\s@ValidateE911Address' {} a -> s {state = a} :: ValidateE911Address) Prelude.. Data._Sensitive

-- | Undocumented member.
validateE911Address_country :: Lens.Lens' ValidateE911Address Prelude.Text
validateE911Address_country = Lens.lens (\ValidateE911Address' {country} -> country) (\s@ValidateE911Address' {} a -> s {country = a} :: ValidateE911Address) Prelude.. Data._Sensitive

-- | Undocumented member.
validateE911Address_postalCode :: Lens.Lens' ValidateE911Address Prelude.Text
validateE911Address_postalCode = Lens.lens (\ValidateE911Address' {postalCode} -> postalCode) (\s@ValidateE911Address' {} a -> s {postalCode = a} :: ValidateE911Address) Prelude.. Data._Sensitive

instance Core.AWSRequest ValidateE911Address where
  type
    AWSResponse ValidateE911Address =
      ValidateE911AddressResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ValidateE911AddressResponse'
            Prelude.<$> (x Data..?> "Address")
            Prelude.<*> (x Data..?> "AddressExternalId")
            Prelude.<*> ( x
                            Data..?> "CandidateAddressList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "ValidationResult")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ValidateE911Address where
  hashWithSalt _salt ValidateE911Address' {..} =
    _salt
      `Prelude.hashWithSalt` awsAccountId
      `Prelude.hashWithSalt` streetNumber
      `Prelude.hashWithSalt` streetInfo
      `Prelude.hashWithSalt` city
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` country
      `Prelude.hashWithSalt` postalCode

instance Prelude.NFData ValidateE911Address where
  rnf ValidateE911Address' {..} =
    Prelude.rnf awsAccountId
      `Prelude.seq` Prelude.rnf streetNumber
      `Prelude.seq` Prelude.rnf streetInfo
      `Prelude.seq` Prelude.rnf city
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf country
      `Prelude.seq` Prelude.rnf postalCode

instance Data.ToHeaders ValidateE911Address where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON ValidateE911Address where
  toJSON ValidateE911Address' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("AwsAccountId" Data..= awsAccountId),
            Prelude.Just ("StreetNumber" Data..= streetNumber),
            Prelude.Just ("StreetInfo" Data..= streetInfo),
            Prelude.Just ("City" Data..= city),
            Prelude.Just ("State" Data..= state),
            Prelude.Just ("Country" Data..= country),
            Prelude.Just ("PostalCode" Data..= postalCode)
          ]
      )

instance Data.ToPath ValidateE911Address where
  toPath = Prelude.const "/emergency-calling/address"

instance Data.ToQuery ValidateE911Address where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newValidateE911AddressResponse' smart constructor.
data ValidateE911AddressResponse = ValidateE911AddressResponse'
  { address :: Prelude.Maybe Address,
    addressExternalId :: Prelude.Maybe Prelude.Text,
    candidateAddressList :: Prelude.Maybe [CandidateAddress],
    validationResult :: Prelude.Maybe Prelude.Natural,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ValidateE911AddressResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'address', 'validateE911AddressResponse_address' - Undocumented member.
--
-- 'addressExternalId', 'validateE911AddressResponse_addressExternalId' - Undocumented member.
--
-- 'candidateAddressList', 'validateE911AddressResponse_candidateAddressList' - Undocumented member.
--
-- 'validationResult', 'validateE911AddressResponse_validationResult' - Undocumented member.
--
-- 'httpStatus', 'validateE911AddressResponse_httpStatus' - The response's http status code.
newValidateE911AddressResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ValidateE911AddressResponse
newValidateE911AddressResponse pHttpStatus_ =
  ValidateE911AddressResponse'
    { address =
        Prelude.Nothing,
      addressExternalId = Prelude.Nothing,
      candidateAddressList = Prelude.Nothing,
      validationResult = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
validateE911AddressResponse_address :: Lens.Lens' ValidateE911AddressResponse (Prelude.Maybe Address)
validateE911AddressResponse_address = Lens.lens (\ValidateE911AddressResponse' {address} -> address) (\s@ValidateE911AddressResponse' {} a -> s {address = a} :: ValidateE911AddressResponse)

-- | Undocumented member.
validateE911AddressResponse_addressExternalId :: Lens.Lens' ValidateE911AddressResponse (Prelude.Maybe Prelude.Text)
validateE911AddressResponse_addressExternalId = Lens.lens (\ValidateE911AddressResponse' {addressExternalId} -> addressExternalId) (\s@ValidateE911AddressResponse' {} a -> s {addressExternalId = a} :: ValidateE911AddressResponse)

-- | Undocumented member.
validateE911AddressResponse_candidateAddressList :: Lens.Lens' ValidateE911AddressResponse (Prelude.Maybe [CandidateAddress])
validateE911AddressResponse_candidateAddressList = Lens.lens (\ValidateE911AddressResponse' {candidateAddressList} -> candidateAddressList) (\s@ValidateE911AddressResponse' {} a -> s {candidateAddressList = a} :: ValidateE911AddressResponse) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
validateE911AddressResponse_validationResult :: Lens.Lens' ValidateE911AddressResponse (Prelude.Maybe Prelude.Natural)
validateE911AddressResponse_validationResult = Lens.lens (\ValidateE911AddressResponse' {validationResult} -> validationResult) (\s@ValidateE911AddressResponse' {} a -> s {validationResult = a} :: ValidateE911AddressResponse)

-- | The response's http status code.
validateE911AddressResponse_httpStatus :: Lens.Lens' ValidateE911AddressResponse Prelude.Int
validateE911AddressResponse_httpStatus = Lens.lens (\ValidateE911AddressResponse' {httpStatus} -> httpStatus) (\s@ValidateE911AddressResponse' {} a -> s {httpStatus = a} :: ValidateE911AddressResponse)

instance Prelude.NFData ValidateE911AddressResponse where
  rnf ValidateE911AddressResponse' {..} =
    Prelude.rnf address
      `Prelude.seq` Prelude.rnf addressExternalId
      `Prelude.seq` Prelude.rnf candidateAddressList
      `Prelude.seq` Prelude.rnf validationResult
      `Prelude.seq` Prelude.rnf httpStatus
