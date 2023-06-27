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
-- Module      : Amazonka.ChimeSdkVoice.ListSupportedPhoneNumberCountries
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the countries that you can order phone numbers from.
module Amazonka.ChimeSdkVoice.ListSupportedPhoneNumberCountries
  ( -- * Creating a Request
    ListSupportedPhoneNumberCountries (..),
    newListSupportedPhoneNumberCountries,

    -- * Request Lenses
    listSupportedPhoneNumberCountries_productType,

    -- * Destructuring the Response
    ListSupportedPhoneNumberCountriesResponse (..),
    newListSupportedPhoneNumberCountriesResponse,

    -- * Response Lenses
    listSupportedPhoneNumberCountriesResponse_phoneNumberCountries,
    listSupportedPhoneNumberCountriesResponse_httpStatus,
  )
where

import Amazonka.ChimeSdkVoice.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListSupportedPhoneNumberCountries' smart constructor.
data ListSupportedPhoneNumberCountries = ListSupportedPhoneNumberCountries'
  { -- | The phone number product type.
    productType :: PhoneNumberProductType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSupportedPhoneNumberCountries' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'productType', 'listSupportedPhoneNumberCountries_productType' - The phone number product type.
newListSupportedPhoneNumberCountries ::
  -- | 'productType'
  PhoneNumberProductType ->
  ListSupportedPhoneNumberCountries
newListSupportedPhoneNumberCountries pProductType_ =
  ListSupportedPhoneNumberCountries'
    { productType =
        pProductType_
    }

-- | The phone number product type.
listSupportedPhoneNumberCountries_productType :: Lens.Lens' ListSupportedPhoneNumberCountries PhoneNumberProductType
listSupportedPhoneNumberCountries_productType = Lens.lens (\ListSupportedPhoneNumberCountries' {productType} -> productType) (\s@ListSupportedPhoneNumberCountries' {} a -> s {productType = a} :: ListSupportedPhoneNumberCountries)

instance
  Core.AWSRequest
    ListSupportedPhoneNumberCountries
  where
  type
    AWSResponse ListSupportedPhoneNumberCountries =
      ListSupportedPhoneNumberCountriesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListSupportedPhoneNumberCountriesResponse'
            Prelude.<$> ( x
                            Data..?> "PhoneNumberCountries"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListSupportedPhoneNumberCountries
  where
  hashWithSalt
    _salt
    ListSupportedPhoneNumberCountries' {..} =
      _salt `Prelude.hashWithSalt` productType

instance
  Prelude.NFData
    ListSupportedPhoneNumberCountries
  where
  rnf ListSupportedPhoneNumberCountries' {..} =
    Prelude.rnf productType

instance
  Data.ToHeaders
    ListSupportedPhoneNumberCountries
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    ListSupportedPhoneNumberCountries
  where
  toPath = Prelude.const "/phone-number-countries"

instance
  Data.ToQuery
    ListSupportedPhoneNumberCountries
  where
  toQuery ListSupportedPhoneNumberCountries' {..} =
    Prelude.mconcat
      ["product-type" Data.=: productType]

-- | /See:/ 'newListSupportedPhoneNumberCountriesResponse' smart constructor.
data ListSupportedPhoneNumberCountriesResponse = ListSupportedPhoneNumberCountriesResponse'
  { -- | The supported phone number countries.
    phoneNumberCountries :: Prelude.Maybe [PhoneNumberCountry],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSupportedPhoneNumberCountriesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'phoneNumberCountries', 'listSupportedPhoneNumberCountriesResponse_phoneNumberCountries' - The supported phone number countries.
--
-- 'httpStatus', 'listSupportedPhoneNumberCountriesResponse_httpStatus' - The response's http status code.
newListSupportedPhoneNumberCountriesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListSupportedPhoneNumberCountriesResponse
newListSupportedPhoneNumberCountriesResponse
  pHttpStatus_ =
    ListSupportedPhoneNumberCountriesResponse'
      { phoneNumberCountries =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The supported phone number countries.
listSupportedPhoneNumberCountriesResponse_phoneNumberCountries :: Lens.Lens' ListSupportedPhoneNumberCountriesResponse (Prelude.Maybe [PhoneNumberCountry])
listSupportedPhoneNumberCountriesResponse_phoneNumberCountries = Lens.lens (\ListSupportedPhoneNumberCountriesResponse' {phoneNumberCountries} -> phoneNumberCountries) (\s@ListSupportedPhoneNumberCountriesResponse' {} a -> s {phoneNumberCountries = a} :: ListSupportedPhoneNumberCountriesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listSupportedPhoneNumberCountriesResponse_httpStatus :: Lens.Lens' ListSupportedPhoneNumberCountriesResponse Prelude.Int
listSupportedPhoneNumberCountriesResponse_httpStatus = Lens.lens (\ListSupportedPhoneNumberCountriesResponse' {httpStatus} -> httpStatus) (\s@ListSupportedPhoneNumberCountriesResponse' {} a -> s {httpStatus = a} :: ListSupportedPhoneNumberCountriesResponse)

instance
  Prelude.NFData
    ListSupportedPhoneNumberCountriesResponse
  where
  rnf ListSupportedPhoneNumberCountriesResponse' {..} =
    Prelude.rnf phoneNumberCountries
      `Prelude.seq` Prelude.rnf httpStatus
