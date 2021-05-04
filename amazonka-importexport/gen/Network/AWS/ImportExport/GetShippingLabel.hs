{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.ImportExport.GetShippingLabel
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation generates a pre-paid UPS shipping label that you will use
-- to ship your device to AWS for processing.
module Network.AWS.ImportExport.GetShippingLabel
  ( -- * Creating a Request
    GetShippingLabel (..),
    newGetShippingLabel,

    -- * Request Lenses
    getShippingLabel_company,
    getShippingLabel_phoneNumber,
    getShippingLabel_postalCode,
    getShippingLabel_street1,
    getShippingLabel_aPIVersion,
    getShippingLabel_name,
    getShippingLabel_city,
    getShippingLabel_street2,
    getShippingLabel_stateOrProvince,
    getShippingLabel_country,
    getShippingLabel_street3,
    getShippingLabel_jobIds,

    -- * Destructuring the Response
    GetShippingLabelResponse (..),
    newGetShippingLabelResponse,

    -- * Response Lenses
    getShippingLabelResponse_warning,
    getShippingLabelResponse_shippingLabelURL,
    getShippingLabelResponse_httpStatus,
  )
where

import Network.AWS.ImportExport.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetShippingLabel' smart constructor.
data GetShippingLabel = GetShippingLabel'
  { company :: Prelude.Maybe Prelude.Text,
    phoneNumber :: Prelude.Maybe Prelude.Text,
    postalCode :: Prelude.Maybe Prelude.Text,
    street1 :: Prelude.Maybe Prelude.Text,
    aPIVersion :: Prelude.Maybe Prelude.Text,
    name :: Prelude.Maybe Prelude.Text,
    city :: Prelude.Maybe Prelude.Text,
    street2 :: Prelude.Maybe Prelude.Text,
    stateOrProvince :: Prelude.Maybe Prelude.Text,
    country :: Prelude.Maybe Prelude.Text,
    street3 :: Prelude.Maybe Prelude.Text,
    jobIds :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetShippingLabel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'company', 'getShippingLabel_company' - Undocumented member.
--
-- 'phoneNumber', 'getShippingLabel_phoneNumber' - Undocumented member.
--
-- 'postalCode', 'getShippingLabel_postalCode' - Undocumented member.
--
-- 'street1', 'getShippingLabel_street1' - Undocumented member.
--
-- 'aPIVersion', 'getShippingLabel_aPIVersion' - Undocumented member.
--
-- 'name', 'getShippingLabel_name' - Undocumented member.
--
-- 'city', 'getShippingLabel_city' - Undocumented member.
--
-- 'street2', 'getShippingLabel_street2' - Undocumented member.
--
-- 'stateOrProvince', 'getShippingLabel_stateOrProvince' - Undocumented member.
--
-- 'country', 'getShippingLabel_country' - Undocumented member.
--
-- 'street3', 'getShippingLabel_street3' - Undocumented member.
--
-- 'jobIds', 'getShippingLabel_jobIds' - Undocumented member.
newGetShippingLabel ::
  GetShippingLabel
newGetShippingLabel =
  GetShippingLabel'
    { company = Prelude.Nothing,
      phoneNumber = Prelude.Nothing,
      postalCode = Prelude.Nothing,
      street1 = Prelude.Nothing,
      aPIVersion = Prelude.Nothing,
      name = Prelude.Nothing,
      city = Prelude.Nothing,
      street2 = Prelude.Nothing,
      stateOrProvince = Prelude.Nothing,
      country = Prelude.Nothing,
      street3 = Prelude.Nothing,
      jobIds = Prelude.mempty
    }

-- | Undocumented member.
getShippingLabel_company :: Lens.Lens' GetShippingLabel (Prelude.Maybe Prelude.Text)
getShippingLabel_company = Lens.lens (\GetShippingLabel' {company} -> company) (\s@GetShippingLabel' {} a -> s {company = a} :: GetShippingLabel)

-- | Undocumented member.
getShippingLabel_phoneNumber :: Lens.Lens' GetShippingLabel (Prelude.Maybe Prelude.Text)
getShippingLabel_phoneNumber = Lens.lens (\GetShippingLabel' {phoneNumber} -> phoneNumber) (\s@GetShippingLabel' {} a -> s {phoneNumber = a} :: GetShippingLabel)

-- | Undocumented member.
getShippingLabel_postalCode :: Lens.Lens' GetShippingLabel (Prelude.Maybe Prelude.Text)
getShippingLabel_postalCode = Lens.lens (\GetShippingLabel' {postalCode} -> postalCode) (\s@GetShippingLabel' {} a -> s {postalCode = a} :: GetShippingLabel)

-- | Undocumented member.
getShippingLabel_street1 :: Lens.Lens' GetShippingLabel (Prelude.Maybe Prelude.Text)
getShippingLabel_street1 = Lens.lens (\GetShippingLabel' {street1} -> street1) (\s@GetShippingLabel' {} a -> s {street1 = a} :: GetShippingLabel)

-- | Undocumented member.
getShippingLabel_aPIVersion :: Lens.Lens' GetShippingLabel (Prelude.Maybe Prelude.Text)
getShippingLabel_aPIVersion = Lens.lens (\GetShippingLabel' {aPIVersion} -> aPIVersion) (\s@GetShippingLabel' {} a -> s {aPIVersion = a} :: GetShippingLabel)

-- | Undocumented member.
getShippingLabel_name :: Lens.Lens' GetShippingLabel (Prelude.Maybe Prelude.Text)
getShippingLabel_name = Lens.lens (\GetShippingLabel' {name} -> name) (\s@GetShippingLabel' {} a -> s {name = a} :: GetShippingLabel)

-- | Undocumented member.
getShippingLabel_city :: Lens.Lens' GetShippingLabel (Prelude.Maybe Prelude.Text)
getShippingLabel_city = Lens.lens (\GetShippingLabel' {city} -> city) (\s@GetShippingLabel' {} a -> s {city = a} :: GetShippingLabel)

-- | Undocumented member.
getShippingLabel_street2 :: Lens.Lens' GetShippingLabel (Prelude.Maybe Prelude.Text)
getShippingLabel_street2 = Lens.lens (\GetShippingLabel' {street2} -> street2) (\s@GetShippingLabel' {} a -> s {street2 = a} :: GetShippingLabel)

-- | Undocumented member.
getShippingLabel_stateOrProvince :: Lens.Lens' GetShippingLabel (Prelude.Maybe Prelude.Text)
getShippingLabel_stateOrProvince = Lens.lens (\GetShippingLabel' {stateOrProvince} -> stateOrProvince) (\s@GetShippingLabel' {} a -> s {stateOrProvince = a} :: GetShippingLabel)

-- | Undocumented member.
getShippingLabel_country :: Lens.Lens' GetShippingLabel (Prelude.Maybe Prelude.Text)
getShippingLabel_country = Lens.lens (\GetShippingLabel' {country} -> country) (\s@GetShippingLabel' {} a -> s {country = a} :: GetShippingLabel)

-- | Undocumented member.
getShippingLabel_street3 :: Lens.Lens' GetShippingLabel (Prelude.Maybe Prelude.Text)
getShippingLabel_street3 = Lens.lens (\GetShippingLabel' {street3} -> street3) (\s@GetShippingLabel' {} a -> s {street3 = a} :: GetShippingLabel)

-- | Undocumented member.
getShippingLabel_jobIds :: Lens.Lens' GetShippingLabel [Prelude.Text]
getShippingLabel_jobIds = Lens.lens (\GetShippingLabel' {jobIds} -> jobIds) (\s@GetShippingLabel' {} a -> s {jobIds = a} :: GetShippingLabel) Prelude.. Prelude._Coerce

instance Prelude.AWSRequest GetShippingLabel where
  type Rs GetShippingLabel = GetShippingLabelResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "GetShippingLabelResult"
      ( \s h x ->
          GetShippingLabelResponse'
            Prelude.<$> (x Prelude..@? "Warning")
            Prelude.<*> (x Prelude..@? "ShippingLabelURL")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetShippingLabel

instance Prelude.NFData GetShippingLabel

instance Prelude.ToHeaders GetShippingLabel where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath GetShippingLabel where
  toPath = Prelude.const "/"

instance Prelude.ToQuery GetShippingLabel where
  toQuery GetShippingLabel' {..} =
    Prelude.mconcat
      [ "Operation=GetShippingLabel",
        "Action"
          Prelude.=: ("GetShippingLabel" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2010-06-01" :: Prelude.ByteString),
        "company" Prelude.=: company,
        "phoneNumber" Prelude.=: phoneNumber,
        "postalCode" Prelude.=: postalCode,
        "street1" Prelude.=: street1,
        "APIVersion" Prelude.=: aPIVersion,
        "name" Prelude.=: name,
        "city" Prelude.=: city,
        "street2" Prelude.=: street2,
        "stateOrProvince" Prelude.=: stateOrProvince,
        "country" Prelude.=: country,
        "street3" Prelude.=: street3,
        "jobIds"
          Prelude.=: Prelude.toQueryList "member" jobIds
      ]

-- | /See:/ 'newGetShippingLabelResponse' smart constructor.
data GetShippingLabelResponse = GetShippingLabelResponse'
  { warning :: Prelude.Maybe Prelude.Text,
    shippingLabelURL :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetShippingLabelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'warning', 'getShippingLabelResponse_warning' - Undocumented member.
--
-- 'shippingLabelURL', 'getShippingLabelResponse_shippingLabelURL' - Undocumented member.
--
-- 'httpStatus', 'getShippingLabelResponse_httpStatus' - The response's http status code.
newGetShippingLabelResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetShippingLabelResponse
newGetShippingLabelResponse pHttpStatus_ =
  GetShippingLabelResponse'
    { warning =
        Prelude.Nothing,
      shippingLabelURL = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
getShippingLabelResponse_warning :: Lens.Lens' GetShippingLabelResponse (Prelude.Maybe Prelude.Text)
getShippingLabelResponse_warning = Lens.lens (\GetShippingLabelResponse' {warning} -> warning) (\s@GetShippingLabelResponse' {} a -> s {warning = a} :: GetShippingLabelResponse)

-- | Undocumented member.
getShippingLabelResponse_shippingLabelURL :: Lens.Lens' GetShippingLabelResponse (Prelude.Maybe Prelude.Text)
getShippingLabelResponse_shippingLabelURL = Lens.lens (\GetShippingLabelResponse' {shippingLabelURL} -> shippingLabelURL) (\s@GetShippingLabelResponse' {} a -> s {shippingLabelURL = a} :: GetShippingLabelResponse)

-- | The response's http status code.
getShippingLabelResponse_httpStatus :: Lens.Lens' GetShippingLabelResponse Prelude.Int
getShippingLabelResponse_httpStatus = Lens.lens (\GetShippingLabelResponse' {httpStatus} -> httpStatus) (\s@GetShippingLabelResponse' {} a -> s {httpStatus = a} :: GetShippingLabelResponse)

instance Prelude.NFData GetShippingLabelResponse
