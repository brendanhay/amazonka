{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53Domains.Types.ExtraParamName
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53Domains.Types.ExtraParamName where

import Network.AWS.Prelude

data ExtraParamName
  = AuIdNumber
  | AuIdType
  | BirthCity
  | BirthCountry
  | BirthDateInYyyyMmDd
  | BirthDepartment
  | BrandNumber
  | CaBusinessEntityType
  | CaLegalRepresentative
  | CaLegalRepresentativeCapacity
  | CaLegalType
  | DocumentNumber
  | DunsNumber
  | EsIdentification
  | EsIdentificationType
  | EsLegalForm
  | FiBusinessNumber
  | FiIdNumber
  | FiNationality
  | FiOrganizationType
  | ItNationality
  | ItPin
  | ItRegistrantEntityType
  | RuPassportData
  | SeIdNumber
  | SgIdNumber
  | UkCompanyNumber
  | UkContactType
  | VatNumber
  deriving
    ( Eq,
      Ord,
      Read,
      Show,
      Enum,
      Bounded,
      Data,
      Typeable,
      Generic
    )

instance FromText ExtraParamName where
  parser =
    takeLowerText >>= \case
      "au_id_number" -> pure AuIdNumber
      "au_id_type" -> pure AuIdType
      "birth_city" -> pure BirthCity
      "birth_country" -> pure BirthCountry
      "birth_date_in_yyyy_mm_dd" -> pure BirthDateInYyyyMmDd
      "birth_department" -> pure BirthDepartment
      "brand_number" -> pure BrandNumber
      "ca_business_entity_type" -> pure CaBusinessEntityType
      "ca_legal_representative" -> pure CaLegalRepresentative
      "ca_legal_representative_capacity" -> pure CaLegalRepresentativeCapacity
      "ca_legal_type" -> pure CaLegalType
      "document_number" -> pure DocumentNumber
      "duns_number" -> pure DunsNumber
      "es_identification" -> pure EsIdentification
      "es_identification_type" -> pure EsIdentificationType
      "es_legal_form" -> pure EsLegalForm
      "fi_business_number" -> pure FiBusinessNumber
      "fi_id_number" -> pure FiIdNumber
      "fi_nationality" -> pure FiNationality
      "fi_organization_type" -> pure FiOrganizationType
      "it_nationality" -> pure ItNationality
      "it_pin" -> pure ItPin
      "it_registrant_entity_type" -> pure ItRegistrantEntityType
      "ru_passport_data" -> pure RuPassportData
      "se_id_number" -> pure SeIdNumber
      "sg_id_number" -> pure SgIdNumber
      "uk_company_number" -> pure UkCompanyNumber
      "uk_contact_type" -> pure UkContactType
      "vat_number" -> pure VatNumber
      e ->
        fromTextError $
          "Failure parsing ExtraParamName from value: '" <> e
            <> "'. Accepted values: au_id_number, au_id_type, birth_city, birth_country, birth_date_in_yyyy_mm_dd, birth_department, brand_number, ca_business_entity_type, ca_legal_representative, ca_legal_representative_capacity, ca_legal_type, document_number, duns_number, es_identification, es_identification_type, es_legal_form, fi_business_number, fi_id_number, fi_nationality, fi_organization_type, it_nationality, it_pin, it_registrant_entity_type, ru_passport_data, se_id_number, sg_id_number, uk_company_number, uk_contact_type, vat_number"

instance ToText ExtraParamName where
  toText = \case
    AuIdNumber -> "AU_ID_NUMBER"
    AuIdType -> "AU_ID_TYPE"
    BirthCity -> "BIRTH_CITY"
    BirthCountry -> "BIRTH_COUNTRY"
    BirthDateInYyyyMmDd -> "BIRTH_DATE_IN_YYYY_MM_DD"
    BirthDepartment -> "BIRTH_DEPARTMENT"
    BrandNumber -> "BRAND_NUMBER"
    CaBusinessEntityType -> "CA_BUSINESS_ENTITY_TYPE"
    CaLegalRepresentative -> "CA_LEGAL_REPRESENTATIVE"
    CaLegalRepresentativeCapacity -> "CA_LEGAL_REPRESENTATIVE_CAPACITY"
    CaLegalType -> "CA_LEGAL_TYPE"
    DocumentNumber -> "DOCUMENT_NUMBER"
    DunsNumber -> "DUNS_NUMBER"
    EsIdentification -> "ES_IDENTIFICATION"
    EsIdentificationType -> "ES_IDENTIFICATION_TYPE"
    EsLegalForm -> "ES_LEGAL_FORM"
    FiBusinessNumber -> "FI_BUSINESS_NUMBER"
    FiIdNumber -> "FI_ID_NUMBER"
    FiNationality -> "FI_NATIONALITY"
    FiOrganizationType -> "FI_ORGANIZATION_TYPE"
    ItNationality -> "IT_NATIONALITY"
    ItPin -> "IT_PIN"
    ItRegistrantEntityType -> "IT_REGISTRANT_ENTITY_TYPE"
    RuPassportData -> "RU_PASSPORT_DATA"
    SeIdNumber -> "SE_ID_NUMBER"
    SgIdNumber -> "SG_ID_NUMBER"
    UkCompanyNumber -> "UK_COMPANY_NUMBER"
    UkContactType -> "UK_CONTACT_TYPE"
    VatNumber -> "VAT_NUMBER"

instance Hashable ExtraParamName

instance NFData ExtraParamName

instance ToByteString ExtraParamName

instance ToQuery ExtraParamName

instance ToHeader ExtraParamName

instance ToJSON ExtraParamName where
  toJSON = toJSONText

instance FromJSON ExtraParamName where
  parseJSON = parseJSONText "ExtraParamName"
