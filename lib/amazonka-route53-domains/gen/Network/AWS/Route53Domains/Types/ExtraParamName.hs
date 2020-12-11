-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53Domains.Types.ExtraParamName
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53Domains.Types.ExtraParamName
  ( ExtraParamName
      ( ExtraParamName',
        AuIdNumber,
        AuIdType,
        BirthCity,
        BirthCountry,
        BirthDateInYyyyMmDd,
        BirthDepartment,
        BrandNumber,
        CaBusinessEntityType,
        CaLegalRepresentative,
        CaLegalRepresentativeCapacity,
        CaLegalType,
        DocumentNumber,
        DunsNumber,
        EsIdentification,
        EsIdentificationType,
        EsLegalForm,
        FiBusinessNumber,
        FiIdNumber,
        FiNationality,
        FiOrganizationType,
        ItNationality,
        ItPin,
        ItRegistrantEntityType,
        RuPassportData,
        SeIdNumber,
        SgIdNumber,
        UkCompanyNumber,
        UkContactType,
        VatNumber
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ExtraParamName = ExtraParamName' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern AuIdNumber :: ExtraParamName
pattern AuIdNumber = ExtraParamName' "AU_ID_NUMBER"

pattern AuIdType :: ExtraParamName
pattern AuIdType = ExtraParamName' "AU_ID_TYPE"

pattern BirthCity :: ExtraParamName
pattern BirthCity = ExtraParamName' "BIRTH_CITY"

pattern BirthCountry :: ExtraParamName
pattern BirthCountry = ExtraParamName' "BIRTH_COUNTRY"

pattern BirthDateInYyyyMmDd :: ExtraParamName
pattern BirthDateInYyyyMmDd = ExtraParamName' "BIRTH_DATE_IN_YYYY_MM_DD"

pattern BirthDepartment :: ExtraParamName
pattern BirthDepartment = ExtraParamName' "BIRTH_DEPARTMENT"

pattern BrandNumber :: ExtraParamName
pattern BrandNumber = ExtraParamName' "BRAND_NUMBER"

pattern CaBusinessEntityType :: ExtraParamName
pattern CaBusinessEntityType = ExtraParamName' "CA_BUSINESS_ENTITY_TYPE"

pattern CaLegalRepresentative :: ExtraParamName
pattern CaLegalRepresentative = ExtraParamName' "CA_LEGAL_REPRESENTATIVE"

pattern CaLegalRepresentativeCapacity :: ExtraParamName
pattern CaLegalRepresentativeCapacity = ExtraParamName' "CA_LEGAL_REPRESENTATIVE_CAPACITY"

pattern CaLegalType :: ExtraParamName
pattern CaLegalType = ExtraParamName' "CA_LEGAL_TYPE"

pattern DocumentNumber :: ExtraParamName
pattern DocumentNumber = ExtraParamName' "DOCUMENT_NUMBER"

pattern DunsNumber :: ExtraParamName
pattern DunsNumber = ExtraParamName' "DUNS_NUMBER"

pattern EsIdentification :: ExtraParamName
pattern EsIdentification = ExtraParamName' "ES_IDENTIFICATION"

pattern EsIdentificationType :: ExtraParamName
pattern EsIdentificationType = ExtraParamName' "ES_IDENTIFICATION_TYPE"

pattern EsLegalForm :: ExtraParamName
pattern EsLegalForm = ExtraParamName' "ES_LEGAL_FORM"

pattern FiBusinessNumber :: ExtraParamName
pattern FiBusinessNumber = ExtraParamName' "FI_BUSINESS_NUMBER"

pattern FiIdNumber :: ExtraParamName
pattern FiIdNumber = ExtraParamName' "FI_ID_NUMBER"

pattern FiNationality :: ExtraParamName
pattern FiNationality = ExtraParamName' "FI_NATIONALITY"

pattern FiOrganizationType :: ExtraParamName
pattern FiOrganizationType = ExtraParamName' "FI_ORGANIZATION_TYPE"

pattern ItNationality :: ExtraParamName
pattern ItNationality = ExtraParamName' "IT_NATIONALITY"

pattern ItPin :: ExtraParamName
pattern ItPin = ExtraParamName' "IT_PIN"

pattern ItRegistrantEntityType :: ExtraParamName
pattern ItRegistrantEntityType = ExtraParamName' "IT_REGISTRANT_ENTITY_TYPE"

pattern RuPassportData :: ExtraParamName
pattern RuPassportData = ExtraParamName' "RU_PASSPORT_DATA"

pattern SeIdNumber :: ExtraParamName
pattern SeIdNumber = ExtraParamName' "SE_ID_NUMBER"

pattern SgIdNumber :: ExtraParamName
pattern SgIdNumber = ExtraParamName' "SG_ID_NUMBER"

pattern UkCompanyNumber :: ExtraParamName
pattern UkCompanyNumber = ExtraParamName' "UK_COMPANY_NUMBER"

pattern UkContactType :: ExtraParamName
pattern UkContactType = ExtraParamName' "UK_CONTACT_TYPE"

pattern VatNumber :: ExtraParamName
pattern VatNumber = ExtraParamName' "VAT_NUMBER"

{-# COMPLETE
  AuIdNumber,
  AuIdType,
  BirthCity,
  BirthCountry,
  BirthDateInYyyyMmDd,
  BirthDepartment,
  BrandNumber,
  CaBusinessEntityType,
  CaLegalRepresentative,
  CaLegalRepresentativeCapacity,
  CaLegalType,
  DocumentNumber,
  DunsNumber,
  EsIdentification,
  EsIdentificationType,
  EsLegalForm,
  FiBusinessNumber,
  FiIdNumber,
  FiNationality,
  FiOrganizationType,
  ItNationality,
  ItPin,
  ItRegistrantEntityType,
  RuPassportData,
  SeIdNumber,
  SgIdNumber,
  UkCompanyNumber,
  UkContactType,
  VatNumber,
  ExtraParamName'
  #-}
