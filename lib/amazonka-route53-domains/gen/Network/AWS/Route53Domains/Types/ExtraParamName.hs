{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
        ExtraParamNameDunsNumber,
        ExtraParamNameBrandNumber,
        ExtraParamNameBirthDepartment,
        ExtraParamNameBirthDateInYyyyMmDd,
        ExtraParamNameBirthCountry,
        ExtraParamNameBirthCity,
        ExtraParamNameDocumentNumber,
        ExtraParamNameAuIdNumber,
        ExtraParamNameAuIdType,
        ExtraParamNameCaLegalType,
        ExtraParamNameCaBusinessEntityType,
        ExtraParamNameCaLegalRepresentative,
        ExtraParamNameCaLegalRepresentativeCapacity,
        ExtraParamNameEsIdentification,
        ExtraParamNameEsIdentificationType,
        ExtraParamNameEsLegalForm,
        ExtraParamNameFiBusinessNumber,
        ExtraParamNameFiIdNumber,
        ExtraParamNameFiNationality,
        ExtraParamNameFiOrganizationType,
        ExtraParamNameItNationality,
        ExtraParamNameItPin,
        ExtraParamNameItRegistrantEntityType,
        ExtraParamNameRuPassportData,
        ExtraParamNameSeIdNumber,
        ExtraParamNameSgIdNumber,
        ExtraParamNameVatNumber,
        ExtraParamNameUkContactType,
        ExtraParamNameUkCompanyNumber,
        fromExtraParamName
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype ExtraParamName = ExtraParamName'
  { fromExtraParamName ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern ExtraParamNameDunsNumber :: ExtraParamName
pattern ExtraParamNameDunsNumber = ExtraParamName' "DUNS_NUMBER"

pattern ExtraParamNameBrandNumber :: ExtraParamName
pattern ExtraParamNameBrandNumber = ExtraParamName' "BRAND_NUMBER"

pattern ExtraParamNameBirthDepartment :: ExtraParamName
pattern ExtraParamNameBirthDepartment = ExtraParamName' "BIRTH_DEPARTMENT"

pattern ExtraParamNameBirthDateInYyyyMmDd :: ExtraParamName
pattern ExtraParamNameBirthDateInYyyyMmDd = ExtraParamName' "BIRTH_DATE_IN_YYYY_MM_DD"

pattern ExtraParamNameBirthCountry :: ExtraParamName
pattern ExtraParamNameBirthCountry = ExtraParamName' "BIRTH_COUNTRY"

pattern ExtraParamNameBirthCity :: ExtraParamName
pattern ExtraParamNameBirthCity = ExtraParamName' "BIRTH_CITY"

pattern ExtraParamNameDocumentNumber :: ExtraParamName
pattern ExtraParamNameDocumentNumber = ExtraParamName' "DOCUMENT_NUMBER"

pattern ExtraParamNameAuIdNumber :: ExtraParamName
pattern ExtraParamNameAuIdNumber = ExtraParamName' "AU_ID_NUMBER"

pattern ExtraParamNameAuIdType :: ExtraParamName
pattern ExtraParamNameAuIdType = ExtraParamName' "AU_ID_TYPE"

pattern ExtraParamNameCaLegalType :: ExtraParamName
pattern ExtraParamNameCaLegalType = ExtraParamName' "CA_LEGAL_TYPE"

pattern ExtraParamNameCaBusinessEntityType :: ExtraParamName
pattern ExtraParamNameCaBusinessEntityType = ExtraParamName' "CA_BUSINESS_ENTITY_TYPE"

pattern ExtraParamNameCaLegalRepresentative :: ExtraParamName
pattern ExtraParamNameCaLegalRepresentative = ExtraParamName' "CA_LEGAL_REPRESENTATIVE"

pattern ExtraParamNameCaLegalRepresentativeCapacity :: ExtraParamName
pattern ExtraParamNameCaLegalRepresentativeCapacity = ExtraParamName' "CA_LEGAL_REPRESENTATIVE_CAPACITY"

pattern ExtraParamNameEsIdentification :: ExtraParamName
pattern ExtraParamNameEsIdentification = ExtraParamName' "ES_IDENTIFICATION"

pattern ExtraParamNameEsIdentificationType :: ExtraParamName
pattern ExtraParamNameEsIdentificationType = ExtraParamName' "ES_IDENTIFICATION_TYPE"

pattern ExtraParamNameEsLegalForm :: ExtraParamName
pattern ExtraParamNameEsLegalForm = ExtraParamName' "ES_LEGAL_FORM"

pattern ExtraParamNameFiBusinessNumber :: ExtraParamName
pattern ExtraParamNameFiBusinessNumber = ExtraParamName' "FI_BUSINESS_NUMBER"

pattern ExtraParamNameFiIdNumber :: ExtraParamName
pattern ExtraParamNameFiIdNumber = ExtraParamName' "FI_ID_NUMBER"

pattern ExtraParamNameFiNationality :: ExtraParamName
pattern ExtraParamNameFiNationality = ExtraParamName' "FI_NATIONALITY"

pattern ExtraParamNameFiOrganizationType :: ExtraParamName
pattern ExtraParamNameFiOrganizationType = ExtraParamName' "FI_ORGANIZATION_TYPE"

pattern ExtraParamNameItNationality :: ExtraParamName
pattern ExtraParamNameItNationality = ExtraParamName' "IT_NATIONALITY"

pattern ExtraParamNameItPin :: ExtraParamName
pattern ExtraParamNameItPin = ExtraParamName' "IT_PIN"

pattern ExtraParamNameItRegistrantEntityType :: ExtraParamName
pattern ExtraParamNameItRegistrantEntityType = ExtraParamName' "IT_REGISTRANT_ENTITY_TYPE"

pattern ExtraParamNameRuPassportData :: ExtraParamName
pattern ExtraParamNameRuPassportData = ExtraParamName' "RU_PASSPORT_DATA"

pattern ExtraParamNameSeIdNumber :: ExtraParamName
pattern ExtraParamNameSeIdNumber = ExtraParamName' "SE_ID_NUMBER"

pattern ExtraParamNameSgIdNumber :: ExtraParamName
pattern ExtraParamNameSgIdNumber = ExtraParamName' "SG_ID_NUMBER"

pattern ExtraParamNameVatNumber :: ExtraParamName
pattern ExtraParamNameVatNumber = ExtraParamName' "VAT_NUMBER"

pattern ExtraParamNameUkContactType :: ExtraParamName
pattern ExtraParamNameUkContactType = ExtraParamName' "UK_CONTACT_TYPE"

pattern ExtraParamNameUkCompanyNumber :: ExtraParamName
pattern ExtraParamNameUkCompanyNumber = ExtraParamName' "UK_COMPANY_NUMBER"

{-# COMPLETE
  ExtraParamNameDunsNumber,
  ExtraParamNameBrandNumber,
  ExtraParamNameBirthDepartment,
  ExtraParamNameBirthDateInYyyyMmDd,
  ExtraParamNameBirthCountry,
  ExtraParamNameBirthCity,
  ExtraParamNameDocumentNumber,
  ExtraParamNameAuIdNumber,
  ExtraParamNameAuIdType,
  ExtraParamNameCaLegalType,
  ExtraParamNameCaBusinessEntityType,
  ExtraParamNameCaLegalRepresentative,
  ExtraParamNameCaLegalRepresentativeCapacity,
  ExtraParamNameEsIdentification,
  ExtraParamNameEsIdentificationType,
  ExtraParamNameEsLegalForm,
  ExtraParamNameFiBusinessNumber,
  ExtraParamNameFiIdNumber,
  ExtraParamNameFiNationality,
  ExtraParamNameFiOrganizationType,
  ExtraParamNameItNationality,
  ExtraParamNameItPin,
  ExtraParamNameItRegistrantEntityType,
  ExtraParamNameRuPassportData,
  ExtraParamNameSeIdNumber,
  ExtraParamNameSgIdNumber,
  ExtraParamNameVatNumber,
  ExtraParamNameUkContactType,
  ExtraParamNameUkCompanyNumber,
  ExtraParamName'
  #-}
