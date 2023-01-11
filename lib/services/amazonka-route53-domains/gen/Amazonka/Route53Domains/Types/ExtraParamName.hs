{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Route53Domains.Types.ExtraParamName
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53Domains.Types.ExtraParamName
  ( ExtraParamName
      ( ..,
        ExtraParamName_AU_ID_NUMBER,
        ExtraParamName_AU_ID_TYPE,
        ExtraParamName_AU_PRIORITY_TOKEN,
        ExtraParamName_BIRTH_CITY,
        ExtraParamName_BIRTH_COUNTRY,
        ExtraParamName_BIRTH_DATE_IN_YYYY_MM_DD,
        ExtraParamName_BIRTH_DEPARTMENT,
        ExtraParamName_BRAND_NUMBER,
        ExtraParamName_CA_BUSINESS_ENTITY_TYPE,
        ExtraParamName_CA_LEGAL_REPRESENTATIVE,
        ExtraParamName_CA_LEGAL_REPRESENTATIVE_CAPACITY,
        ExtraParamName_CA_LEGAL_TYPE,
        ExtraParamName_DOCUMENT_NUMBER,
        ExtraParamName_DUNS_NUMBER,
        ExtraParamName_ES_IDENTIFICATION,
        ExtraParamName_ES_IDENTIFICATION_TYPE,
        ExtraParamName_ES_LEGAL_FORM,
        ExtraParamName_EU_COUNTRY_OF_CITIZENSHIP,
        ExtraParamName_FI_BUSINESS_NUMBER,
        ExtraParamName_FI_ID_NUMBER,
        ExtraParamName_FI_NATIONALITY,
        ExtraParamName_FI_ORGANIZATION_TYPE,
        ExtraParamName_IT_NATIONALITY,
        ExtraParamName_IT_PIN,
        ExtraParamName_IT_REGISTRANT_ENTITY_TYPE,
        ExtraParamName_RU_PASSPORT_DATA,
        ExtraParamName_SE_ID_NUMBER,
        ExtraParamName_SG_ID_NUMBER,
        ExtraParamName_UK_COMPANY_NUMBER,
        ExtraParamName_UK_CONTACT_TYPE,
        ExtraParamName_VAT_NUMBER
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ExtraParamName = ExtraParamName'
  { fromExtraParamName ::
      Data.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
    )

pattern ExtraParamName_AU_ID_NUMBER :: ExtraParamName
pattern ExtraParamName_AU_ID_NUMBER = ExtraParamName' "AU_ID_NUMBER"

pattern ExtraParamName_AU_ID_TYPE :: ExtraParamName
pattern ExtraParamName_AU_ID_TYPE = ExtraParamName' "AU_ID_TYPE"

pattern ExtraParamName_AU_PRIORITY_TOKEN :: ExtraParamName
pattern ExtraParamName_AU_PRIORITY_TOKEN = ExtraParamName' "AU_PRIORITY_TOKEN"

pattern ExtraParamName_BIRTH_CITY :: ExtraParamName
pattern ExtraParamName_BIRTH_CITY = ExtraParamName' "BIRTH_CITY"

pattern ExtraParamName_BIRTH_COUNTRY :: ExtraParamName
pattern ExtraParamName_BIRTH_COUNTRY = ExtraParamName' "BIRTH_COUNTRY"

pattern ExtraParamName_BIRTH_DATE_IN_YYYY_MM_DD :: ExtraParamName
pattern ExtraParamName_BIRTH_DATE_IN_YYYY_MM_DD = ExtraParamName' "BIRTH_DATE_IN_YYYY_MM_DD"

pattern ExtraParamName_BIRTH_DEPARTMENT :: ExtraParamName
pattern ExtraParamName_BIRTH_DEPARTMENT = ExtraParamName' "BIRTH_DEPARTMENT"

pattern ExtraParamName_BRAND_NUMBER :: ExtraParamName
pattern ExtraParamName_BRAND_NUMBER = ExtraParamName' "BRAND_NUMBER"

pattern ExtraParamName_CA_BUSINESS_ENTITY_TYPE :: ExtraParamName
pattern ExtraParamName_CA_BUSINESS_ENTITY_TYPE = ExtraParamName' "CA_BUSINESS_ENTITY_TYPE"

pattern ExtraParamName_CA_LEGAL_REPRESENTATIVE :: ExtraParamName
pattern ExtraParamName_CA_LEGAL_REPRESENTATIVE = ExtraParamName' "CA_LEGAL_REPRESENTATIVE"

pattern ExtraParamName_CA_LEGAL_REPRESENTATIVE_CAPACITY :: ExtraParamName
pattern ExtraParamName_CA_LEGAL_REPRESENTATIVE_CAPACITY = ExtraParamName' "CA_LEGAL_REPRESENTATIVE_CAPACITY"

pattern ExtraParamName_CA_LEGAL_TYPE :: ExtraParamName
pattern ExtraParamName_CA_LEGAL_TYPE = ExtraParamName' "CA_LEGAL_TYPE"

pattern ExtraParamName_DOCUMENT_NUMBER :: ExtraParamName
pattern ExtraParamName_DOCUMENT_NUMBER = ExtraParamName' "DOCUMENT_NUMBER"

pattern ExtraParamName_DUNS_NUMBER :: ExtraParamName
pattern ExtraParamName_DUNS_NUMBER = ExtraParamName' "DUNS_NUMBER"

pattern ExtraParamName_ES_IDENTIFICATION :: ExtraParamName
pattern ExtraParamName_ES_IDENTIFICATION = ExtraParamName' "ES_IDENTIFICATION"

pattern ExtraParamName_ES_IDENTIFICATION_TYPE :: ExtraParamName
pattern ExtraParamName_ES_IDENTIFICATION_TYPE = ExtraParamName' "ES_IDENTIFICATION_TYPE"

pattern ExtraParamName_ES_LEGAL_FORM :: ExtraParamName
pattern ExtraParamName_ES_LEGAL_FORM = ExtraParamName' "ES_LEGAL_FORM"

pattern ExtraParamName_EU_COUNTRY_OF_CITIZENSHIP :: ExtraParamName
pattern ExtraParamName_EU_COUNTRY_OF_CITIZENSHIP = ExtraParamName' "EU_COUNTRY_OF_CITIZENSHIP"

pattern ExtraParamName_FI_BUSINESS_NUMBER :: ExtraParamName
pattern ExtraParamName_FI_BUSINESS_NUMBER = ExtraParamName' "FI_BUSINESS_NUMBER"

pattern ExtraParamName_FI_ID_NUMBER :: ExtraParamName
pattern ExtraParamName_FI_ID_NUMBER = ExtraParamName' "FI_ID_NUMBER"

pattern ExtraParamName_FI_NATIONALITY :: ExtraParamName
pattern ExtraParamName_FI_NATIONALITY = ExtraParamName' "FI_NATIONALITY"

pattern ExtraParamName_FI_ORGANIZATION_TYPE :: ExtraParamName
pattern ExtraParamName_FI_ORGANIZATION_TYPE = ExtraParamName' "FI_ORGANIZATION_TYPE"

pattern ExtraParamName_IT_NATIONALITY :: ExtraParamName
pattern ExtraParamName_IT_NATIONALITY = ExtraParamName' "IT_NATIONALITY"

pattern ExtraParamName_IT_PIN :: ExtraParamName
pattern ExtraParamName_IT_PIN = ExtraParamName' "IT_PIN"

pattern ExtraParamName_IT_REGISTRANT_ENTITY_TYPE :: ExtraParamName
pattern ExtraParamName_IT_REGISTRANT_ENTITY_TYPE = ExtraParamName' "IT_REGISTRANT_ENTITY_TYPE"

pattern ExtraParamName_RU_PASSPORT_DATA :: ExtraParamName
pattern ExtraParamName_RU_PASSPORT_DATA = ExtraParamName' "RU_PASSPORT_DATA"

pattern ExtraParamName_SE_ID_NUMBER :: ExtraParamName
pattern ExtraParamName_SE_ID_NUMBER = ExtraParamName' "SE_ID_NUMBER"

pattern ExtraParamName_SG_ID_NUMBER :: ExtraParamName
pattern ExtraParamName_SG_ID_NUMBER = ExtraParamName' "SG_ID_NUMBER"

pattern ExtraParamName_UK_COMPANY_NUMBER :: ExtraParamName
pattern ExtraParamName_UK_COMPANY_NUMBER = ExtraParamName' "UK_COMPANY_NUMBER"

pattern ExtraParamName_UK_CONTACT_TYPE :: ExtraParamName
pattern ExtraParamName_UK_CONTACT_TYPE = ExtraParamName' "UK_CONTACT_TYPE"

pattern ExtraParamName_VAT_NUMBER :: ExtraParamName
pattern ExtraParamName_VAT_NUMBER = ExtraParamName' "VAT_NUMBER"

{-# COMPLETE
  ExtraParamName_AU_ID_NUMBER,
  ExtraParamName_AU_ID_TYPE,
  ExtraParamName_AU_PRIORITY_TOKEN,
  ExtraParamName_BIRTH_CITY,
  ExtraParamName_BIRTH_COUNTRY,
  ExtraParamName_BIRTH_DATE_IN_YYYY_MM_DD,
  ExtraParamName_BIRTH_DEPARTMENT,
  ExtraParamName_BRAND_NUMBER,
  ExtraParamName_CA_BUSINESS_ENTITY_TYPE,
  ExtraParamName_CA_LEGAL_REPRESENTATIVE,
  ExtraParamName_CA_LEGAL_REPRESENTATIVE_CAPACITY,
  ExtraParamName_CA_LEGAL_TYPE,
  ExtraParamName_DOCUMENT_NUMBER,
  ExtraParamName_DUNS_NUMBER,
  ExtraParamName_ES_IDENTIFICATION,
  ExtraParamName_ES_IDENTIFICATION_TYPE,
  ExtraParamName_ES_LEGAL_FORM,
  ExtraParamName_EU_COUNTRY_OF_CITIZENSHIP,
  ExtraParamName_FI_BUSINESS_NUMBER,
  ExtraParamName_FI_ID_NUMBER,
  ExtraParamName_FI_NATIONALITY,
  ExtraParamName_FI_ORGANIZATION_TYPE,
  ExtraParamName_IT_NATIONALITY,
  ExtraParamName_IT_PIN,
  ExtraParamName_IT_REGISTRANT_ENTITY_TYPE,
  ExtraParamName_RU_PASSPORT_DATA,
  ExtraParamName_SE_ID_NUMBER,
  ExtraParamName_SG_ID_NUMBER,
  ExtraParamName_UK_COMPANY_NUMBER,
  ExtraParamName_UK_CONTACT_TYPE,
  ExtraParamName_VAT_NUMBER,
  ExtraParamName'
  #-}
