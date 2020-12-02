{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.Types.Sum
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudSearch.Types.Sum where

import Network.AWS.Prelude

data AlgorithmicStemming
  = ASFull
  | ASLight
  | ASMinimal
  | ASNone
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText AlgorithmicStemming where
    parser = takeLowerText >>= \case
        "full" -> pure ASFull
        "light" -> pure ASLight
        "minimal" -> pure ASMinimal
        "none" -> pure ASNone
        e -> fromTextError $ "Failure parsing AlgorithmicStemming from value: '" <> e
           <> "'. Accepted values: full, light, minimal, none"

instance ToText AlgorithmicStemming where
    toText = \case
        ASFull -> "full"
        ASLight -> "light"
        ASMinimal -> "minimal"
        ASNone -> "none"

instance Hashable     AlgorithmicStemming
instance NFData       AlgorithmicStemming
instance ToByteString AlgorithmicStemming
instance ToQuery      AlgorithmicStemming
instance ToHeader     AlgorithmicStemming

instance FromXML AlgorithmicStemming where
    parseXML = parseXMLText "AlgorithmicStemming"

-- | An <http://tools.ietf.org/html/rfc4646 IETF RFC 4646> language code or @mul@ for multiple languages.
--
--
data AnalysisSchemeLanguage
  = AR
  | BG
  | CA
  | CS
  | DA
  | DE
  | EL
  | EN
  | ES
  | EU
  | FA
  | FI
  | FR
  | GA
  | GL
  | HE
  | HI
  | HU
  | HY
  | IT
  | Id
  | JA
  | KO
  | LV
  | Mul
  | NL
  | NO
  | PT
  | RO
  | RU
  | SV
  | TH
  | TR
  | ZhHans
  | ZhHant
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText AnalysisSchemeLanguage where
    parser = takeLowerText >>= \case
        "ar" -> pure AR
        "bg" -> pure BG
        "ca" -> pure CA
        "cs" -> pure CS
        "da" -> pure DA
        "de" -> pure DE
        "el" -> pure EL
        "en" -> pure EN
        "es" -> pure ES
        "eu" -> pure EU
        "fa" -> pure FA
        "fi" -> pure FI
        "fr" -> pure FR
        "ga" -> pure GA
        "gl" -> pure GL
        "he" -> pure HE
        "hi" -> pure HI
        "hu" -> pure HU
        "hy" -> pure HY
        "it" -> pure IT
        "id" -> pure Id
        "ja" -> pure JA
        "ko" -> pure KO
        "lv" -> pure LV
        "mul" -> pure Mul
        "nl" -> pure NL
        "no" -> pure NO
        "pt" -> pure PT
        "ro" -> pure RO
        "ru" -> pure RU
        "sv" -> pure SV
        "th" -> pure TH
        "tr" -> pure TR
        "zh-hans" -> pure ZhHans
        "zh-hant" -> pure ZhHant
        e -> fromTextError $ "Failure parsing AnalysisSchemeLanguage from value: '" <> e
           <> "'. Accepted values: ar, bg, ca, cs, da, de, el, en, es, eu, fa, fi, fr, ga, gl, he, hi, hu, hy, it, id, ja, ko, lv, mul, nl, no, pt, ro, ru, sv, th, tr, zh-hans, zh-hant"

instance ToText AnalysisSchemeLanguage where
    toText = \case
        AR -> "ar"
        BG -> "bg"
        CA -> "ca"
        CS -> "cs"
        DA -> "da"
        DE -> "de"
        EL -> "el"
        EN -> "en"
        ES -> "es"
        EU -> "eu"
        FA -> "fa"
        FI -> "fi"
        FR -> "fr"
        GA -> "ga"
        GL -> "gl"
        HE -> "he"
        HI -> "hi"
        HU -> "hu"
        HY -> "hy"
        IT -> "it"
        Id -> "id"
        JA -> "ja"
        KO -> "ko"
        LV -> "lv"
        Mul -> "mul"
        NL -> "nl"
        NO -> "no"
        PT -> "pt"
        RO -> "ro"
        RU -> "ru"
        SV -> "sv"
        TH -> "th"
        TR -> "tr"
        ZhHans -> "zh-Hans"
        ZhHant -> "zh-Hant"

instance Hashable     AnalysisSchemeLanguage
instance NFData       AnalysisSchemeLanguage
instance ToByteString AnalysisSchemeLanguage
instance ToQuery      AnalysisSchemeLanguage
instance ToHeader     AnalysisSchemeLanguage

instance FromXML AnalysisSchemeLanguage where
    parseXML = parseXMLText "AnalysisSchemeLanguage"

-- | The type of field. The valid options for a field depend on the field type. For more information about the supported field types, see <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/configuring-index-fields.html Configuring Index Fields> in the /Amazon CloudSearch Developer Guide/ .
--
--
data IndexFieldType
  = Date
  | DateArray
  | Double
  | DoubleArray
  | Int
  | IntArray
  | Latlon
  | Literal
  | LiteralArray
  | Text
  | TextArray
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText IndexFieldType where
    parser = takeLowerText >>= \case
        "date" -> pure Date
        "date-array" -> pure DateArray
        "double" -> pure Double
        "double-array" -> pure DoubleArray
        "int" -> pure Int
        "int-array" -> pure IntArray
        "latlon" -> pure Latlon
        "literal" -> pure Literal
        "literal-array" -> pure LiteralArray
        "text" -> pure Text
        "text-array" -> pure TextArray
        e -> fromTextError $ "Failure parsing IndexFieldType from value: '" <> e
           <> "'. Accepted values: date, date-array, double, double-array, int, int-array, latlon, literal, literal-array, text, text-array"

instance ToText IndexFieldType where
    toText = \case
        Date -> "date"
        DateArray -> "date-array"
        Double -> "double"
        DoubleArray -> "double-array"
        Int -> "int"
        IntArray -> "int-array"
        Latlon -> "latlon"
        Literal -> "literal"
        LiteralArray -> "literal-array"
        Text -> "text"
        TextArray -> "text-array"

instance Hashable     IndexFieldType
instance NFData       IndexFieldType
instance ToByteString IndexFieldType
instance ToQuery      IndexFieldType
instance ToHeader     IndexFieldType

instance FromXML IndexFieldType where
    parseXML = parseXMLText "IndexFieldType"

-- | The state of processing a change to an option. One of:
--
--
--     * RequiresIndexDocuments: The option's latest value will not be deployed until 'IndexDocuments' has been called and indexing is complete.    * Processing: The option's latest value is in the process of being activated.    * Active: The option's latest value is fully deployed.     * FailedToValidate: The option value is not compatible with the domain's data and cannot be used to index the data. You must either modify the option value or update or remove the incompatible documents.
--
data OptionState
  = Active
  | FailedToValidate
  | Processing
  | RequiresIndexDocuments
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText OptionState where
    parser = takeLowerText >>= \case
        "active" -> pure Active
        "failedtovalidate" -> pure FailedToValidate
        "processing" -> pure Processing
        "requiresindexdocuments" -> pure RequiresIndexDocuments
        e -> fromTextError $ "Failure parsing OptionState from value: '" <> e
           <> "'. Accepted values: active, failedtovalidate, processing, requiresindexdocuments"

instance ToText OptionState where
    toText = \case
        Active -> "Active"
        FailedToValidate -> "FailedToValidate"
        Processing -> "Processing"
        RequiresIndexDocuments -> "RequiresIndexDocuments"

instance Hashable     OptionState
instance NFData       OptionState
instance ToByteString OptionState
instance ToQuery      OptionState
instance ToHeader     OptionState

instance FromXML OptionState where
    parseXML = parseXMLText "OptionState"

-- | The instance type (such as @search.m1.small@ ) on which an index partition is hosted.
--
--
data PartitionInstanceType
  = Search_M1_Large
  | Search_M1_Small
  | Search_M2_2XLarge
  | Search_M2_XLarge
  | Search_M3_2XLarge
  | Search_M3_Large
  | Search_M3_Medium
  | Search_M3_XLarge
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText PartitionInstanceType where
    parser = takeLowerText >>= \case
        "search.m1.large" -> pure Search_M1_Large
        "search.m1.small" -> pure Search_M1_Small
        "search.m2.2xlarge" -> pure Search_M2_2XLarge
        "search.m2.xlarge" -> pure Search_M2_XLarge
        "search.m3.2xlarge" -> pure Search_M3_2XLarge
        "search.m3.large" -> pure Search_M3_Large
        "search.m3.medium" -> pure Search_M3_Medium
        "search.m3.xlarge" -> pure Search_M3_XLarge
        e -> fromTextError $ "Failure parsing PartitionInstanceType from value: '" <> e
           <> "'. Accepted values: search.m1.large, search.m1.small, search.m2.2xlarge, search.m2.xlarge, search.m3.2xlarge, search.m3.large, search.m3.medium, search.m3.xlarge"

instance ToText PartitionInstanceType where
    toText = \case
        Search_M1_Large -> "search.m1.large"
        Search_M1_Small -> "search.m1.small"
        Search_M2_2XLarge -> "search.m2.2xlarge"
        Search_M2_XLarge -> "search.m2.xlarge"
        Search_M3_2XLarge -> "search.m3.2xlarge"
        Search_M3_Large -> "search.m3.large"
        Search_M3_Medium -> "search.m3.medium"
        Search_M3_XLarge -> "search.m3.xlarge"

instance Hashable     PartitionInstanceType
instance NFData       PartitionInstanceType
instance ToByteString PartitionInstanceType
instance ToQuery      PartitionInstanceType
instance ToHeader     PartitionInstanceType

instance FromXML PartitionInstanceType where
    parseXML = parseXMLText "PartitionInstanceType"

data SuggesterFuzzyMatching
  = High
  | Low
  | None
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText SuggesterFuzzyMatching where
    parser = takeLowerText >>= \case
        "high" -> pure High
        "low" -> pure Low
        "none" -> pure None
        e -> fromTextError $ "Failure parsing SuggesterFuzzyMatching from value: '" <> e
           <> "'. Accepted values: high, low, none"

instance ToText SuggesterFuzzyMatching where
    toText = \case
        High -> "high"
        Low -> "low"
        None -> "none"

instance Hashable     SuggesterFuzzyMatching
instance NFData       SuggesterFuzzyMatching
instance ToByteString SuggesterFuzzyMatching
instance ToQuery      SuggesterFuzzyMatching
instance ToHeader     SuggesterFuzzyMatching

instance FromXML SuggesterFuzzyMatching where
    parseXML = parseXMLText "SuggesterFuzzyMatching"
