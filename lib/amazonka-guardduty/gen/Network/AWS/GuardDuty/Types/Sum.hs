{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.Sum
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.GuardDuty.Types.Sum where

import Network.AWS.Prelude

-- | The status of detector.
data DetectorStatus
  = Disabled
  | Enabled
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText DetectorStatus where
    parser = takeLowerText >>= \case
        "disabled" -> pure Disabled
        "enabled" -> pure Enabled
        e -> fromTextError $ "Failure parsing DetectorStatus from value: '" <> e
           <> "'. Accepted values: disabled, enabled"

instance ToText DetectorStatus where
    toText = \case
        Disabled -> "DISABLED"
        Enabled -> "ENABLED"

instance Hashable     DetectorStatus
instance NFData       DetectorStatus
instance ToByteString DetectorStatus
instance ToQuery      DetectorStatus
instance ToHeader     DetectorStatus

instance FromJSON DetectorStatus where
    parseJSON = parseJSONText "DetectorStatus"

-- | Finding Feedback Value
data Feedback
  = NotUseful
  | Useful
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText Feedback where
    parser = takeLowerText >>= \case
        "not_useful" -> pure NotUseful
        "useful" -> pure Useful
        e -> fromTextError $ "Failure parsing Feedback from value: '" <> e
           <> "'. Accepted values: not_useful, useful"

instance ToText Feedback where
    toText = \case
        NotUseful -> "NOT_USEFUL"
        Useful -> "USEFUL"

instance Hashable     Feedback
instance NFData       Feedback
instance ToByteString Feedback
instance ToQuery      Feedback
instance ToHeader     Feedback

instance ToJSON Feedback where
    toJSON = toJSONText

-- | The action associated with a filter.
data FilterAction
  = Archive
  | Noop
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText FilterAction where
    parser = takeLowerText >>= \case
        "archive" -> pure Archive
        "noop" -> pure Noop
        e -> fromTextError $ "Failure parsing FilterAction from value: '" <> e
           <> "'. Accepted values: archive, noop"

instance ToText FilterAction where
    toText = \case
        Archive -> "ARCHIVE"
        Noop -> "NOOP"

instance Hashable     FilterAction
instance NFData       FilterAction
instance ToByteString FilterAction
instance ToQuery      FilterAction
instance ToHeader     FilterAction

instance ToJSON FilterAction where
    toJSON = toJSONText

instance FromJSON FilterAction where
    parseJSON = parseJSONText "FilterAction"

-- | The types of finding statistics.
data FindingStatisticType =
  CountBySeverity
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText FindingStatisticType where
    parser = takeLowerText >>= \case
        "count_by_severity" -> pure CountBySeverity
        e -> fromTextError $ "Failure parsing FindingStatisticType from value: '" <> e
           <> "'. Accepted values: count_by_severity"

instance ToText FindingStatisticType where
    toText = \case
        CountBySeverity -> "COUNT_BY_SEVERITY"

instance Hashable     FindingStatisticType
instance NFData       FindingStatisticType
instance ToByteString FindingStatisticType
instance ToQuery      FindingStatisticType
instance ToHeader     FindingStatisticType

instance ToJSON FindingStatisticType where
    toJSON = toJSONText

-- | The format of the ipSet.
data IPSetFormat
  = AlienVault
  | FireEye
  | OtxCSV
  | ProofPoint
  | Stix
  | Txt
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText IPSetFormat where
    parser = takeLowerText >>= \case
        "alien_vault" -> pure AlienVault
        "fire_eye" -> pure FireEye
        "otx_csv" -> pure OtxCSV
        "proof_point" -> pure ProofPoint
        "stix" -> pure Stix
        "txt" -> pure Txt
        e -> fromTextError $ "Failure parsing IPSetFormat from value: '" <> e
           <> "'. Accepted values: alien_vault, fire_eye, otx_csv, proof_point, stix, txt"

instance ToText IPSetFormat where
    toText = \case
        AlienVault -> "ALIEN_VAULT"
        FireEye -> "FIRE_EYE"
        OtxCSV -> "OTX_CSV"
        ProofPoint -> "PROOF_POINT"
        Stix -> "STIX"
        Txt -> "TXT"

instance Hashable     IPSetFormat
instance NFData       IPSetFormat
instance ToByteString IPSetFormat
instance ToQuery      IPSetFormat
instance ToHeader     IPSetFormat

instance ToJSON IPSetFormat where
    toJSON = toJSONText

instance FromJSON IPSetFormat where
    parseJSON = parseJSONText "IPSetFormat"

-- | The status of ipSet file uploaded.
data IPSetStatus
  = ISSActivating
  | ISSActive
  | ISSDeactivating
  | ISSDeletePending
  | ISSDeleted
  | ISSError'
  | ISSInactive
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText IPSetStatus where
    parser = takeLowerText >>= \case
        "activating" -> pure ISSActivating
        "active" -> pure ISSActive
        "deactivating" -> pure ISSDeactivating
        "delete_pending" -> pure ISSDeletePending
        "deleted" -> pure ISSDeleted
        "error" -> pure ISSError'
        "inactive" -> pure ISSInactive
        e -> fromTextError $ "Failure parsing IPSetStatus from value: '" <> e
           <> "'. Accepted values: activating, active, deactivating, delete_pending, deleted, error, inactive"

instance ToText IPSetStatus where
    toText = \case
        ISSActivating -> "ACTIVATING"
        ISSActive -> "ACTIVE"
        ISSDeactivating -> "DEACTIVATING"
        ISSDeletePending -> "DELETE_PENDING"
        ISSDeleted -> "DELETED"
        ISSError' -> "ERROR"
        ISSInactive -> "INACTIVE"

instance Hashable     IPSetStatus
instance NFData       IPSetStatus
instance ToByteString IPSetStatus
instance ToQuery      IPSetStatus
instance ToHeader     IPSetStatus

instance FromJSON IPSetStatus where
    parseJSON = parseJSONText "IPSetStatus"

data OrderBy
  = Asc
  | Desc
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText OrderBy where
    parser = takeLowerText >>= \case
        "asc" -> pure Asc
        "desc" -> pure Desc
        e -> fromTextError $ "Failure parsing OrderBy from value: '" <> e
           <> "'. Accepted values: asc, desc"

instance ToText OrderBy where
    toText = \case
        Asc -> "ASC"
        Desc -> "DESC"

instance Hashable     OrderBy
instance NFData       OrderBy
instance ToByteString OrderBy
instance ToQuery      OrderBy
instance ToHeader     OrderBy

instance ToJSON OrderBy where
    toJSON = toJSONText

-- | The format of the threatIntelSet.
data ThreatIntelSetFormat
  = TISFAlienVault
  | TISFFireEye
  | TISFOtxCSV
  | TISFProofPoint
  | TISFStix
  | TISFTxt
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ThreatIntelSetFormat where
    parser = takeLowerText >>= \case
        "alien_vault" -> pure TISFAlienVault
        "fire_eye" -> pure TISFFireEye
        "otx_csv" -> pure TISFOtxCSV
        "proof_point" -> pure TISFProofPoint
        "stix" -> pure TISFStix
        "txt" -> pure TISFTxt
        e -> fromTextError $ "Failure parsing ThreatIntelSetFormat from value: '" <> e
           <> "'. Accepted values: alien_vault, fire_eye, otx_csv, proof_point, stix, txt"

instance ToText ThreatIntelSetFormat where
    toText = \case
        TISFAlienVault -> "ALIEN_VAULT"
        TISFFireEye -> "FIRE_EYE"
        TISFOtxCSV -> "OTX_CSV"
        TISFProofPoint -> "PROOF_POINT"
        TISFStix -> "STIX"
        TISFTxt -> "TXT"

instance Hashable     ThreatIntelSetFormat
instance NFData       ThreatIntelSetFormat
instance ToByteString ThreatIntelSetFormat
instance ToQuery      ThreatIntelSetFormat
instance ToHeader     ThreatIntelSetFormat

instance ToJSON ThreatIntelSetFormat where
    toJSON = toJSONText

instance FromJSON ThreatIntelSetFormat where
    parseJSON = parseJSONText "ThreatIntelSetFormat"

-- | The status of threatIntelSet file uploaded.
data ThreatIntelSetStatus
  = Activating
  | Active
  | Deactivating
  | DeletePending
  | Deleted
  | Error'
  | Inactive
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ThreatIntelSetStatus where
    parser = takeLowerText >>= \case
        "activating" -> pure Activating
        "active" -> pure Active
        "deactivating" -> pure Deactivating
        "delete_pending" -> pure DeletePending
        "deleted" -> pure Deleted
        "error" -> pure Error'
        "inactive" -> pure Inactive
        e -> fromTextError $ "Failure parsing ThreatIntelSetStatus from value: '" <> e
           <> "'. Accepted values: activating, active, deactivating, delete_pending, deleted, error, inactive"

instance ToText ThreatIntelSetStatus where
    toText = \case
        Activating -> "ACTIVATING"
        Active -> "ACTIVE"
        Deactivating -> "DEACTIVATING"
        DeletePending -> "DELETE_PENDING"
        Deleted -> "DELETED"
        Error' -> "ERROR"
        Inactive -> "INACTIVE"

instance Hashable     ThreatIntelSetStatus
instance NFData       ThreatIntelSetStatus
instance ToByteString ThreatIntelSetStatus
instance ToQuery      ThreatIntelSetStatus
instance ToHeader     ThreatIntelSetStatus

instance FromJSON ThreatIntelSetStatus where
    parseJSON = parseJSONText "ThreatIntelSetStatus"
