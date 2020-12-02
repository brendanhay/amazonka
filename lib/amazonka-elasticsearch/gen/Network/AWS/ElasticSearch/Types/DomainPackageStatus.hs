{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.DomainPackageStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.DomainPackageStatus where

import Network.AWS.Prelude

data DomainPackageStatus
  = DPSActive
  | DPSAssociating
  | DPSAssociationFailed
  | DPSDissociating
  | DPSDissociationFailed
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

instance FromText DomainPackageStatus where
  parser =
    takeLowerText >>= \case
      "active" -> pure DPSActive
      "associating" -> pure DPSAssociating
      "association_failed" -> pure DPSAssociationFailed
      "dissociating" -> pure DPSDissociating
      "dissociation_failed" -> pure DPSDissociationFailed
      e ->
        fromTextError $
          "Failure parsing DomainPackageStatus from value: '" <> e
            <> "'. Accepted values: active, associating, association_failed, dissociating, dissociation_failed"

instance ToText DomainPackageStatus where
  toText = \case
    DPSActive -> "ACTIVE"
    DPSAssociating -> "ASSOCIATING"
    DPSAssociationFailed -> "ASSOCIATION_FAILED"
    DPSDissociating -> "DISSOCIATING"
    DPSDissociationFailed -> "DISSOCIATION_FAILED"

instance Hashable DomainPackageStatus

instance NFData DomainPackageStatus

instance ToByteString DomainPackageStatus

instance ToQuery DomainPackageStatus

instance ToHeader DomainPackageStatus

instance FromJSON DomainPackageStatus where
  parseJSON = parseJSONText "DomainPackageStatus"
