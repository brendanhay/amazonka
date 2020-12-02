{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.PatchFilterKey
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.PatchFilterKey where

import Network.AWS.Prelude

data PatchFilterKey
  = AdvisoryId
  | Arch
  | BugzillaId
  | Classification
  | CveId
  | Epoch
  | MsrcSeverity
  | Name
  | PatchId
  | PatchSet
  | Priority
  | Product
  | ProductFamily
  | Release
  | Repository
  | Section
  | Security
  | Severity
  | Version
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

instance FromText PatchFilterKey where
  parser =
    takeLowerText >>= \case
      "advisory_id" -> pure AdvisoryId
      "arch" -> pure Arch
      "bugzilla_id" -> pure BugzillaId
      "classification" -> pure Classification
      "cve_id" -> pure CveId
      "epoch" -> pure Epoch
      "msrc_severity" -> pure MsrcSeverity
      "name" -> pure Name
      "patch_id" -> pure PatchId
      "patch_set" -> pure PatchSet
      "priority" -> pure Priority
      "product" -> pure Product
      "product_family" -> pure ProductFamily
      "release" -> pure Release
      "repository" -> pure Repository
      "section" -> pure Section
      "security" -> pure Security
      "severity" -> pure Severity
      "version" -> pure Version
      e ->
        fromTextError $
          "Failure parsing PatchFilterKey from value: '" <> e
            <> "'. Accepted values: advisory_id, arch, bugzilla_id, classification, cve_id, epoch, msrc_severity, name, patch_id, patch_set, priority, product, product_family, release, repository, section, security, severity, version"

instance ToText PatchFilterKey where
  toText = \case
    AdvisoryId -> "ADVISORY_ID"
    Arch -> "ARCH"
    BugzillaId -> "BUGZILLA_ID"
    Classification -> "CLASSIFICATION"
    CveId -> "CVE_ID"
    Epoch -> "EPOCH"
    MsrcSeverity -> "MSRC_SEVERITY"
    Name -> "NAME"
    PatchId -> "PATCH_ID"
    PatchSet -> "PATCH_SET"
    Priority -> "PRIORITY"
    Product -> "PRODUCT"
    ProductFamily -> "PRODUCT_FAMILY"
    Release -> "RELEASE"
    Repository -> "REPOSITORY"
    Section -> "SECTION"
    Security -> "SECURITY"
    Severity -> "SEVERITY"
    Version -> "VERSION"

instance Hashable PatchFilterKey

instance NFData PatchFilterKey

instance ToByteString PatchFilterKey

instance ToQuery PatchFilterKey

instance ToHeader PatchFilterKey

instance ToJSON PatchFilterKey where
  toJSON = toJSONText

instance FromJSON PatchFilterKey where
  parseJSON = parseJSONText "PatchFilterKey"
