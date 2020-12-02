{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManager.Types.CertificateType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CertificateManager.Types.CertificateType where

import Network.AWS.Prelude

data CertificateType
  = AmazonIssued
  | Imported
  | Private
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

instance FromText CertificateType where
  parser =
    takeLowerText >>= \case
      "amazon_issued" -> pure AmazonIssued
      "imported" -> pure Imported
      "private" -> pure Private
      e ->
        fromTextError $
          "Failure parsing CertificateType from value: '" <> e
            <> "'. Accepted values: amazon_issued, imported, private"

instance ToText CertificateType where
  toText = \case
    AmazonIssued -> "AMAZON_ISSUED"
    Imported -> "IMPORTED"
    Private -> "PRIVATE"

instance Hashable CertificateType

instance NFData CertificateType

instance ToByteString CertificateType

instance ToQuery CertificateType

instance ToHeader CertificateType

instance FromJSON CertificateType where
  parseJSON = parseJSONText "CertificateType"
