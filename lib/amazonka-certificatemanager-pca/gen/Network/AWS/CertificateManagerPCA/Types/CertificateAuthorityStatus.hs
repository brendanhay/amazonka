{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManagerPCA.Types.CertificateAuthorityStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CertificateManagerPCA.Types.CertificateAuthorityStatus where

import Network.AWS.Prelude

data CertificateAuthorityStatus
  = Active
  | Creating
  | Deleted
  | Disabled
  | Expired
  | Failed
  | PendingCertificate
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

instance FromText CertificateAuthorityStatus where
  parser =
    takeLowerText >>= \case
      "active" -> pure Active
      "creating" -> pure Creating
      "deleted" -> pure Deleted
      "disabled" -> pure Disabled
      "expired" -> pure Expired
      "failed" -> pure Failed
      "pending_certificate" -> pure PendingCertificate
      e ->
        fromTextError $
          "Failure parsing CertificateAuthorityStatus from value: '" <> e
            <> "'. Accepted values: active, creating, deleted, disabled, expired, failed, pending_certificate"

instance ToText CertificateAuthorityStatus where
  toText = \case
    Active -> "ACTIVE"
    Creating -> "CREATING"
    Deleted -> "DELETED"
    Disabled -> "DISABLED"
    Expired -> "EXPIRED"
    Failed -> "FAILED"
    PendingCertificate -> "PENDING_CERTIFICATE"

instance Hashable CertificateAuthorityStatus

instance NFData CertificateAuthorityStatus

instance ToByteString CertificateAuthorityStatus

instance ToQuery CertificateAuthorityStatus

instance ToHeader CertificateAuthorityStatus

instance ToJSON CertificateAuthorityStatus where
  toJSON = toJSONText

instance FromJSON CertificateAuthorityStatus where
  parseJSON = parseJSONText "CertificateAuthorityStatus"
