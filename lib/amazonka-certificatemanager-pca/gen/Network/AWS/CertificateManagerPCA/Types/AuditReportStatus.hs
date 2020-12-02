{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManagerPCA.Types.AuditReportStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CertificateManagerPCA.Types.AuditReportStatus where

import Network.AWS.Prelude

data AuditReportStatus
  = ARSCreating
  | ARSFailed
  | ARSSuccess
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

instance FromText AuditReportStatus where
  parser =
    takeLowerText >>= \case
      "creating" -> pure ARSCreating
      "failed" -> pure ARSFailed
      "success" -> pure ARSSuccess
      e ->
        fromTextError $
          "Failure parsing AuditReportStatus from value: '" <> e
            <> "'. Accepted values: creating, failed, success"

instance ToText AuditReportStatus where
  toText = \case
    ARSCreating -> "CREATING"
    ARSFailed -> "FAILED"
    ARSSuccess -> "SUCCESS"

instance Hashable AuditReportStatus

instance NFData AuditReportStatus

instance ToByteString AuditReportStatus

instance ToQuery AuditReportStatus

instance ToHeader AuditReportStatus

instance FromJSON AuditReportStatus where
  parseJSON = parseJSONText "AuditReportStatus"
