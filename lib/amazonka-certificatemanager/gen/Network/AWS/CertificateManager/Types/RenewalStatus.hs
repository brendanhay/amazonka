{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManager.Types.RenewalStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CertificateManager.Types.RenewalStatus where

import Network.AWS.Prelude

data RenewalStatus
  = RSFailed
  | RSPendingAutoRenewal
  | RSPendingValidation
  | RSSuccess
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

instance FromText RenewalStatus where
  parser =
    takeLowerText >>= \case
      "failed" -> pure RSFailed
      "pending_auto_renewal" -> pure RSPendingAutoRenewal
      "pending_validation" -> pure RSPendingValidation
      "success" -> pure RSSuccess
      e ->
        fromTextError $
          "Failure parsing RenewalStatus from value: '" <> e
            <> "'. Accepted values: failed, pending_auto_renewal, pending_validation, success"

instance ToText RenewalStatus where
  toText = \case
    RSFailed -> "FAILED"
    RSPendingAutoRenewal -> "PENDING_AUTO_RENEWAL"
    RSPendingValidation -> "PENDING_VALIDATION"
    RSSuccess -> "SUCCESS"

instance Hashable RenewalStatus

instance NFData RenewalStatus

instance ToByteString RenewalStatus

instance ToQuery RenewalStatus

instance ToHeader RenewalStatus

instance FromJSON RenewalStatus where
  parseJSON = parseJSONText "RenewalStatus"
