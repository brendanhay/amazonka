{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManager.Types.DomainStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CertificateManager.Types.DomainStatus where

import Network.AWS.Prelude

data DomainStatus
  = Failed
  | PendingValidation
  | Success
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

instance FromText DomainStatus where
  parser =
    takeLowerText >>= \case
      "failed" -> pure Failed
      "pending_validation" -> pure PendingValidation
      "success" -> pure Success
      e ->
        fromTextError $
          "Failure parsing DomainStatus from value: '" <> e
            <> "'. Accepted values: failed, pending_validation, success"

instance ToText DomainStatus where
  toText = \case
    Failed -> "FAILED"
    PendingValidation -> "PENDING_VALIDATION"
    Success -> "SUCCESS"

instance Hashable DomainStatus

instance NFData DomainStatus

instance ToByteString DomainStatus

instance ToQuery DomainStatus

instance ToHeader DomainStatus

instance FromJSON DomainStatus where
  parseJSON = parseJSONText "DomainStatus"
