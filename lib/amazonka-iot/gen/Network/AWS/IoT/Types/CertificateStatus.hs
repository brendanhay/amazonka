{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.CertificateStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.CertificateStatus where

import Network.AWS.Prelude

data CertificateStatus
  = CSActive
  | CSInactive
  | CSPendingActivation
  | CSPendingTransfer
  | CSRegisterInactive
  | CSRevoked
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

instance FromText CertificateStatus where
  parser =
    takeLowerText >>= \case
      "active" -> pure CSActive
      "inactive" -> pure CSInactive
      "pending_activation" -> pure CSPendingActivation
      "pending_transfer" -> pure CSPendingTransfer
      "register_inactive" -> pure CSRegisterInactive
      "revoked" -> pure CSRevoked
      e ->
        fromTextError $
          "Failure parsing CertificateStatus from value: '" <> e
            <> "'. Accepted values: active, inactive, pending_activation, pending_transfer, register_inactive, revoked"

instance ToText CertificateStatus where
  toText = \case
    CSActive -> "ACTIVE"
    CSInactive -> "INACTIVE"
    CSPendingActivation -> "PENDING_ACTIVATION"
    CSPendingTransfer -> "PENDING_TRANSFER"
    CSRegisterInactive -> "REGISTER_INACTIVE"
    CSRevoked -> "REVOKED"

instance Hashable CertificateStatus

instance NFData CertificateStatus

instance ToByteString CertificateStatus

instance ToQuery CertificateStatus

instance ToHeader CertificateStatus

instance ToJSON CertificateStatus where
  toJSON = toJSONText

instance FromJSON CertificateStatus where
  parseJSON = parseJSONText "CertificateStatus"
