{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.LoadBalancerTLSCertificateDomainStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.LoadBalancerTLSCertificateDomainStatus where

import Network.AWS.Prelude

data LoadBalancerTLSCertificateDomainStatus
  = LBTCDSFailed
  | LBTCDSPendingValidation
  | LBTCDSSuccess
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

instance FromText LoadBalancerTLSCertificateDomainStatus where
  parser =
    takeLowerText >>= \case
      "failed" -> pure LBTCDSFailed
      "pending_validation" -> pure LBTCDSPendingValidation
      "success" -> pure LBTCDSSuccess
      e ->
        fromTextError $
          "Failure parsing LoadBalancerTLSCertificateDomainStatus from value: '" <> e
            <> "'. Accepted values: failed, pending_validation, success"

instance ToText LoadBalancerTLSCertificateDomainStatus where
  toText = \case
    LBTCDSFailed -> "FAILED"
    LBTCDSPendingValidation -> "PENDING_VALIDATION"
    LBTCDSSuccess -> "SUCCESS"

instance Hashable LoadBalancerTLSCertificateDomainStatus

instance NFData LoadBalancerTLSCertificateDomainStatus

instance ToByteString LoadBalancerTLSCertificateDomainStatus

instance ToQuery LoadBalancerTLSCertificateDomainStatus

instance ToHeader LoadBalancerTLSCertificateDomainStatus

instance FromJSON LoadBalancerTLSCertificateDomainStatus where
  parseJSON = parseJSONText "LoadBalancerTLSCertificateDomainStatus"
