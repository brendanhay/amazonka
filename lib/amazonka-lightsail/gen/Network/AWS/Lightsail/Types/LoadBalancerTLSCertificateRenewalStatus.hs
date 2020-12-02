{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.LoadBalancerTLSCertificateRenewalStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.LoadBalancerTLSCertificateRenewalStatus where

import Network.AWS.Prelude

data LoadBalancerTLSCertificateRenewalStatus
  = LBTCRSFailed
  | LBTCRSPendingAutoRenewal
  | LBTCRSPendingValidation
  | LBTCRSSuccess
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

instance FromText LoadBalancerTLSCertificateRenewalStatus where
  parser =
    takeLowerText >>= \case
      "failed" -> pure LBTCRSFailed
      "pending_auto_renewal" -> pure LBTCRSPendingAutoRenewal
      "pending_validation" -> pure LBTCRSPendingValidation
      "success" -> pure LBTCRSSuccess
      e ->
        fromTextError $
          "Failure parsing LoadBalancerTLSCertificateRenewalStatus from value: '" <> e
            <> "'. Accepted values: failed, pending_auto_renewal, pending_validation, success"

instance ToText LoadBalancerTLSCertificateRenewalStatus where
  toText = \case
    LBTCRSFailed -> "FAILED"
    LBTCRSPendingAutoRenewal -> "PENDING_AUTO_RENEWAL"
    LBTCRSPendingValidation -> "PENDING_VALIDATION"
    LBTCRSSuccess -> "SUCCESS"

instance Hashable LoadBalancerTLSCertificateRenewalStatus

instance NFData LoadBalancerTLSCertificateRenewalStatus

instance ToByteString LoadBalancerTLSCertificateRenewalStatus

instance ToQuery LoadBalancerTLSCertificateRenewalStatus

instance ToHeader LoadBalancerTLSCertificateRenewalStatus

instance FromJSON LoadBalancerTLSCertificateRenewalStatus where
  parseJSON = parseJSONText "LoadBalancerTLSCertificateRenewalStatus"
