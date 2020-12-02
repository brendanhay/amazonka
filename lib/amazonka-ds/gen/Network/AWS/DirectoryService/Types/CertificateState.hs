{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.Types.CertificateState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectoryService.Types.CertificateState where

import Network.AWS.Prelude

data CertificateState
  = DeregisterFailed
  | Deregistered
  | Deregistering
  | RegisterFailed
  | Registered
  | Registering
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

instance FromText CertificateState where
  parser =
    takeLowerText >>= \case
      "deregisterfailed" -> pure DeregisterFailed
      "deregistered" -> pure Deregistered
      "deregistering" -> pure Deregistering
      "registerfailed" -> pure RegisterFailed
      "registered" -> pure Registered
      "registering" -> pure Registering
      e ->
        fromTextError $
          "Failure parsing CertificateState from value: '" <> e
            <> "'. Accepted values: deregisterfailed, deregistered, deregistering, registerfailed, registered, registering"

instance ToText CertificateState where
  toText = \case
    DeregisterFailed -> "DeregisterFailed"
    Deregistered -> "Deregistered"
    Deregistering -> "Deregistering"
    RegisterFailed -> "RegisterFailed"
    Registered -> "Registered"
    Registering -> "Registering"

instance Hashable CertificateState

instance NFData CertificateState

instance ToByteString CertificateState

instance ToQuery CertificateState

instance ToHeader CertificateState

instance FromJSON CertificateState where
  parseJSON = parseJSONText "CertificateState"
