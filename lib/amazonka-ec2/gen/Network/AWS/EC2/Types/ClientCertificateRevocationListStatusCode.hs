{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ClientCertificateRevocationListStatusCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ClientCertificateRevocationListStatusCode where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data ClientCertificateRevocationListStatusCode
  = CCRLSCActive
  | CCRLSCPending
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

instance FromText ClientCertificateRevocationListStatusCode where
  parser =
    takeLowerText >>= \case
      "active" -> pure CCRLSCActive
      "pending" -> pure CCRLSCPending
      e ->
        fromTextError $
          "Failure parsing ClientCertificateRevocationListStatusCode from value: '" <> e
            <> "'. Accepted values: active, pending"

instance ToText ClientCertificateRevocationListStatusCode where
  toText = \case
    CCRLSCActive -> "active"
    CCRLSCPending -> "pending"

instance Hashable ClientCertificateRevocationListStatusCode

instance NFData ClientCertificateRevocationListStatusCode

instance ToByteString ClientCertificateRevocationListStatusCode

instance ToQuery ClientCertificateRevocationListStatusCode

instance ToHeader ClientCertificateRevocationListStatusCode

instance FromXML ClientCertificateRevocationListStatusCode where
  parseXML = parseXMLText "ClientCertificateRevocationListStatusCode"
