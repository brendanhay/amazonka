{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ClientVPNAuthenticationType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ClientVPNAuthenticationType where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data ClientVPNAuthenticationType
  = CertificateAuthentication
  | DirectoryServiceAuthentication
  | FederatedAuthentication
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

instance FromText ClientVPNAuthenticationType where
  parser =
    takeLowerText >>= \case
      "certificate-authentication" -> pure CertificateAuthentication
      "directory-service-authentication" -> pure DirectoryServiceAuthentication
      "federated-authentication" -> pure FederatedAuthentication
      e ->
        fromTextError $
          "Failure parsing ClientVPNAuthenticationType from value: '" <> e
            <> "'. Accepted values: certificate-authentication, directory-service-authentication, federated-authentication"

instance ToText ClientVPNAuthenticationType where
  toText = \case
    CertificateAuthentication -> "certificate-authentication"
    DirectoryServiceAuthentication -> "directory-service-authentication"
    FederatedAuthentication -> "federated-authentication"

instance Hashable ClientVPNAuthenticationType

instance NFData ClientVPNAuthenticationType

instance ToByteString ClientVPNAuthenticationType

instance ToQuery ClientVPNAuthenticationType

instance ToHeader ClientVPNAuthenticationType

instance FromXML ClientVPNAuthenticationType where
  parseXML = parseXMLText "ClientVPNAuthenticationType"
