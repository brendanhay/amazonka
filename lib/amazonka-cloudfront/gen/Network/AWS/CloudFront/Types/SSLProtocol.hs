{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.SSLProtocol
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.SSLProtocol where

import Network.AWS.Prelude

data SSLProtocol
  = SSLV3
  | TLSV1
  | TLSV1_1
  | TLSV1_2
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

instance FromText SSLProtocol where
  parser =
    takeLowerText >>= \case
      "sslv3" -> pure SSLV3
      "tlsv1" -> pure TLSV1
      "tlsv1.1" -> pure TLSV1_1
      "tlsv1.2" -> pure TLSV1_2
      e ->
        fromTextError $
          "Failure parsing SSLProtocol from value: '" <> e
            <> "'. Accepted values: sslv3, tlsv1, tlsv1.1, tlsv1.2"

instance ToText SSLProtocol where
  toText = \case
    SSLV3 -> "SSLv3"
    TLSV1 -> "TLSv1"
    TLSV1_1 -> "TLSv1.1"
    TLSV1_2 -> "TLSv1.2"

instance Hashable SSLProtocol

instance NFData SSLProtocol

instance ToByteString SSLProtocol

instance ToQuery SSLProtocol

instance ToHeader SSLProtocol

instance FromXML SSLProtocol where
  parseXML = parseXMLText "SSLProtocol"

instance ToXML SSLProtocol where
  toXML = toXMLText
