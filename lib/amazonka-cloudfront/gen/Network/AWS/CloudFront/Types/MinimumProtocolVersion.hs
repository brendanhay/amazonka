{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.MinimumProtocolVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.MinimumProtocolVersion where

import Network.AWS.Prelude

data MinimumProtocolVersion
  = MPVSSLV3
  | MPVTLSV1
  | MPVTLSV12016
  | MPVTLSV1_12016
  | MPVTLSV1_22018
  | MPVTLSV1_22019
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

instance FromText MinimumProtocolVersion where
  parser =
    takeLowerText >>= \case
      "sslv3" -> pure MPVSSLV3
      "tlsv1" -> pure MPVTLSV1
      "tlsv1_2016" -> pure MPVTLSV12016
      "tlsv1.1_2016" -> pure MPVTLSV1_12016
      "tlsv1.2_2018" -> pure MPVTLSV1_22018
      "tlsv1.2_2019" -> pure MPVTLSV1_22019
      e ->
        fromTextError $
          "Failure parsing MinimumProtocolVersion from value: '" <> e
            <> "'. Accepted values: sslv3, tlsv1, tlsv1_2016, tlsv1.1_2016, tlsv1.2_2018, tlsv1.2_2019"

instance ToText MinimumProtocolVersion where
  toText = \case
    MPVSSLV3 -> "SSLv3"
    MPVTLSV1 -> "TLSv1"
    MPVTLSV12016 -> "TLSv1_2016"
    MPVTLSV1_12016 -> "TLSv1.1_2016"
    MPVTLSV1_22018 -> "TLSv1.2_2018"
    MPVTLSV1_22019 -> "TLSv1.2_2019"

instance Hashable MinimumProtocolVersion

instance NFData MinimumProtocolVersion

instance ToByteString MinimumProtocolVersion

instance ToQuery MinimumProtocolVersion

instance ToHeader MinimumProtocolVersion

instance FromXML MinimumProtocolVersion where
  parseXML = parseXMLText "MinimumProtocolVersion"

instance ToXML MinimumProtocolVersion where
  toXML = toXMLText
