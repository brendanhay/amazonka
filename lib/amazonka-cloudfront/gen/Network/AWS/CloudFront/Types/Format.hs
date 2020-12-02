{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.Format
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.Format where

import Network.AWS.Prelude

data Format = URLEncoded
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

instance FromText Format where
  parser =
    takeLowerText >>= \case
      "urlencoded" -> pure URLEncoded
      e ->
        fromTextError $
          "Failure parsing Format from value: '" <> e
            <> "'. Accepted values: urlencoded"

instance ToText Format where
  toText = \case
    URLEncoded -> "URLEncoded"

instance Hashable Format

instance NFData Format

instance ToByteString Format

instance ToQuery Format

instance ToHeader Format

instance FromXML Format where
  parseXML = parseXMLText "Format"

instance ToXML Format where
  toXML = toXMLText
