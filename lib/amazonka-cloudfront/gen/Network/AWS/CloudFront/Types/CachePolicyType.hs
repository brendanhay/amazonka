{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.CachePolicyType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.CachePolicyType where

import Network.AWS.Prelude

data CachePolicyType
  = CPTCustom
  | CPTManaged
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

instance FromText CachePolicyType where
  parser =
    takeLowerText >>= \case
      "custom" -> pure CPTCustom
      "managed" -> pure CPTManaged
      e ->
        fromTextError $
          "Failure parsing CachePolicyType from value: '" <> e
            <> "'. Accepted values: custom, managed"

instance ToText CachePolicyType where
  toText = \case
    CPTCustom -> "custom"
    CPTManaged -> "managed"

instance Hashable CachePolicyType

instance NFData CachePolicyType

instance ToByteString CachePolicyType

instance ToQuery CachePolicyType

instance ToHeader CachePolicyType

instance FromXML CachePolicyType where
  parseXML = parseXMLText "CachePolicyType"

instance ToXML CachePolicyType where
  toXML = toXMLText
