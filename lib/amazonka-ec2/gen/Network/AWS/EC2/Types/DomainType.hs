{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.DomainType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.DomainType where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data DomainType
  = DTStandard
  | DTVPC
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

instance FromText DomainType where
  parser =
    takeLowerText >>= \case
      "standard" -> pure DTStandard
      "vpc" -> pure DTVPC
      e ->
        fromTextError $
          "Failure parsing DomainType from value: '" <> e
            <> "'. Accepted values: standard, vpc"

instance ToText DomainType where
  toText = \case
    DTStandard -> "standard"
    DTVPC -> "vpc"

instance Hashable DomainType

instance NFData DomainType

instance ToByteString DomainType

instance ToQuery DomainType

instance ToHeader DomainType

instance FromXML DomainType where
  parseXML = parseXMLText "DomainType"
