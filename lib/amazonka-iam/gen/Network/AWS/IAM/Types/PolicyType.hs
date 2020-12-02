{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.PolicyType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.PolicyType where

import Network.AWS.Prelude

data PolicyType
  = Inline
  | Managed
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

instance FromText PolicyType where
  parser =
    takeLowerText >>= \case
      "inline" -> pure Inline
      "managed" -> pure Managed
      e ->
        fromTextError $
          "Failure parsing PolicyType from value: '" <> e
            <> "'. Accepted values: inline, managed"

instance ToText PolicyType where
  toText = \case
    Inline -> "INLINE"
    Managed -> "MANAGED"

instance Hashable PolicyType

instance NFData PolicyType

instance ToByteString PolicyType

instance ToQuery PolicyType

instance ToHeader PolicyType

instance FromXML PolicyType where
  parseXML = parseXMLText "PolicyType"
