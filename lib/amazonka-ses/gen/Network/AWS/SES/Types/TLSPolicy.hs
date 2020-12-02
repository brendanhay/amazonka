{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types.TLSPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SES.Types.TLSPolicy where

import Network.AWS.Prelude

data TLSPolicy
  = Optional
  | Require
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

instance FromText TLSPolicy where
  parser =
    takeLowerText >>= \case
      "optional" -> pure Optional
      "require" -> pure Require
      e ->
        fromTextError $
          "Failure parsing TLSPolicy from value: '" <> e
            <> "'. Accepted values: optional, require"

instance ToText TLSPolicy where
  toText = \case
    Optional -> "Optional"
    Require -> "Require"

instance Hashable TLSPolicy

instance NFData TLSPolicy

instance ToByteString TLSPolicy

instance ToQuery TLSPolicy

instance ToHeader TLSPolicy

instance FromXML TLSPolicy where
  parseXML = parseXMLText "TLSPolicy"
