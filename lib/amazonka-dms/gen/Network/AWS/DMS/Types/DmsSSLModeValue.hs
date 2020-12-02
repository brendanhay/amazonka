{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.DmsSSLModeValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.DmsSSLModeValue where

import Network.AWS.Prelude

data DmsSSLModeValue
  = DSMVNone
  | DSMVRequire
  | DSMVVerifyCa
  | DSMVVerifyFull
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

instance FromText DmsSSLModeValue where
  parser =
    takeLowerText >>= \case
      "none" -> pure DSMVNone
      "require" -> pure DSMVRequire
      "verify-ca" -> pure DSMVVerifyCa
      "verify-full" -> pure DSMVVerifyFull
      e ->
        fromTextError $
          "Failure parsing DmsSSLModeValue from value: '" <> e
            <> "'. Accepted values: none, require, verify-ca, verify-full"

instance ToText DmsSSLModeValue where
  toText = \case
    DSMVNone -> "none"
    DSMVRequire -> "require"
    DSMVVerifyCa -> "verify-ca"
    DSMVVerifyFull -> "verify-full"

instance Hashable DmsSSLModeValue

instance NFData DmsSSLModeValue

instance ToByteString DmsSSLModeValue

instance ToQuery DmsSSLModeValue

instance ToHeader DmsSSLModeValue

instance ToJSON DmsSSLModeValue where
  toJSON = toJSONText

instance FromJSON DmsSSLModeValue where
  parseJSON = parseJSONText "DmsSSLModeValue"
