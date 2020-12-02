{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.AuthMechanismValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.AuthMechanismValue where

import Network.AWS.Prelude

data AuthMechanismValue
  = AMVDefault
  | AMVMongodbCr
  | AMVScramSha1
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

instance FromText AuthMechanismValue where
  parser =
    takeLowerText >>= \case
      "default" -> pure AMVDefault
      "mongodb_cr" -> pure AMVMongodbCr
      "scram_sha_1" -> pure AMVScramSha1
      e ->
        fromTextError $
          "Failure parsing AuthMechanismValue from value: '" <> e
            <> "'. Accepted values: default, mongodb_cr, scram_sha_1"

instance ToText AuthMechanismValue where
  toText = \case
    AMVDefault -> "default"
    AMVMongodbCr -> "mongodb_cr"
    AMVScramSha1 -> "scram_sha_1"

instance Hashable AuthMechanismValue

instance NFData AuthMechanismValue

instance ToByteString AuthMechanismValue

instance ToQuery AuthMechanismValue

instance ToHeader AuthMechanismValue

instance ToJSON AuthMechanismValue where
  toJSON = toJSONText

instance FromJSON AuthMechanismValue where
  parseJSON = parseJSONText "AuthMechanismValue"
