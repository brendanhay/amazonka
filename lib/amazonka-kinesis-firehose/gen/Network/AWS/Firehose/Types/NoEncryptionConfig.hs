{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.NoEncryptionConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.NoEncryptionConfig where

import Network.AWS.Prelude

data NoEncryptionConfig = NoEncryption
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

instance FromText NoEncryptionConfig where
  parser =
    takeLowerText >>= \case
      "noencryption" -> pure NoEncryption
      e ->
        fromTextError $
          "Failure parsing NoEncryptionConfig from value: '" <> e
            <> "'. Accepted values: noencryption"

instance ToText NoEncryptionConfig where
  toText = \case
    NoEncryption -> "NoEncryption"

instance Hashable NoEncryptionConfig

instance NFData NoEncryptionConfig

instance ToByteString NoEncryptionConfig

instance ToQuery NoEncryptionConfig

instance ToHeader NoEncryptionConfig

instance ToJSON NoEncryptionConfig where
  toJSON = toJSONText

instance FromJSON NoEncryptionConfig where
  parseJSON = parseJSONText "NoEncryptionConfig"
